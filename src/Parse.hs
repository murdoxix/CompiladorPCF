{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP)
import Data.Char ( isNumber, ord, isUpper )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else",
                          "succ", "pred", "ifz", "Nat", "in",
                          "rec", "type"],
         reservedOpNames = ["->",":","="]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyvar :: P Name
tyvar = Tok.lexeme lexer $ do
  n <- identifier
  if isUpper $ head n
    then return n
    else unexpected "Los sinónimos de tipos deben empezar con mayuscula"

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> flip STyName <$> tyvar <*> getPos
         <|> parens typeP

typeP :: P STy
typeP = try (do
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (SFunTy x y))
      <|> tyatom

const :: P Const
const = CNat <$> num

unaryOpName :: P UnaryOp
unaryOpName =
      (reserved "succ" >> return Succ)
  <|> (reserved "pred" >> return Pred)

unaryOp :: P STerm
unaryOp = do
  i <- getPos
  o <- unaryOpName
  return (SUnaryOp i o)

atom :: P STerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> unaryOp
       <|> parens tm

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         args <- binders
         reservedOp "->"
         t <- tm
         return (SLam i args t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         e <- tm
         return (SIfZ i c t e)

binding :: P (Name, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v,ty)

multibinding :: P [(Name, STy)]
multibinding = do vs <- many1 var
                  reservedOp ":"
                  ty <- typeP
                  return (map (\v -> (v,ty)) vs)

-- | Parser de lista de binders
binders :: P [(Name, STy)]
binders = concat <$> (many $ parens multibinding)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         (x, xty) <- parens binding
         reservedOp "->"
         t <- tm
         return (SFix i f fty x xty t)

lett :: P STerm
lett = do i <- getPos
          reserved "let"
          v <- var
          args <- binders
          reservedOp ":"
          ty <- typeP
          reservedOp "="
          t <- tm
          reserved "in"
          tt <- tm
          return (SLet i v args ty t tt)

letRec :: P STerm
letRec = do i <- getPos
            reserved "let"
            reserved "rec"
            f <- var
            args <- binders
            reservedOp ":"
            ty <- typeP
            reservedOp "="
            t <- tm
            reserved "in"
            tt <- tm
            return (SRec i f args ty t tt)

-- | Parser de términos
tm :: P STerm
tm = app <|> lam <|> ifz <|> fix <|> try lett <|> letRec

tySynon :: P (SDecl STerm)
tySynon = do i <- getPos
             reserved "type"
             n <- tyvar
             reservedOp "="
             ty <- typeP
             return (SSynonymDecl i n ty)

letd :: P (SDecl STerm)
letd = do i <- getPos
          reserved "let"
          v <- var
          args <- binders
          reservedOp ":"
          ty <- typeP
          reservedOp "="
          t <- tm
          return (SDecl i v args ty t)

letRecd :: P (SDecl STerm)
letRecd = do i <- getPos
             reserved "let"
             reserved "rec"
             f <- var
             args <- binders
             reservedOp ":"
             ty <- typeP
             reservedOp "="
             t <- tm
             return (SDeclRec i f args ty t)

-- | Parser de declaraciones
decl :: P (SDecl STerm)
decl = tySynon <|> try letd <|> letRecd

-- | Parser de programas (listas de declaraciones) 
program :: P [SDecl STerm]
program = many decl

-- | Parsea una declaración o un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (SDecl STerm) STerm)
declOrTm = try (Right <$> tm) <|> (Left <$> decl)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP tm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
