{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

import Control.Monad.Trans
import Data.List ( nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Data.Maybe ( fromJust, isJust )
import Control.Exception ( catch , IOException )
import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )
import Options.Applicative

import Global ( GlEnv(..) )
import Errors
import Lang hiding ( Print )
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab, elab_decl, desugar )
import Bytecompile ( bytecompileModule, bcWrite, runBC, bcRead )
import Eval ( eval )
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import ClosureConversion ( runCC )

prompt :: String
prompt = "PCF> "

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
        <> progDesc "Compilador de PCF"
        <> header "Compilador de PCF de la materia Compiladores 2020" )

    go :: (Mode,[FilePath]) -> IO ()
    go (Interactive,files) = void $ runPCF $ catchErrors $ runInputT defaultSettings (repl files)
    go (Typecheck, files) = void $ runPCF $ catchErrors $ verifyFiles files
    go (Bytecompile, files) = void $ runPCF $ catchErrors $
      ( do modul <- verifyMod files
           bytecode <- bytecompileModule modul
           let lastFile = last files
               fileByte = ((++ "byte") . take (length lastFile - 3)) lastFile
           liftIO $ bcWrite bytecode fileByte )
    go (Run, files) = void $ runPCF $ catchErrors
      ( do asBytecodes <- liftIO $ mapM bcRead files
           mapM_ runBC asBytecodes )
    go (ClosureConversion, files) = void $ runPCF $ catchErrors $
      ( do modul <- verifyMod files
           printPCF "Resultado de CC:"
           mapM_ (printPCF . show) (runCC modul) )
    go (LLVM, files) = undefined

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConversion
          | LLVM

-- | Parser de banderas
parseMode :: Parser Mode
parseMode =
      flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos")
  <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
  <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM")
  <|> flag' ClosureConversion (long "cc" <> help "Imprime el resultado de hacer conversión de clausuras y hoisting")
  <|> flag' LLVM (long "llvm" <> short 'l' <> help "Compila el código a  LLVM y lo ejecuta")
  <|> flag Interactive Interactive ( long "interactive" <> short 'i'<> help "Ejecutar en forma interactiva" )

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))

repl :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
repl args = do
        lift $ compileFiles args
        s <- lift $ get
        when (inter s) $ liftIO $ putStrLn
          (  "Entorno interactivo para PCF0.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (flip when loop) b

-- Toma una lista de archivos con código fuente y devuelve un único Module.
-- Chequea que haya al menos un archivo y que la última declaración sea
-- de tipo Nat.
verifyMod :: MonadPCF m => [String] -> m Module
verifyMod []    = failPCF "Error de compilación de módulo. Debe haber al menos una declaración."
verifyMod files = do mods <- verifyFiles files
                     let modul = foldl (++) [] mods
                         isNatTheLast = (declType (last modul)) == NatTy
                     if isNatTheLast
                       then return modul
                       else failPCF "Error de compilación de módulo. La última declaración debe ser de tipo Nat."

verifyFiles :: MonadPCF m => [String] -> m [Module]
verifyFiles [] = return []
verifyFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        verified <- verifyFile x
        restVerified <- verifyFiles xs
        return (verified : restVerified)

compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

parseFile :: MonadPCF m => String -> m [SDecl STerm]
parseFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    printPCF ("Abriendo "++filename++"...")
    when ((drop (length f -4) filename) /= ".pcf") $ failPCF $ "El archivo "++filename++" no tiene extensión .pcf"
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    parseIO filename program x

compileFile :: MonadPCF m => String -> m ()
compileFile f = parseFile f >>= mapM_ handleDecl

verifyFile :: MonadPCF m => String -> m Module
verifyFile f = parseFile f >>= mapM verifyDecl >>= return.(map fromJust).(filter isJust)

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

verifyDecl ::  MonadPCF m => SDecl STerm -> m (Maybe (Decl Term))
verifyDecl d = do
        mde <- elab_decl d
        case mde of
          Nothing -> return Nothing
          Just de@(Decl p x ty t) ->
            do tcDecl de
               return $ Just de

handleDecl ::  MonadPCF m => SDecl STerm -> m ()
handleDecl d = do
        mde <- elab_decl d
        case mde of
          Nothing -> return ()
          Just de@(Decl p x ty t) ->
            do tcDecl de
               te <- eval t
               addDecl (Decl p x ty te)
               return ()

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of
      Left d  -> handleDecl d
      Right t -> handleTerm t

handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         te <- eval tt
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    nx <- desugar x'
    ex <- elab x'
    t  <- case x' of
           (SV p f) -> maybe ex id <$> lookupDecl f
           _        -> return ex
    printPCF "STerm:"
    printPCF (show x')
    printPCF "\nNTerm:"
    printPCF (show nx)
    printPCF "\nTerm:"
    printPCF (show t)

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)
