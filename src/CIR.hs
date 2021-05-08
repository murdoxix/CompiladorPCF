module CIR where

import Lang hiding ( V, Val )
import Data.List ( intercalate )
import Control.Monad.Writer
import Control.Monad.State

newtype Reg = Temp String
  deriving Show

data Val = R Reg | C Int | G Name
  deriving Show

type Loc = String

data Inst =
    Assign Reg Expr
  | Store Name Expr
  deriving Show

data Expr =
    BinOp BinaryOp Val Val
  | Phi [(Loc, Val)]
  | Call Val [Val]
  | MkClosure Loc [Val]
  | V Val
  | Access Val Int
  | Print Val
  deriving Show

data Terminator =
    Jump Loc
  | CondJump Cond Loc Loc
  | Return Val
  deriving Show

data Cond =
    Eq Val Val
  deriving Show

type BasicBlock = (Loc, [Inst], Terminator)
type Blocks = [BasicBlock]

type CanonFun = (String, [String], Blocks)
type CanonVal = String -- Sólo el nombre, tipo puntero siempre
newtype CanonProg = CanonProg [Either CanonFun CanonVal]

print :: (Blocks, [Inst], Val) -> String
print (bs, is, v) =
  concatMap printBlock bs ++ show is ++ "\n" ++ show v ++ "\n"

printBlock :: BasicBlock -> String
printBlock (loc, is, t) =
  loc ++ ":\n" ++
  concatMap (\i -> "  " ++ show i ++ "\n") is ++
  show t ++ "\n"

instance Show CanonProg where
  show (CanonProg prog) = concatMap pr1 prog where
    pr1 (Left (f, args, blocks)) =
      f ++ "(" ++ intercalate ", " args ++ ") {\n"
        ++ concatMap printBlock blocks ++ "}\n\n"

    pr1 (Right v) =
      "declare " ++ v ++ "\n\n"

type CanonMonad = StateT (Int, Loc, [Inst]) (Writer Blocks) ()

-- ~ type CanonFun = (String, [String], Blocks)
-- ~ type CanonVal = String -- Sólo el nombre, tipo puntero siempre
-- ~ newtype CanonProg = CanonProg [Either CanonFun CanonVal]

runCanon :: IrDecls -> CanonProg
runCanon decls = CanonProg canon_funs ++ canon_vals ++ [pcfmain]
    where isfun (IrFun{}) = True
          isfun _         = False
          isval (IrVal{}) = True
          isval _         = False
          canon_funs = runCanon' $ filter isfun decls
          canon_vals = filter isval decls
          pcfmain = Left $ makepcfmain canon_vals 

-- ~ data IrDecl =
    -- ~ IrVal { irDeclName :: Name,
            -- ~ irDeclDef  :: Ir }
  -- ~ | IrFun { irDeclName     :: Name,
            -- ~ irDeclArity    :: Int,
            -- ~ irDeclArgNames :: [Name],
            -- ~ irDeclBody     :: Ir }
  -- ~ deriving Show
