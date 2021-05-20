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

type CanonMonad = StateT (Int, Loc, [Inst]) (Writer Blocks)

-- ~ type CanonFun = (String, [String], Blocks)
-- ~ type CanonVal = String -- Sólo el nombre, tipo puntero siempre
-- ~ newtype CanonProg = CanonProg [Either CanonFun CanonVal]

runCanon :: IrDecls -> CanonProg
runCanon decls = CanonProg $ canon_funs ++ canon_vals ++ [pcfmain]
    where isfun = \d -> case d of {IrFun{} -> True; _ -> False}
          funs = filter isfun decls
          vals = filter (not . isfun) decls
          start_state = (0, "", [])

          makeFuns = foldr f (start_state, []) funs
              where f (IrFun funName _ funArgs funBody) (state, canonfuns) =
                        let monad = do startBlock funName
                                       v <- irToCanon funBody
                                       finishBlock $ Return v
                            (state', newblocks) = runWriter $ execStateT monad state
                        in (state', (Left (funName, funArgs, newblocks)):canonfuns)
          canon_funs = snd makeFuns

          canon_vals = map (Right . irDeclName) vals

          pcfmain = let pcfmainblocks = execWriter $ runStateT (makepcfmain vals) $ fst makeFuns
                    in Left ("pcfmain", [], pcfmainblocks)

makepcfmain :: IrDecls -> CanonMonad ()
makepcfmain vals = do startBlock "pcfmain"
                      v <- f vals
                      finishBlock $ Return v
    where f ((IrVal valName valDef):xs) = do
                      val <- irToCanon valDef
                      addInst $ Store valName (V val)
                      if null xs then return val else f xs

irToCanon :: Ir -> CanonMonad Val
irToCanon (IrVar v) = undefined


-- ~ data Ir =
    -- ~ IrVar Name
  -- ~ | IrCall Ir [Ir]
  -- ~ | IrConst Const
  -- ~ | IrBinaryOp BinaryOp Ir Ir
  -- ~ | IrLet Name Ir Ir
  -- ~ | IrIfZ Ir Ir Ir
  -- ~ | MkClosure Name [Ir]
  -- ~ | IrAccess Ir Int
  -- ~ deriving Show

startBlock :: Loc -> CanonMonad ()
startBlock loc = modify $ \(n,_,_) -> (n,loc,[])

finishBlock :: Terminator -> CanonMonad ()
finishBlock term = do (n,l,i) <- get
                      tell [(l, reverse i, term)]

addInst :: Inst -> CanonMonad ()
addInst inst = modify $ \(n,l,i) -> (n,l,inst:i)
