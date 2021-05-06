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

runCanon :: IrDecls -> CanonProg
runCanon decls = let terminatorspcfmain = tail $ map extractName $ filter vals decls
                     runWriterTMonad = runWriterT $ mapM canon decls
                     initialState = (0, terminatorspcfmain)
                     (canonProgWOpcfmain, pcfmainBlocks) = evalState runWriterTMonad initialState
                     pcfmain = Left ("pcfmain", [], pcfmainBlocks)
                     canonProg = pcfmain : canonProgWOpcfmain
                 in CanonProg canonProg
  where
    vals IrVal{} = True
    vals _       = False
    extractName IrVal{ irDeclName = n } = n

-- canon compila IrDecl a CanonVal o CanonFun según corresponda, ese es
-- el valor de retorno. La lista de bloques básicos (Blocks) que lleva
-- la mónada WriterT son las instrucciones para inicializar las IrVal por pcfmain.
-- El Int que lleva como estado la mónada State es para generar nombres
-- frescos para los registros. Y la lista de nombres ([Name]) que también lleva
-- el estado es la lista de los nombres de IrVals, para armar los terminadores
-- de pcfmain.
canon :: IrDecl -> WriterT Blocks (State (Int, [Name])) (Either CanonFun CanonVal)
canon ( IrVal { irDeclName = n, irDeclDef = ir} ) =
  do id <- lift getid
     term <- lift getterm
     let (pcfblocks, newid) = blocks ir n (Just term) id
     tell pcfblocks
     lift $ putid newid
     return (Right n)

canon ( IrFun { irDeclName = n, irDeclArity = 2, irDeclArgNames = args, irDeclBody = ir} ) =
  do id <- lift getid
     let (funblocks, newid) = blocks ir n Nothing id
     lift $ putid newid
     return (Left (n, args, funblocks))


-- ~ type CanonFun = (String, [String], [BasicBlock])

-- blocks genera los bloques básicos (Blocks) dada una expresión del
-- lenguaje intermedio Ir, su nombre, Maybe Name que es Just n si la
-- expresión es IrVal y Nothing si es un IrFun (esto para, en el primer caso,
-- agregar una instrucción Store al final con el valor de la expresión y
-- armar el terminador del bloque, que va a ser parte de pcfmain) y
-- un Int que es para generar nombres de registros frescos, desde ese
-- valor hacia arriba y devuelve el próximo nombre fresco disponible.
blocks :: Ir -> Name -> (Maybe Name) -> Int -> (Blocks, Int)
-- ~ blocks :: Ir -> Name -> (Maybe Name) -> WriterT Blocks (State Int) [Inst]
blocks (IrVar m) n mn id =
  let 
                    

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

-- ~ newtype Reg = Temp String
  -- ~ deriving Show

-- ~ data Val = R Reg | C Int | G Name
  -- ~ deriving Show

-- ~ type Loc = String

-- ~ data Inst =
    -- ~ Assign Reg Expr
  -- ~ | Store Name Expr
  -- ~ deriving Show

-- ~ data Expr =
    -- ~ BinOp BinaryOp Val Val
  -- ~ | Phi [(Loc, Val)]
  -- ~ | Call Val [Val]
  -- ~ | MkClosure Loc [Val]
  -- ~ | V Val
  -- ~ | Access Val Int
  -- ~ | Print Val
  -- ~ deriving Show

-- ~ data Terminator =
    -- ~ Jump Loc
  -- ~ | CondJump Cond Loc Loc
  -- ~ | Return Val
  -- ~ deriving Show

-- ~ data Cond =
    -- ~ Eq Val Val
  -- ~ deriving Show

-- ~ type BasicBlock = (Loc, [Inst], Terminator)

getid :: State (Int, [Name]) Int
getid = do (id, _) <- get
           return id

putid :: Int -> State (Int, [Name]) ()
putid id = do (_, terms) <- get
              put (id, terms)

getterm :: State (Int, [Name]) Name
getterm = do (id, terms) <- get
             put (id, tail terms)
             return (head terms)
