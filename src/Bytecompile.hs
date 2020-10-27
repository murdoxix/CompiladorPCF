{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Bytecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, bytecompileModule, runBC, bcWrite, bcRead)
 where

import Lang 
import Subst
import MonadPCF

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where go =
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern SUCC     = 6
pattern PRED     = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14

bc :: MonadPCF m => Term -> m Bytecode
bc (V _ (Bound i)) = return [ACCESS, i]

bc (V _ (Free v)) = error $ "Error de compilación. Está la variable " ++ v ++ " libre. Los términos deberían ser cerrados"

bc (Const _ (CNat n)) = return [CONST, n]

bc (Lam _ _ _ t) = bc t >>= (\ct -> return ([FUNCTION, length ct + 1]++ct++[RETURN]))

bc (App _ f e) = do cf <- bc f
                    ce <- bc e
                    return (cf++ce++[RETURN])

bc (UnaryOp _ Succ e) = bc e >>= (\ce -> return (ce++[SUCC]))

bc (UnaryOp _ Pred e) = bc e >>= (\ce -> return (ce++[PRED]))

bc (Fix _ _ _ _ _ e) = bc e >>= (\ce -> return ([FUNCTION, length ce + 1]++ce++[RETURN, FIX]))

bc (IfZ _ e t1 t2) = error "implementame"


-- ~ data Tm info var =
    -- ~ V info var
  -- ~ | Const info Const
  -- ~ | Lam info Name Ty (Tm info var)
  -- ~ | App info (Tm info var) (Tm info var)
  -- ~ | UnaryOp info UnaryOp (Tm info var)
  -- ~ | Fix info Name Ty Name Ty (Tm info var)
  -- ~ | IfZ info (Tm info var) (Tm info var) (Tm info var)
  -- ~ deriving (Show, Functor)
  
-- ~ data Var =
    -- ~ Bound !Int
  -- ~ | Free Name
  -- ~ deriving Show

-- ~ bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule mod = error "implementame"

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadPCF m => Bytecode -> m ()
runBC c = error "implementame"

-- ~ runBVM :: 
