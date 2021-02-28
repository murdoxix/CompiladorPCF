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

import Common ( Pos( NoPos ) )
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

-- | Valores de la pila de la máquina BVM.
data PVal =
    I Int
  | Fun BVMEnv Bytecode
  | RA BVMEnv Bytecode

type Pila = [PVal]
type BVMEnv = Pila

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
pattern SUM      = 15
pattern SUB      = 16

bc :: MonadPCF m => Term -> m Bytecode
bc (V _ (Bound i)) = return [ACCESS, i]

bc (V _ (Free v)) = failPCF $ "Error de compilación. Está la variable " ++ v ++ " libre. Los términos deberían ser cerrados."

bc (Const _ (CNat n)) = return [CONST, n]

bc (Lam _ _ _ t) = bc t >>= (\ct -> return ([FUNCTION, length ct + 1]++ct++[RETURN]))

bc (App _ f e) = do cf <- bc f
                    ce <- bc e
                    return (cf++ce++[CALL])

bc (UnaryOp _ Succ e) = bc e >>= (\ce -> return (ce++[SUCC]))

bc (UnaryOp _ Pred e) = bc e >>= (\ce -> return (ce++[PRED]))

bc (BinaryOp _ Sum t1 t2) = do ct1 <- bc t1
                               ct2 <- bc t2 
                               return (ct1++ct2++[SUM])

bc (BinaryOp _ Sub t1 t2) = do ct1 <- bc t1
                               ct2 <- bc t2 
                               return (ct1++ct2++[SUB])

bc (Fix _ _ _ _ _ e) = bc e >>= (\ce -> return ([FUNCTION, length ce + 1]++ce++[RETURN, FIX]))

bc (IfZ _ e t1 t2) = do ce <- bc e
                        ct1 <- bc t1
                        ct2 <- bc t2
                        return (ce++[IFZ,length ct1 + 2]++ct1++[JUMP,length ct2]++ct2)

bc (Let _ _ _ e1 e2) = do ce1 <- bc e1
                          ce2 <- bc e2
                          return (ce1++[SHIFT]++ce2++[DROP])

bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule mod = translate mod >>= bc >>= (\cmod -> return $ cmod ++ [PRINT, STOP])
  where
    translate [] = failPCF "Error de compilación de módulo. Debe haber al menos una declaración."
    translate ((Decl{declPos = p, declName = v, declType = ty, declBody = e}):[]) =
      if ty == NatTy
        then return e
        else failPCF "Error de compilación de módulo. La última declaración debe ser de tipo Nat."
    translate ((Decl{declPos = p, declName = v, declType = ty, declBody = e}):xs) =
      translate xs >>= (\txs -> return (Let p v ty e (close v txs)))

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
runBC c = runBVM c [] []

runBVM :: MonadPCF m => Bytecode -> BVMEnv -> Pila -> m ()
runBVM (RETURN:_) _ (v:(RA e c):s) = runBVM c e (v:s)

runBVM (CONST:n:c) e s = runBVM c e ((I n):s)

runBVM (ACCESS:i:c) e s = runBVM c e (e!!i:s)

runBVM (FUNCTION:l:cfYc) e s = runBVM (drop l cfYc) e ((Fun e cfYc):s)

runBVM (CALL:c) e (v:(Fun ef cf):s) = runBVM cf (v:ef) ((RA e c):s)

runBVM (SUCC:c) e ((I n):s) = runBVM c e ((I (n+1)):s)

runBVM (PRED:c) e ((I n):s) = runBVM c e ((I $ max 0 (n-1)):s)

runBVM (SUM:c) e ((I n2):(I n1):s) = runBVM c e ((I (n1+n2)):s)

runBVM (SUB:c) e ((I n2):(I n1):s) = runBVM c e ((I (n1-n2)):s)

runBVM (IFZ:lct1:c) e ((I ve):s) = if ve == 0
                                     then runBVM c e s
                                     else runBVM (drop lct1 c) e s

runBVM (FIX:c) e ((Fun ce cf):s) = let efix = (Fun efix cf) : ce
                                   in runBVM c e ((Fun efix cf):s)

runBVM (STOP:_) _ _ = return ()

runBVM (JUMP:n:c) e s = runBVM (drop n c) e s

runBVM (SHIFT:c) e (v:s) = runBVM c (v:e) s

runBVM (DROP:c) (v:e) s = runBVM c e s

runBVM (PRINT:c) e ((I n):s) = (printPCF $ show n) >> runBVM c e ((I n):s)
