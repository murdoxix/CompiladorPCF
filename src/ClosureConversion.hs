{-|
Module      : ClosureConversion
Description : Implementa el algoritmo Closure Conversion.
Copyright   : (c) Alejandro Rivosecchi, 2021.
License     : GPL-3
Maintainer  : alerivo228@gmail.com
Stability   : experimental

Este módulo implementa la Closure Conversion; convierte a
las funciones de alto orden a objetos concretos, clausuras,
que las representan. Al terminar la conversíon, el programa
ya no presenta funciones de alto orden y todas las
funciones son top-level.
-}

module ClosureConversion ( runCC ) where

import Lang
import Subst ( openN, open )
import Control.Monad.State
import Control.Monad.Writer
import Data.List ( isPrefixOf )

produceName :: String -> CCState Name
produceName s = do id <- get
                   put (id+1)
                   return $ "__" ++ s ++ (show id)

type CCState a = StateT Int (Writer [IrDecl]) a

closureConvert :: Term -> CCState Ir
closureConvert (V _ (Free v)) = return (IrVar v)

closureConvert (V _ (Bound n)) = error "ClosureConversion: no deberia haber variables ligadas acá"

closureConvert (Const _ n) = return (IrConst n)

closureConvert (Print _ t) = do irt <- closureConvert t
                                return (IrPrint irt)

closureConvert (BinaryOp _ b t e) = do irt <- closureConvert t
                                       ire <- closureConvert e
                                       return (IrBinaryOp b irt ire)

closureConvert (IfZ _ c t e) = do irc <- closureConvert c
                                  irt <- closureConvert t
                                  ire <- closureConvert e
                                  return (IrIfZ irc irt ire)

closureConvert (Let _ v _ t e) = do irt <- closureConvert t
                                    v' <- produceName v
                                    ire <- closureConvert (open v' e)
                                    return (IrLet v irt ire)

closureConvert (App _ f x) = do irf <- closureConvert f
                                irx <- closureConvert x
                                return (IrCall (IrAccess irf 0) [irf, irx])

closureConvert (Lam _ x _ e) = closureConvertFun [x] e

closureConvert (Fix _ n _ x _ e) = closureConvertFun [n,x] e

-- closureConvertFun abstrae las similaridades de CC de las funciones,
-- es decir, las Lam y Fix. Si vars tiene un elemento es el caso Lam,
-- si tiene dos elementos es un Fix. No hay otro caso.
closureConvertFun :: [Name] -> Term -> CCState Ir
closureConvertFun vars e = do irfunName <- produceName ""
                              vars' <- mapM produceName vars
                              let opene = openN vars' e
                              ire <- closureConvert opene
                              cloName <- produceName "clo"
                              let freeVarse = freeNestedVars e
                                  indexedFreeVars =
                                    case length vars of
                                      1 -> zip3 freeVarse [1..] (cycle [cloName])
                                      2 -> zip3 (freeVarse++[vars'!!0]) [1..] (cycle [cloName])
                                  irfunArgNames = [cloName, last vars']
                                  irfunBody = foldl aux ire indexedFreeVars
                                  irfun = IrFun { irDeclName = irfunName,
                                                  irDeclArity = 2,
                                                  irDeclArgNames = irfunArgNames,
                                                  irDeclBody =  irfunBody}
                                  clo = MkClosure irfunName (map IrVar freeVarse)
                              tell [irfun]
                              return clo
  where aux e (x, n, cloName) = IrLet x (IrAccess (IrVar cloName) n) e

-- freeNestedVars devuelve las variables libres en un término generadas
-- por nosotros, es decir, las que tienen __ como prefijo.
freeNestedVars :: Term -> [Name]
freeNestedVars t = filter ("__" `isPrefixOf`) (freeVars t)

runCC :: Module -> IrDecls
runCC mod = execWriter $ evalStateT (mapM_ runCC' mod) 0
  where runCC' decl = do declcc <- closureConvert (declBody decl)
                         tell [IrVal (declName decl) declcc]
