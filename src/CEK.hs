{-|
Module      : CEK
Description : Implementa la máquina CEK.
Copyright   : (c) Alejandro Rivosecchi, 2020.
License     : GPL-3
Maintainer  : alerivo228@gmail.com
Stability   : experimental

Este módulo implementa la máquina CEK y la traducción de valores en términos.
-}

module CEK where

import Common ( Pos( NoPos ) )
import Lang
import Subst ( substN )
import MonadPCF ( MonadPCF, lookupDecl, failPCF )

-- Ejecuta la máquina CEK sobre un término dado
exec :: MonadPCF m => Term -> m Term
exec t = (search t [] []) >>= valToTerm

-- Fase de búsqueda
search  :: MonadPCF m => Term -> Env -> Kont -> m Val
search (UnaryOp _ o t) p k =
  search t p (KUnaryOp o : k)

search (IfZ _ c t e) p k =
  search c p (KIfZ p t e : k)

search (App _ t u) p k =
  search t p (KArg p u : k)

search (V _ (Bound x)) p k =
  destroy (p!!x) k

search (V _ (Free x)) p k =
  do l <- lookupDecl x
     case l of
       (Just (Const _ n)) -> destroy (VConst n) k
       (Just lamOrFix)    -> destroy (VClos $ Clos [] lamOrFix) k
       Nothing            -> failPCF "Variable libre no declarada. No debería pasar."

search (Const _ n) p k =
  destroy (VConst n) k

search (Let _ _ _ t1 t2) p k =
  search t1 p ((KLet p t2):k)

search lamOrFix p k =
  destroy (VClos $ Clos p lamOrFix) k

-- Fase de reducción
destroy :: MonadPCF m => Val  -> Kont -> m Val
destroy z@(VConst (CNat 0)) (KUnaryOp Pred : k) =
  destroy z k

destroy (VConst (CNat np)) (KUnaryOp Pred : k) =
  destroy (VConst (CNat $ np - 1)) k

destroy (VConst (CNat n)) (KUnaryOp Succ : k) =
  destroy (VConst (CNat $ n + 1)) k

destroy (VConst (CNat 0)) (KIfZ p t e : k) =
  search t p k

destroy (VConst (CNat _)) (KIfZ p t e : k) =
  search e p k

destroy (VClos clos) (KArg p t : k) =
  search t p (KClos clos : k)

destroy v (KLet p t : k) =
  search t (v : p) k

destroy v (KClos (Clos p (Lam _ _ _ t)) : k) =
  search t (v : p) k

destroy v (KClos clos@(Clos p (Fix _ _ _ _ _ t)) : k) =
  search t (v : VClos clos : p) k

destroy v [] = return v

destroy _ _ = failPCF "Error en tiempo de ejecución. No hay ninguna regla de reducción para aplicar"

valToTerm :: MonadPCF m => Val -> m Term
valToTerm (VConst n) = return (Const NoPos n)

valToTerm (VClos (Clos p t)) =
  do pAsTerms <- mapM valToTerm p
     return $ substN pAsTerms t
