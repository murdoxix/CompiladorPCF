{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl, desugar ) where

import Lang
import Subst
import MonadPCF ( MonadPCF, failPosPCF, addTy, lookupTy )
import Common ( Pos (NoPos) )

desugar_type :: MonadPCF m => STy -> m Ty
desugar_type SNatTy =
  return NatTy

desugar_type (STyName p n) = 
  do mty <- lookupTy n
     maybe
       (failPosPCF p $ "DesugarType: el sinónimo "++n++" no está declarado")
       (\ty  -> return (NamedTy n ty))
       mty

desugar_type (SFunTy ty ty') =
  do tyd  <- desugar_type ty
     ty'd <- desugar_type ty'
     return (FunTy tyd ty'd)

desugar :: MonadPCF m => STerm -> m NTerm      
desugar (SV p v) =
  return (V p v)

desugar (SConst p c) =
  return (Const p c)

desugar (SLet p x [] ty t t') = 
  do tyd <- desugar_type ty
     td  <- desugar t
     t'd <- desugar t'
     return (Let p x tyd td t'd)

desugar (SLet p f [(x, ty)] ty' t t') =
  desugar $ SLet p f [] (SFunTy ty ty') (SLam p [(x, ty)] t) t'

desugar (SLet p f l ty t t') =
  desugar $ SLet p f [] (helper l ty) (SLam p l t) t'

desugar (SRec p _ [] _ _ _) =
  failPosPCF p $ "DesugarTérmino: let rec debe tener al menos un argumento"

desugar (SRec p f [(x, ty)] ty' t t') =
  desugar $ SLet p f [] (SFunTy ty ty') (SFix p f (SFunTy ty ty') x ty t) t'

desugar (SRec p f ((x1, ty1):l) ty t t') =
  desugar $ SRec p f [(x1, ty1)] (helper l ty) (SLam p l t) t'

desugar (SLam p [] _) =
  failPosPCF p $ "DesugarTérmino: fun debe tener al menos un argumento"

desugar (SLam p [(x, ty)] t) =
  do tyd <- desugar_type ty
     td  <- desugar t
     return (Lam p x tyd td)

desugar (SLam p ((x1, ty1):l) t) =
  do ty1d <- desugar_type ty1
     t'   <- desugar $ SLam p l t
     return (Lam p x1 ty1d t')

desugar (SApp p (SUnaryOp p' Succ) t) =
  do td <- desugar t
     return (BinaryOp p Sum td (Const NoPos (CNat 1)))

desugar (SApp p (SUnaryOp p' Pred) t) =
  do td <- desugar t
     return (BinaryOp p Sub td (Const NoPos (CNat 1)))

desugar (SApp p (SUnaryOp p' UPrint) t) =
  do td <- desugar t
     return (Print p td)

desugar (SApp p (SApp _ t (SBinaryOp _ b)) t') =
  do td  <- desugar t
     t'd <- desugar t'
     return (BinaryOp p b td t'd)

desugar (SApp p (SBinaryOp _ b) t) =
  do td  <- desugar t
     return (Lam p "x" NatTy (BinaryOp p b (V NoPos "x") td))

desugar (SApp p t (SBinaryOp _ b)) =
  do td  <- desugar t
     return (Lam p "x" NatTy (BinaryOp p b td (V NoPos "x")))

desugar (SApp p t t') =
  do td  <- desugar t
     t'd <- desugar t'
     return (App p td t'd)

desugar (SUnaryOp p Succ) =
  return (Lam p "x" NatTy (BinaryOp p Sum (V NoPos "x") (Const NoPos (CNat 1))))

desugar (SUnaryOp p Pred) =
  return (Lam p "x" NatTy (BinaryOp p Sub (V NoPos "x") (Const NoPos (CNat 1))))

desugar (SUnaryOp p UPrint) =
  return (Lam p "x" NatTy (Print p (V NoPos "x")))

desugar (SBinaryOp p o) =
  return (Lam p "x" NatTy (Lam p "y" NatTy (BinaryOp p o (V NoPos "x") (V NoPos "y"))))

desugar (SFix p f fty x xty t) =
  do ftyd <- desugar_type fty
     xtyd <- desugar_type xty
     td   <- desugar t
     return (Fix p f ftyd x xtyd td)

desugar (SIfZ p c t e) =
  do cd <- desugar c
     td <- desugar t
     ed <- desugar e
     return (IfZ p cd td ed)

-- | helper recibe [(x1,t1),...,(xn,tn)] t y devuelve t1->...->tn->t
helper :: [(Name, STy)] -> STy -> STy
helper [] ty = ty
helper ((_,ty):ls) ty' = SFunTy ty $ helper ls ty'

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadPCF m => STerm -> m Term
elab stm = do desugared <- desugar stm
              return $ elab' desugared
  where
    elab' :: NTerm -> Term
    elab' (V p v)               = V p (Free v)
    elab' (Const p c)           = Const p c
    elab' (Lam p v ty t)        = Lam p v ty (close v (elab' t))
    elab' (App p h a)           = App p (elab' h) (elab' a)
    elab' (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' t))
    elab' (IfZ p c t e)         = IfZ p (elab' c) (elab' t) (elab' e)
    elab' (BinaryOp p o h a)    = BinaryOp p o (elab' h) (elab' a)
    elab' (Let p v ty t e)      = Let p v ty (elab' t) (close v (elab' e))
    elab' (Print p h)           = Print p (elab' h)

desugar_decl :: MonadPCF m => SDecl STerm -> m (Maybe (Decl STerm))
desugar_decl (SSynonymDecl p n ty) =
  do tyd <- desugar_type ty
     addTy n tyd
     return Nothing

desugar_decl d = do dd <- desugar_decl' d
                    return (Just dd)

desugar_decl' :: MonadPCF m => SDecl STerm -> m (Decl STerm)
desugar_decl' (SDecl p x [] ty t) =
  do tyd <- desugar_type ty
     return (Decl p x tyd t)

desugar_decl' (SDecl p f [(x, ty)] ty' t) =
  do tyd  <- desugar_type ty
     ty'd <- desugar_type ty'
     return (Decl p f (FunTy tyd ty'd) (SLam p [(x, ty)] t))

desugar_decl' (SDecl p f l ty t) =
  do helped <- desugar_type $ helper l ty
     return (Decl p f helped (SLam p l t))

desugar_decl' (SDeclRec p _ [] _ _) =
  failPosPCF p $ "DesugarDeclaración: let rec debe tener al menos un argumento"

desugar_decl' (SDeclRec p f [(x, ty)] ty' t) =
  do tyd  <- desugar_type ty
     ty'd <- desugar_type ty'
     return (Decl p f (FunTy tyd ty'd) (SFix p f (SFunTy ty ty') x ty t))

desugar_decl' (SDeclRec p f ((x1, ty1):l) ty t) =
  desugar_decl' $ SDeclRec p f [(x1, ty1)] (helper l ty) (SLam p l t)

elab_decl :: MonadPCF m => SDecl STerm -> m (Maybe (Decl Term))
elab_decl d = do mdd <- desugar_decl d
                 maybe
                   (return Nothing)
                   (\dd -> do t <- elab $ declBody dd
                              return (Just (dd {declBody = t})))
                   mdd
