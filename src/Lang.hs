{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )

-- | AST de Tipos superficiales
data SugaredTy info =
      SNatTy 
    | STyName info Name
    | SFunTy (SugaredTy info) (SugaredTy info)
    deriving (Show,Eq)

type STy = SugaredTy Pos

-- | AST de Tipos
data Ty =
      NatTy 
    | NamedTy Name Ty
    | FunTy Ty Ty
    deriving Show

instance Eq Ty where
  NatTy == NatTy = True
  (NamedTy _ ty) == (NamedTy _ ty') = ty == ty'
  (NamedTy _ ty) == ty' = ty == ty'
  ty == (NamedTy _ ty') = ty == ty'
  (FunTy ty1 ty1') == (FunTy ty2 ty2') = ty1 == ty2 && ty1' == ty2'
  _ == _ = False

type Name = String

data Const = CNat Int
  deriving Show

data UnaryOp = Succ | Pred
  deriving (Show,Eq)

-- | tipo de datos de declaraciones azucaradas, parametrizado por el tipo del cuerpo de la declaración
data SDecl a =
    SDecl { sdeclPos :: Pos, sdeclName :: Name, sdeclArgs :: [(Name, STy)], sdeclType :: STy, sdeclBody :: a }
  | SDeclRec { sdeclPos :: Pos, sdeclName :: Name, sdeclArgs :: [(Name, STy)], sdeclRtn :: STy, sdeclBody :: a }
  | SSynonymDecl { sdeclPos :: Pos, sdeclName :: Name, sdeclType :: STy }
  deriving (Show,Functor)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a =
    Decl { declPos :: Pos, declName :: Name, declType :: Ty, declBody :: a }
  deriving (Show,Functor)

-- | AST superficial de los términos. Incluye azucar sintáctico.
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data STm info var =
    SV info var
  | SConst info Const
  | SLet info Name [(Name, STy)] STy (STm info var) (STm info var)
  | SRec info Name [(Name, STy)] STy (STm info var) (STm info var)
  | SLam info [(Name, STy)] (STm info var)
  | SApp info (STm info var) (STm info var)
  | SUnaryOp info UnaryOp
  | SFix info Name STy Name STy (STm info var)
  | SIfZ info (STm info var) (STm info var) (STm info var)
  deriving (Show, Functor)

type STerm = STm Pos Name   -- ^ 'STm' tiene 'Name's como variables ligadas y libres, guarda posición

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var =
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | UnaryOp info UnaryOp (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var) (Tm info var)
  deriving (Show, Functor)

type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición
type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición

type Module = [Decl Term]

data Var =
    Bound !Int
  | Free Name
  deriving Show

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _)           = i
getInfo (Const i _)       = i
getInfo (Lam i _ _ _)     = i
getInfo (App i _ _ )      = i
getInfo (UnaryOp i _ _)   = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _)     = i
getInfo (Let i _ _ _ _)   = i

-- | Obtiene las variables libres de un término LC.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))    = [v]
freeVars (V _ _)           = []
freeVars (Lam _ _ _ t)     = freeVars t
freeVars (App _ l r)       = freeVars l ++ freeVars r
freeVars (UnaryOp _ _ t)   = freeVars t
freeVars (Fix _ _ _ _ _ t) = freeVars t
freeVars (IfZ _ c t e)     = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)       = []
freeVars (Let _ _ _ e1 e2) = freeVars e1 ++ freeVars e2

-- | Clausura. El término incluye a Lam o Fix.
data Clos = Clos Env Term
  deriving Show

-- | Valores del lenguaje.
data Val =
    VConst Const
  | VClos Clos
  deriving Show

type Env = [Val]

-- | Marcos o frames.
data Frame =
    KArg Env Term
  | KClos Clos
  | KIfZ Env Term Term
  | KUnaryOp UnaryOp
  deriving Show

type Kont = [Frame]
