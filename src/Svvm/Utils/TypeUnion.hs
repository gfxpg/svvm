{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Svvm.Utils.TypeUnion
  ( IsMember,
    Ty,
    ty,
    tyEq,
    tyInt,
    TypeUnion,
    (@>),
    unionExhausted,
    TypeUnionOps (..)
  )
where

import Data.Kind (Constraint, Type)
import Data.Typeable
import GHC.TypeLits

-- Proxies

data Ty t = Typeable t => Ty (Proxy t)

deriving instance Eq (Ty t)

instance Show (Ty t) where
  show (Ty p) = "'" <> show (typeRep p)

ty :: Typeable t => Ty t
ty = Ty (Proxy :: Proxy t)

tyEq :: Ty t1 -> Ty t2 -> Bool
tyEq (Ty t1) (Ty t2) = typeRep t1 == typeRep t2

tyInt :: KnownNat n => Ty n -> Int
tyInt (Ty p) = fromIntegral $ natVal p

-- Membership constraints

type IsMember t ts = (MemberCheck t ts ts, Typeable t)

type family MemberCheck (t :: Type) (ts :: [Type]) (ets :: [Type]) :: Constraint where
  MemberCheck t '[] ets = TypeError (Text "Type " :<>: ShowType t :<>: Text " does not belong to the set " :<>: ShowType ets)
  MemberCheck t (t ': ts) ets = ()
  MemberCheck t (u ': ts) ets = MemberCheck t ts ets

-- Type union

data TypeUnion (ts :: [Type]) where -- encode current type as an index into the list of types
  UnionElem :: t -> TypeUnion (t ': ts) -- comparable to Zero
  UnionNext :: TypeUnion ts -> TypeUnion (t ': ts) -- comparable to Succ

(@>) :: (t -> a) -> (TypeUnion ts -> a) -> TypeUnion (t ': ts) -> a
(r @> l) u = case u of
  UnionElem e -> r e -- union contains type t
  UnionNext n -> l n -- union contains another type

infixr 2 @>

unionExhausted :: TypeUnion '[] -> a
unionExhausted u = case u of

class TypeUnionOps (t :: Type) (ts :: [Type]) where
  liftU :: t -> TypeUnion ts
  extractU :: TypeUnion ts -> Maybe t

instance {-# OVERLAPPING #-} TypeUnionOps t (t ': ts) where
  liftU = UnionElem
  extractU (UnionElem e) = Just e
  extractU (UnionNext n) = Nothing

instance TypeUnionOps t ts => TypeUnionOps t (u ': ts) where
  liftU = UnionNext . liftU
  extractU (UnionElem e) = Nothing
  extractU (UnionNext n) = extractU n

instance Show (TypeUnion '[]) where
  show u = case u of

instance (Show t, Show (TypeUnion ts)) => Show (TypeUnion (t ': ts)) where
  show (UnionNext n) = show n
  show (UnionElem e) = let estr = show e in if ' ' `elem` estr then "(" <> estr <> ")" else estr

instance Eq (TypeUnion '[]) where
  u1 == u2 = False

instance (Eq t, Eq (TypeUnion ts)) => Eq (TypeUnion (t ': ts)) where
  (UnionElem e1) == (UnionElem e2) = e1 == e2
  (UnionNext u1) == (UnionNext u2) = u1 == u2
  _ == _ = False
