{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Svvm.Storage where

import Control.Applicative
import Data.Kind (Constraint, Type)
import Data.Maybe (fromJust)
import Data.Type.Bool
import Data.Type.Equality
import Data.Typeable
import GHC.TypeLits
import Svvm.Utils.TypeUnion

-- Data types

data I32

data U32

data F32

data I64

data U64

type StorableTypes = '[ '(I32, 1), '(U32, 1), '(F32, 1), '(I64, 2), '(U64, 2)]

type IsStorable t n = (KnownNat n, Typeable t, StorageCheck t n StorableTypes StorableTypes)

type IsInteger t = IsMember t '[I32, U32, I64, U64]

type IsFloatingPoint t = IsMember t '[F32]

-- Registers

data RegLocation = RegLocVirt | RegLocPhys
  deriving (Eq, Show)

type SReg t = SRegs t 1

data SRegs t n = IsStorable t n => SRegs (Ty t) (Ty n) Int RegLocation

deriving instance Eq (SRegs t n)

type VReg t = VRegs t 1

data VRegs t n = IsStorable t n => VRegs (Ty t) (Ty n) Int RegLocation

deriving instance Eq (VRegs t n)

instance IsStorable t n => Show (SRegs t n) where
  show (SRegs t n i RegLocVirt) = "(" <> show t <> ")%s[" <> show i <> (if tyInt n > 1 then ":" <> show (i + tyInt n - 1) else "") <> "]"
  show (SRegs t n i RegLocPhys) = "(" <> show t <> ")s[" <> show i <> (if tyInt n > 1 then ":" <> show (i + tyInt n - 1) else "") <> "]"

instance KnownNat n => Show (VRegs t n) where
  show (VRegs t n i RegLocVirt) = "(" <> show t <> ")%v[" <> show i <> (if tyInt n > 1 then ":" <> show (i + tyInt n - 1) else "") <> "]"
  show (VRegs t n i RegLocPhys) = "(" <> show t <> ")v[" <> show i <> (if tyInt n > 1 then ":" <> show (i + tyInt n - 1) else "") <> "]"

class RegEltSize r where
  eltSizeDw :: r -> Int
  dwordCount :: r -> Int

instance RegEltSize (SRegs t n) where
  eltSizeDw (SRegs t _ _ _) = fromJust $ lookupEltSize (Proxy :: Proxy t) (Proxy :: Proxy StorableTypes)
  dwordCount (SRegs _ n _ _) = tyInt n

instance RegEltSize (VRegs t n) where
  eltSizeDw (VRegs t _ _ _) = fromJust $ lookupEltSize (Proxy :: Proxy t) (Proxy :: Proxy StorableTypes)
  dwordCount (VRegs _ n _ _) = tyInt n

-- Internal implementation

type family StorageCheck (t :: Type) (n :: Nat) (ts :: [(Type, Nat)]) (ets :: [(Type, Nat)]) :: Constraint where
  StorageCheck t n '[] ets =
    TypeError (Text "Type " :<>: ShowType t :<>: Text " is not a member of storable types: " :<>: PrintType ets)
  StorageCheck t n ('(t, sz) ': ts) ets =
    If
      (Mod n sz == 0)
      (() :: Constraint)
      ( TypeError
          ( Text "Elements of type " :<>: ShowType t
              :<>: Text " cannot be stored in "
              :<>: ShowType n
              :<>: Text " dwords"
              :<>: Text " (multiple of "
              :<>: ShowType sz
              :<>: Text " expected)"
          )
      )
  StorageCheck t n ('(u, sz) ': ts) ets = StorageCheck t n ts ets

type family PrintType (ts :: [(Type, Nat)]) :: ErrorMessage where
  PrintType '[] = Text ""
  PrintType ('(t, _) ': '[]) = ShowType t
  PrintType ('(t, _) ': ts) = ShowType t :<>: Text ", " :<>: PrintType ts

-- Workaround for overlapping instances: both list head and list tail are passed as type a
class Typeable a => LookupEltSize a where
  lookupEltSize :: Typeable t => Proxy t -> Proxy a -> Maybe Int

instance (Typeable t, KnownNat n) => LookupEltSize ('(t, n) :: (Type, Nat)) where
  lookupEltSize (_ :: Proxy t0) (_ :: Proxy '(t, n))
    | typeRep (Proxy :: Proxy t0) == typeRep (Proxy :: Proxy t) = Just $ fromIntegral $ natVal (Proxy :: Proxy n)
    | otherwise = Nothing

instance Typeable a => LookupEltSize ('[] :: [a]) where
  lookupEltSize _ _ = Nothing

instance (LookupEltSize x, LookupEltSize xs, Typeable (x ': xs)) => LookupEltSize (x ': xs) where
  lookupEltSize t _ = lookupEltSize t (Proxy :: Proxy x) <|> lookupEltSize t (Proxy :: Proxy xs)
