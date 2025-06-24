{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts,
  FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records: labels are phantom, so at run-time, the record
   is just a heterogenous list of field values.

   This sort of record is generalizable to `tables' (which are, at
   run-time, a list or a map containing the heterogenous lists
   of field values).

   The are different models of labels that go with this module;
   see the files Label?.hs.
-}

module Data.HList.RecordP where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.HArray

-- --------------------------------------------------------------------------
-- Record types as Phantom labels with values

newtype RecordP (ls::[k]) vs = RecordP (HList vs)


-- Build a record. I wonder if the 'ls' argument of mkRecordP can be
-- removed. So far, we had no need for it...

mkRecordP :: (SameLength ls vs, HLabelSet ls) => proxy ls -> HList vs -> RecordP ls vs
mkRecordP _ vs = RecordP vs

-- Build an empty record
emptyRecordP :: RecordP ('[]) ('[])
emptyRecordP = mkRecordP Proxy HNil

-- Converting between RecordP and Record (label/value pairs)

-- The following class declares a bijection between Record and recordP
class HRLabelSet r => RecordR2P (r::[*]) (ls::[*]) (vs::[*]) | r -> ls vs, ls vs -> r where
    record_r2p :: Record r -> RecordP ls vs
    record_p2r :: RecordP ls vs -> Record r

instance RecordR2P ('[]) ('[]) ('[]) where
    record_r2p _ = emptyRecordP
    record_p2r _ = emptyRecord

instance (RecordR2P r ls vs, HRLabelSet (Tagged l v ': r),
          HLabelSet (l ': ls), SameLength ls vs)
    => RecordR2P (Tagged l v ': r) (l ': ls) (v ': vs) where
    record_r2p (Record (HCons f r)) = f  .*. record_r2p (Record r)
    record_p2r (RecordP (HCons v r)) = Tagged v .*. record_p2r (RecordP r)

labels_of_recordp :: RecordP ls vs -> Proxy ls
labels_of_recordp _ = Proxy

-- --------------------------------------------------------------------------
-- A Show instance to appeal to normal records
-- to save the coding time (rather than run-time), we just
-- convert RecordP to regular Record, which we know how to show

instance (RecordR2P r ls vs, ShowComponents r, HRLabelSet r) =>
    Show (RecordP ls vs) where show rp = show $ record_p2r rp


-- --------------------------------------------------------------------------
-- Extension for records

instance (HLabelSet (l ': ls), SameLength ls vs)
    => HExtend (Tagged l v) (RecordP ls vs)
 where
  type HExtendR (Tagged l v) (RecordP ls vs) = RecordP (l ': ls) (v ': vs)
  Tagged v .*. RecordP vs = mkRecordP Proxy (HCons v vs)



-- --------------------------------------------------------------------------
-- Record concatenation

instance ( HLabelSet ls''
         , HAppendR (RecordP ls vs) (RecordP ls' vs') ~ RecordP ls'' vs''
         , HAppendList ls ls' ~ ls''
         , HAppendList vs vs' ~ vs''
         , SameLength ls'' vs''
         )
    => HAppend (RecordP ls vs) (RecordP ls' vs')
 where
  hAppend (RecordP vs) (RecordP vs') = mkRecordP Proxy (hAppend vs vs')

type instance HAppendR (RecordP ls vs) (RecordP ls' vs') = RecordP (HAppendR ls ls') (HAppendR vs vs')

-- --------------------------------------------------------------------------
-- Lookup operation

-- Because hLookupByLabel is so frequent and important, we
-- implement it separately. The algorithm is familiar assq,
-- only the comparison operation is done at compile-time

instance (HEq l l' b, HasFieldP' b l (l' ': ls) vs v)
    => HasField l (RecordP (l' ': ls) vs) v where
    hLookupByLabel = hLookupByLabelP' (Proxy :: Proxy b)

class HasFieldP' b l ls vs v | b l ls vs -> v where
    hLookupByLabelP' :: Proxy b -> Label l -> RecordP ls vs -> v

instance HasFieldP' True l (l ': ls) (v ': vs) v where
    hLookupByLabelP' _ _ (RecordP (HCons v _)) = v

instance HasField l (RecordP ls vs) v
    => HasFieldP' False l (l' ': ls) (v' ': vs) v where
    hLookupByLabelP' _ l (RecordP (HCons _ vs)) =
        hLookupByLabel l ((RecordP vs)::RecordP ls vs)


{-
-- --------------------------------------------------------------------------
-- Delete operation
hDeleteAtLabelP :: HProjectByLabelP l ls vs lso v vso =>
                   l -> RecordP ls vs -> RecordP lso vso
hDeleteAtLabelP l r = snd $ h2ProjectByLabelP l r


-- --------------------------------------------------------------------------
-- Update operation
hUpdateAtLabelP :: (HUpdateAtHNat n e1 t1 l', HFind e t n) => 
		   e -> e1 -> RecordP t t1 -> RecordP ls l'
hUpdateAtLabelP l v rp@(RecordP vs) = RecordP (hUpdateAtHNat n v vs)
 where
  n       = hFind l (labels_of_recordp rp)
-}

-- --------------------------------------------------------------------------
-- Projection for records
-- It is also an important operation: the basis of many
-- deconstructors -- so we try to implement it efficiently.

-- Project by a single label
class HProjectByLabelP l ls vs lso v vso | l ls vs -> lso v vso where
    h2ProjectByLabelP :: Label l -> RecordP ls vs -> (v,RecordP lso vso)

instance (HEq l l' b, HProjectByLabelP' b l (l' ': ls) vs lso v vso)
    => HProjectByLabelP l (l' ': ls) vs lso v vso where
    h2ProjectByLabelP = h2ProjectByLabelP' (Proxy :: Proxy b)

class HProjectByLabelP' b l ls vs lso v vso | b l ls vs -> lso v vso where
    h2ProjectByLabelP' :: Proxy b -> Label l -> RecordP ls vs -> (v,RecordP lso vso)

instance HProjectByLabelP' True l (l ': ls) (v ': vs) ls v vs where
    h2ProjectByLabelP' _ _ (RecordP (HCons v vs)) = (v,RecordP vs)

instance (HProjectByLabelP l ls vs lso' v vso')
    => HProjectByLabelP' False l (l' ': ls) (v' ': vs)
       (l' ': lso') v (v' ': vso') where
    h2ProjectByLabelP' _ l (RecordP (HCons v' vs)) =
        let (v,RecordP vso) = h2ProjectByLabelP l ((RecordP vs)::RecordP ls vs)
        in (v, RecordP (HCons v' vso))


-- Invariant: r = rin `disjoint-union` rout
--            labels(rin) = ls
-- classes H2ProjectByLabels and H2ProjectByLabels' are declared in
-- Record.hs

{- need to change H2ProjectByLabels kind variables back to * from [*]
instance H2ProjectByLabels (l ': ls)
                           (RecordP '[] '[]) (RecordP '[] '[])
                           (RecordP '[] '[])
    where
    h2projectByLabels _ _ = (emptyRecordP,emptyRecordP)

instance (HMember l' ls b,
          H2ProjectByLabels' b ls (RecordP (l' ': ls') vs') rin rout)
    => H2ProjectByLabels ls (RecordP (l' ': ls') vs') rin rout where
    h2projectByLabels = h2projectByLabels' (Proxy :: Proxy b)

instance H2ProjectByLabels ls (RecordP ls' vs') (RecordP lin vin) rout =>
    H2ProjectByLabels' True ls (RecordP (l' ': ls') (v' ': vs'))
                             (RecordP (l' ': lin) (v' ': vin)) rout where
    h2projectByLabels' _ ls (RecordP (HCons v' vs')) =
        (RecordP (HCons v' vin), rout)
        where (RecordP vin,rout) =
                  h2projectByLabels ls ((RecordP vs')::RecordP ls' vs')

instance H2ProjectByLabels ls (RecordP ls' vs') rin (RecordP lo vo) =>
    H2ProjectByLabels' False ls (RecordP (l' ': ls') (v' ': vs'))
                              rin (RecordP (l' ': lo) (v' ': vo)) where
    h2projectByLabels' _ ls (RecordP (HCons v' vs')) =
        (rin, RecordP (HCons v' vo))
        where (rin,RecordP vo) =
                  h2projectByLabels ls ((RecordP vs')::RecordP ls' vs')
-}

{-
-- --------------------------------------------------------------------------
-- Subtyping for records

-- Hmm, a bit too conservative. It works for all our examples,
-- where the record extension is by simple extension. In the future,
-- we should account for possible field permutation.

instance H2ProjectByLabels ls' (RecordP ls vs) (RecordP ls' vs') rout
    =>  SubType (RecordP ls vs) (RecordP ls' vs')
    -}
