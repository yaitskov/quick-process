{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Description : Automate some of the ways to make labels.

-}

module Data.HList.MakeLabels (
    makeLabels,
    makeLabels3,

    -- * labels using kind 'Symbol'
    makeLabels6,
    makeLabelable,
    ) where

import Data.Typeable
import Data.HList.FakePrelude
import Data.HList.Label3
import Data.HList.Labelable

import Language.Haskell.TH
import Data.Char
import Control.Monad

make_cname, make_dname :: String -> Name
make_cname (x:xs) = mkName ("Label" ++ toUpper x : xs)
make_cname _ = error "Data.HList.MakeLabels.make_cname: empty string"

make_dname (x:xs) = mkName (toLower x : xs)
make_dname _ = error "Data.HList.MakeLabels.make_dname: empty string"

dcl :: String -> Q [Dec]
dcl n = let
    c = make_cname n
    d = make_dname n

    dd =
#if MIN_VERSION_template_haskell(2,12,0)
      dataD (return []) c [] Nothing [] [derivClause Nothing [ [t| Typeable |] ]]
#elif MIN_VERSION_template_haskell(2,11,0)
      dataD (return []) c [] Nothing [] (fmap (:[]) [t| Typeable |])
#else
      dataD (return []) c [] [] [''Typeable]
#endif

    labelSig = sigD d [t| Label $(conT c) |]

    labelDec = valD
                  (varP d)
                  (normalB [| Label |])
                  []

    showInst = instanceD
            (return [])
            [t| Show $(conT c) |]
            [valD (varP 'show)
                (normalB [| \_ -> n |])
                [] ]

 in sequence [
        labelSig,
        labelDec,

        dd,

        showInst ]


{- |

Labels like "Data.HList.Label5".

 The following TH declaration splice should be placed at top-level, before the
 created values are used. Enable @-XTemplateHaskell@ too.

>  makeLabels ["getX","getY","draw","X"]

should expand into the following declarations

> data LabelGetX deriving Typeable
> data LabelGetY deriving Typeable
> data LabelDraw deriving Typeable
> data LabelX deriving Typeable

> getX = Label :: Label LabelGetX
> getY = Label :: Label LabelGetY
> draw = Label :: Label LabelDraw
> x    = Label :: Label LabelX

-}
makeLabels :: [String] -> Q [Dec]
makeLabels = fmap concat . mapM dcl


-- | for "Data.HList.Label3"
makeLabels3 :: String -- ^ namespace
    -> [String] -- ^ labels
    -> Q [Dec]
makeLabels3 ns (k:ks) =
    let pt1 = fmap (concatMap (drop 2)) $ mapM dcl (ns : k : ks)

        sq1 = valD (varP (make_dname k))
                (normalB [| firstLabel (undefined :: $(conT (make_cname ns)))
                                       (undefined :: $(conT (make_cname k))) |])
                []

        sqs = [ valD (varP (make_dname k2))
                (normalB [| nextLabel $(varE (make_dname k1))
                                    (undefined :: $(conT (make_cname k2))) |])
                []

                | (k1,k2) <- zip (k:ks) ks ]

    in fmap concat $ sequence [ pt1, sequence (sq1 : sqs) ]
-- possibly there is a better option
makeLabels3 ns [] = fail ("makeLabels3 "++ ns ++ " []")

{- | for "Data.HList.Label6"

> makeLabels6 ["x","y"]

is a shortcut for

> x = Label :: Label "x"
> y = Label :: Label "y"

-}
makeLabels6 :: [String] -> Q [Dec]
makeLabels6 ns = fmap concat $ forM ns $ \n -> sequence
  [sigD (make_dname n) [t| Label $(litT (strTyLit n)) |],
   valD (varP (make_dname n)) (normalB [| Label |]) []]


{- | @makeLabelable \"x y z\"@ expands out to

> x = hLens' (Label :: Label "x")
> y = hLens' (Label :: Label "y")
> z = hLens' (Label :: Label "z")

Refer to "Data.HList.Labelable" for usage.

-}
makeLabelable :: String -> Q [Dec]
makeLabelable xs = fmap concat $ mapM makeLabel1 (words xs)
    where
        -- a bit indirect, ghc-7.6 TH is a bit too eager to reject
        -- mis-matched kind variables
        makeLabel1 x = sequence
              [
                sigD (mkName x) makeSig,
                valD (varP (mkName x)) (normalB (varE 'hLens' `appE` lt))
                            []
                ]
            where lt = [| Label :: $([t| Label $l |]) |]
                  l = litT (strTyLit x)

                  makeSig = [t| forall r s t a b. (Labelable $l r s t a b) =>
                              LabeledOptic $l r s t a b
                              |]
