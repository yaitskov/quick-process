{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{- | Description : quasiquoter inspired by -XNamedFieldPuns -}
module Data.HList.RecordPuns (
    -- $ex
    pun

    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.HList.Record
import Data.HList.FakePrelude
import Data.List
import Data.HList.HList

{- $ex

>>> :set -XQuasiQuotes -XViewPatterns

[@patterns@]

>>> let y = Label :: Label "y"
>>> let x = Label :: Label "x"
>>> [pun| x y |] <- return (x .=. 3 .*. y .=. "hi" .*. emptyRecord)
>>> print (x,y)
(3,"hi")

[@expressions@]

Compare with the standard way to construct records above

>>> let x = 3; y = "hi"
>>> [pun|x y|]
Record{x=3,y="hi"}

[@nesting@]

Nesting is supported. Variables inside
@{ }@ and @( )@ are one level deeper, like the built-in syntax.
Furthermore the outer @{ }@ can be left out because @[pun|{x}|]@ is more
cluttered than @[pun|x|]@.
More concretely the pattern:


> let [pun| ab@{ a b } y z c{d} |] = x

is short for:

> let ab = x.ab
>     a = x.ab.a
>     b = x.ab.b
>     y = x.y
>     z = x.z
>     -- c is not bound
>     d = x.c.d

Where here `.` is a left-associative field lookup (as it is in other languages).

The pun quasiquoter can also be used in an expression context:

> let mkX ab a b y z d = [pun| ab@{ a b } y z c{d} |]
>     x = mkX ab b y z d

Here `mkX` includes @ab a b y z d@. @ab@ needs to be a record, and if it has
fields called @a@ or @b@ they are overridden by the values of @a@ and @b@ (via
'hLeftUnion' = '.<++.') . In other words,

> let mkX ab_ a b y z d = let ab = [pun| a b |] .<++. ab_
>                               in [pun| ab y z c{d} |]

For patterns, any order and additional fields are allowed if @{ }@ is used,
just as in built-in record syntax. But it is often necessary to restrict the
order and number of fields, such as if the record is a 'hRearrange' of a 'hLeftUnion'.
So use @( )@ instead:

> let [pun| (x _ y{}) |] = list
> -- desugars to something like:
> Record ((Tagged x :: Tagged "x" s1) `HCons`
>         (Tagged _ :: Tagged t   s2) `HCons`
>         (Tagged _ :: Tagged "y" s3) `HCons`
>          HNil) = list

Note that this also introduces the familiar wild card pattern (@_@),
and shows again how to ensure a label is present but not bind a variable
to it.

For comparison, here are three equivalent ways to define variables `x` and `y`

> let [pun| x y{} |] = r
> let [pun|{ x y{} }|] = r -- or this
> let x = r .!. (Label :: Label "x")
>     y = constrainType (r .!. (Label :: Label "y"))
>     constrainType :: Record t -> Record t
>     constrainType = id

See also @examples/pun.hs@. In @{}@ patterns, @pun@ can work with
'Variant' too.

-}


-- | requires labels to be promoted strings (kind Symbol), as provided by
-- "Data.HList.Label6" (ie. the label for foo is @Label :: Label \"foo\"@),
-- or "Data.HList.Labelable"
pun :: QuasiQuoter
pun = QuasiQuoter {
    quotePat = suppressWarning mp . parseRec,
    quoteExp = suppressWarning me . parseRec,
    quoteDec  = error "Data.HList.RecordPuns.quoteDec",
    quoteType = error "Data.HList.RecordPuns.quoteType"
 }


-- | the warning about @implicit {} added@ doesn't
-- make sense at top level (but it does if you say
-- have  [pun| x @ y |]
suppressWarning f (V a) = f (C [V a])
suppressWarning f x = f x

-- extracts ["x1","x2"] becomes \x -> (x .!. x1, x .!. x2),
-- where x1 = Label :: Label "x1"
extracts xs = do
    record <- newName "record"
    -- to fix #5 I could comment out the ensureLength below
    lamE [varP record] $ tupE
            [ [| $(varE record) .!. $label  |]
                | x <- xs,
                let label = [| Label :: Label $(litT (strTyLit x)) |],
                x /= "_"
                ]

mkPair :: String -> ExpQ -> ExpQ
mkPair x xe = [| (Label :: Label $(litT (strTyLit x))) .=. $xe |]



me :: Tree -> ExpQ
me (C as) = foldr (\(l,e) acc -> [| $(mkPair l e) .*. $acc |]) [| emptyRecord |] (mes as)
me (D _as) = error "Data.HList.RecordPuns.mp impossible"
me a = do
    reportWarning $ "Data.HList.RecordPuns.mp implicit {} added around:" ++ show a
    me (C [a])

mes :: [Tree] -> [(String, ExpQ)]
mes (V a : V "@": b : c) = (a, [| $(me b) `hLeftUnion` $(dyn a) |]) : mes c
mes (V a : C b : c)      = (a, me (C b)) : mes c
mes (V a : D b : c)      = (a, me (C b)) : mes c
mes (V a : b)            = (a, varE (mkName a)) : mes b
mes [] = []
mes inp = error $ "Data.HList.RecordPuns.mes: cannot translate remaining:" ++
                        show (map ppTree inp)

mp :: Tree -> PatQ
mp (C as) =
    let extractPats = mps as
        tupleP = tupP [ p | (binding, p) <- extractPats, binding /= "_" ]
    in viewP (extracts (map fst extractPats)) tupleP


mp (D as) = conP 'Record
  [foldr ( \ (n,p) xs -> conP 'HCons
                [ let ty
                          | n == "_"  = [| undefined :: Tagged anyLabel t |]
                          | otherwise = [| undefined :: Tagged $(litT (strTyLit n)) t |]
                  in viewP [| \x -> x `asTypeOf` $ty |]
                      (conP 'Tagged [p]),
                xs])
          (conP 'HNil [])
          (mps as)]
mp a = do
    reportWarning $ "Data.HList.RecordPuns.mp implicit {} added around:" ++ show a
    mp (C [a])

mps :: [Tree] -> [(String, PatQ)]
mps (V a : V "@" : b : c) = (a, asP (mkName a) (mp b)) :  mps c
mps (V a : C b : c) = (a, mp (C b)) : mps c
mps (V a : D b : c) = (a, mp (D b)) : mps c
mps (V "_" : b) = ("_", wildP) : mps b
mps (V a : b) = (a, varP (mkName a)) : mps b
mps [] = []
mps inp = error $ "Data.HList.RecordPuns.mps: cannot translate remaining pattern:" ++
                        show (map ppTree inp)

data Tree = C [Tree] -- ^ curly @{ }@
          | D [Tree] -- ^ @(  )@
          | V String -- ^ variable
  deriving Show

{- |

>>> parseRec "{ a b c {d e f}  } d"
C [C [V "a",V "b",V "c",C [V "d",V "e",V "f"]],V "d"]

>>> ppTree $ parseRec "{a b c {d e {} f @ g}}"
"{a b c {d e {} f @ g}}"

>>> ppTree $ parseRec "a b c {d e {} f @ g}"
"{a b c {d e {} f @ g}}"

>>> ppTree $ parseRec "(a b { (d) e } )"
"(a b {(d) e})"

-}
parseRec :: String -> Tree
parseRec str = case parseRec' 0 0 [[]] $ lexing str of
    [x] -> x -- avoid adding another layer if possible
    x -> C (reverse x)

parseRec' :: Int -> Int -> [[Tree]] -> [String] -> [Tree]
parseRec' n m accum  ("{" : rest)  = parseRec' (n+1) m ([] : accum) rest
parseRec' n m accum  ("(" : rest)  = parseRec'  n (m+1) ([] : accum) rest
parseRec' n m (a:b:c) ("}" : rest) = parseRec' (n-1) m ((C (reverse a) : b) : c)  rest
parseRec' n m (a:b:c) (")" : rest) = parseRec' n (m-1) ((D (reverse a) : b) : c)  rest
parseRec' n m (b:c) (a   : rest)
         | a `notElem` ["{","}","(",")"] = parseRec' n m   ((V a : b) : c) rest
parseRec' 0 0 (a:_) []             = a
parseRec' _ _ accum e              = error ("Data.HList.RecordPuns.parseRec' unexpected: " ++ show e
                                            ++ "\n parsed:" ++ show (reverse accum))

ppTree :: Tree -> String
ppTree (C ts) = "{" ++ unwords (map ppTree ts) ++ "}"
ppTree (D ts) = "(" ++ unwords (map ppTree ts) ++ ")"
ppTree (V x)  = x

lexing = unfoldr (\v -> case lex v of
                    ("", "") : _ -> Nothing
                    e : _ -> Just e
                    _ -> Nothing)
