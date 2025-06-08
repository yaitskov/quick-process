module System.Process.Quick.Sbv.Arbitrary where

import System.Process.Quick.Prelude
import Data.SBV
import Data.SBV.String qualified as S
import Data.SBV.RegExp

getSingleValue :: (SymVal b, Modelable m) => m -> Maybe b
getSingleValue m
  | modelExists m =
    case toPairs $ getModelDictionary m of
      [(k, _)] -> getModelValue k m
      _ -> Nothing
  | otherwise = Nothing

satOne :: (Satisfiable a, SymVal b) => Int -> a -> Maybe b
satOne _n p = unsafePerformIO (getSingleValue <$> sat p)

satN :: (Satisfiable a, SymVal b) => Int -> a -> [b]
satN n p = unsafePerformIO (mapMaybe getSingleValue . allSatResults <$> asat)
  where
    asat = allSatWith defaultSMTCfg { allSatMaxModelCount = Just n } p

findStringByRegex :: (SymVal b) => RegExp -> Gen b
findStringByRegex r = go (3 :: Int)
  where
    go t = sized $ \l ->
      if t > 0
        then do
          n <- chooseInt (0, l)
          case trySat n of
            Just y -> pure y
            Nothing -> go $ t - 1
        else do
          case satN l matchRx of
            [] -> error $ "No solution for regex: " <> show r
            ss -> elements ss

    matchRx (x :: SString) = match x r

    trySat n =
      satOne n (\x -> matchRx x
                 .&& S.length x .== literal (fromIntegral n))
