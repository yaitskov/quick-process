-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module VerifyCallSpecs where

import CallSpecs.Find ()
import System.Process.Quick
import System.Process.Quick.Prelude


-- goRef :: forall x. (Typeable x, Data x) => x -> IO x
-- goRef x =
--   case cast x of
--     Just (i :: Refined Positive Int) -> do
--       putStrLn $ "i " <> show (unrefine i * 22)
--       case refine $ (unrefine i) * 22 of
--         Left _e -> pure x
--         Right (caed :: Refined Positive Int) ->
--           case cast caed of
--             Nothing -> pure x
--             Just r -> pure r
--     _ -> pure x

-- go :: forall x. (Typeable x, Data x) => x -> IO x
-- go x =
--   case cast x of
--     Just (Tagged i :: Tagged "a" Int) -> do
--       putStrLn $ "i " <> show (i * 22)
--       case cast (Tagged @"a" $ i * 22) of
--         Nothing -> pure x
--         Just r -> pure r
--     _ -> pure x

-- go1 :: forall x. (Typeable x, Data x) => x -> IO x
-- go1 x =
--   case cast x of
--     Just (Fo i :: Fo "a" Int) -> do
--       putStrLn $ "i " <> show (i * 22)
--       case cast (Fo @"a" $ i * 22) of
--         Nothing -> pure x
--         Just r -> pure r
--     _ -> pure x


-- go2 :: forall x. (Typeable x, Data x) => x -> IO x
-- go2 x
--   | _ `App` a@(TypeRep @(aa :: k2)) `App` _ <- typeOf x -- TypeRep @x
--   , TypeRep <- typeRepKind a -- ?
--   , Just Refl <- eqT @x @(Fo (aa) Int)
--   = do let Fo i = x
--        putStrLn $ "i " <> show (i * 22)
--        pure . Fo $ i * 22

--   | otherwise = pure x

main :: IO ()
main = $(discoverAndVerifyCallSpecs (fromList [TrailingHelpValidate]) 1)
