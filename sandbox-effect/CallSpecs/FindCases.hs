{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.FindCases where

import System.Process.Th
import System.Process.Th.Prelude


$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "find"
  (   ConstArg "."
  .*. Subcases
        "FindCases"
        [ Subcase "FindPrintf"
          (KeyArg @(Refined (Regex "^[%][fpactbnM%]$") String) "-printf" .*. HNil)
        , Subcase "FindExec"
          (KeyArg @(Refined (Regex "^(ls|file|du)$") String) "-exec" .*. ConstArg "{}" .*. ConstArg ";" .*. HNil)
        ]
  .*. HNil
  )
 )
