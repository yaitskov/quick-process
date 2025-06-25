{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.FindCases where

import System.Process.Quick
import System.Process.Quick.Prelude


$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "find"
  (   ConstArg "."
  .*. Subcases
        "FindCases"
        [ Subcase "FindPrintf"
          (KeyArg @(Refined (Regex "^[%][fpactbnM%]$") String) "-printf" .*. HNil)
        , Subcase "FindExec"
          (KeyArg @(Refined (Regex "^(ls|file|du)$") String) "-exec" .*. ConstArgs ["{}", ";"] .*. HNil)
        ]
  .*. HNil
  )
 )
