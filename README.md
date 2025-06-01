# quick-process

## Motivation
The strongest trait of Haskell language is its type system.  This
powerful type system gives infinite opportunities for experimenting
with mapping relational entities onto application values in safer,
more comprehensible and maintainable ways.

Compare popularity of Java and Haskell languagues and number of SOL libraries in them:

```haskell
> length $ words "Hasql Beam Reigh8 postgresql-typed persistent esqueleto Opaleye Rel8 Squeal Selda Groundhog"
11
> length $ words "JPA Hibernate JOOQ EJB"
4
```

Haskell ecosystem counts 2.75 times more SQL libraries nonetheless
according to [TIOBE index](https://www.tiobe.com/tiobe-index/) in 2025
Java is 20 times more popular than Haskell and by
[PYPL](https://pypl.github.io/PYPL.html) 126 times!

As far as I remember only [JOOQ](https://www.jooq.org/) resembles a
type safe library. Other libraries require runtime environment to
check compatibility of codebase with SQL queries.

RDBMSs talk SQL and it are inherently text oriented for extenal
clients. All these Haskell libraries first of all are trying to hide
plain string manipulation behind type fence as deep as possible.

Once I tried had to launch an external process in a Haskell program.
Keeping in mind the 50-200x slope on SQL arena in Haskell, I expected
to find at least a few libraries on
[hackage](https://hackage.haskell.org/) providing some type safety
layer between my application and execv syscall interface accepting a
bare strings.

The observation above motivated me experimenting with a type safe
wrapper for [process](https://hackage.haskell.org/package/process)
library.

Structure of command line arguments is way simpler than SQL. An
external program can be modelled as a function with a side effect.
Haskell has an amazing library for testing functions -
[QuickCheck](hackage.haskell.org/package/QuickCheck) including impure
ones.

Main concern of external programs - they are not shipped with the
application.  Recall [PRM
hell](https://en.wikipedia.org/wiki/Dependency_hell) phrase. These
days situation with external explicit dependency resolution during
software installation and upgrade improved by nix and bazel. Nix and
bazel are powerful, because they can pack/isolate/unpack the whole
dependency universe of a single app, but they are complicated systems
with a steep learning curve. Plus nix is not supported on Windows.
That's why they've got limited popularity and lot of software is still
distributed as a self-extracting archive assuming some dependencies are
compatible and preinstalled manually.

Explicit list of dependencies is manually currated.

Language does not provide out of the box solution to build such list.
Taking into account human factor explicit list of dependencies always
has a chance to diverge from the full (effective) one.  E.g. host
system got newer version of dependency which behaves differently.

Software installation out of prebuilt executables usually don't run
tests.

## Goals

quick-process defines following goals:

- provide DSL for describing a call spec of an external program
- generate types, from the call spec, compatible with application
  domain and arguments of an external program
- automatic discovery of call specs in code base
- check call spec compatibility during app development, testing and
  installation
- process launch and mapping call spec to CreadeProcess

<!-- ## Haskell projects launching processes -->

<!-- List of open source Haskell projects using process library for -->
<!-- specific programs: -->

<!-- - [ogma](https://github.com/nasa/ogma) -->
<!-- - [IHP](https://github.com/digitallyinduced/ihp) -->
<!-- - [pandoc](https://github.com/jgm/pandoc/) -->
<!-- - [aura](https://hackage.haskell.org/package/aura) -->
<!-- - [Agda](https://hackage.haskell.org/package/Agda) -->
<!-- - [hoogle](https://hackage.haskell.org/package/hoogle) -->
<!-- - [yi-core](https://hackage.haskell.org/package/yi-core) -->
<!-- - [debian](https://hackage.haskell.org/package/debian) -->

## Call spec verification

Often call spec can be verified with `--help` key terminating command
line arguments. It's way easier than running the program in sandbox,
because no files gerenration is required and validating after effects
either.  Help key validation support can be checked.


## Examples

### Constant argument

``` haskell
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs where
import System.Process.Th

$(genCallSpec [TrailingHelpValidate, SandboxValidate] "date" (ConstArg "+%Y" .*. HNil))
-------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module CallSpecTest where

import CallSpecs
import System.Process.Th

main :: IO ()
main = $(discoverAndVerifyCallSpecs
          (fromList [ TrailingHelpValidate
                    , SandboxValidate
                    ])
          3)
-------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Main where

import CallSpecs
import System.Process.Th

main :: IO ()
main = callProcess Date
```

`genCallSpec` defines type `Date` with nullary constructor and
`CallSpec` instance for it.

`discoverAndVerifyCallSpecs` discovers all types with `CallSpec`
instances, generates 3 values per type ande executes help key check.
There is not much to check besides exit code in Date spec.

`callProcess` is similar to
[callProcess](https://hackage.haskell.org/package/process/docs/System-Process.html#v:callProcess)
from process library, but accepts typed input instead of strings.

### Variable argument

``` haskell
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs where
import System.Process.Th

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "/bin/cp"
  (   VarArg @(Refined (InFile "hs") FilePath) "source"
  .*. VarArg @(Refined (OutFile "*") FilePath) "destination"
  .*. HNil
  )
 )
-------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module CallSpecTest where

import CallSpecs
import System.Process.Th

main :: IO ()
main = $(discoverAndVerifyCallSpecs
          (fromList [ TrailingHelpValidate
                    , SandboxValidate
                    ])
          100)
-------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Main where

import CallSpecs
import System.Process.Th

main :: IO ()
main =
  callProcess $ BinCp $(refinedTH "app.hs") $(refinedTH "app.bak")
```

`CallSpec` of cp command requires 2 parameters and here quick-process
power start to show up. Refined constraint InFile ensures that first
string is a valid file path to a Haskell source file. This part is
delegated to [refined](https://hackage.haskell.org/package/refined)
library. HelpKey mode generates appropriate values, but they don't
point to real files on disk.  Use Sandbox mode to actually launch
process in a temporary dir with real files.  In Sandbox `OutFile`
cause to check that the file appears on the path once process
terminates.


### Subcases

Call spec can be composed of sum types.

``` haskell
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs where
import System.Process.Th

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
```

Note usage of `Regex` predicate - thanks to
[sbv](https://hackage.haskell.org/package/refined) and z3 SMT solver
values satisfing arbitrary TDFA regex can be generated.
