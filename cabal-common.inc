// This is a Cabal file template that is used to generate the final cabal file
// for this project using the C preprocessor. Use the `autogen` script from
// the project root directory to do the generation.
//
// This file has the same terms for use and distribution as other files
// in this software. See file LICENSE for details.

flag Werror
    description: Enable -Werror for fixing all warnings
    default: False
    manual: True

flag Weverything
    description: Enable almost every possible warning
    default: False
    manual: True

common options
    default-extensions:
        BlockArguments,
        DataKinds,
        DeriveAnyClass,
        DeriveGeneric,
        DerivingStrategies,
        DuplicateRecordFields,
        LambdaCase,
        MultiWayIf,
        NoFieldSelectors,
        NoStarIsType,
        OverloadedRecordDot,
        OverloadedStrings,
        QuasiQuotes,
        StrictData,
        TypeFamilies
    default-language: GHC2021
    ghc-options:
        -fshow-warning-groups

    if flag(Werror)
        ghc-options:
            -Werror

    if flag(Weverything)
        ghc-options:
            -Weverything
            -Wno-all-missed-specialisations
            -Wno-implicit-prelude
            -Wno-missed-specialisations
            -Wno-missing-import-lists
            -Wno-missing-kind-signatures
            -Wno-missing-safe-haskell-mode
            -Wno-monomorphism-restriction
            -Wno-safe
            -Wno-unsafe
            -Wno-unticked-promoted-constructors
    else
        ghc-options: -Wall -Wcompat
