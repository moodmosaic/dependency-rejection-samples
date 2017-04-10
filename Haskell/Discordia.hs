-- This module implements a basic Test Runner for testing QuickCheck properties.
-- It's mostly identical to what xmonad does. [1][2]
-- Given the (current) size of this codebase, that's the simplest thing that can
-- possibly work [3], without taking on needless dependencies on other packages.
-- It's named Discordia, after Eris (/ˈɪərɪs, ˈɛrɪs/; Greek: Ἔρις, "Strife") the
-- Greek goddess of strife 'n discord. Eris is the equivalent of Latin Discordia
-- which means "discord".
--
-- 1. https://github.com/xmonad/xmonad/blob/f03d2cdf74288bb3632b601ccd43bf02b0da4532/tests/Properties.hs#L27-L44
--
-- 2. xmonad scores currently 22 points in the (Haskell) Reddit: "What are some
--    examples of really good Haskell code?"
--    https://www.reddit.com/r/haskell/comments/2udvkh/what_are_some_examples_of_really_good_haskell_code/
--
-- 3. http://wiki.c2.com/?DoTheSimplestThingThatCouldPossiblyWork
--

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import           MaitreDTests

main :: IO Bool
main =
  checkParallel $ Group "Maître d' tests" [
      ("tryAccept behaves correctly when it can accept"
      , tryAcceptBehavesCorrectlyWhenItCanAccept)
    , ("tryAccept behaves correctly when it can not accept"
      , tryAcceptBehavesCorrectlyWhenItCanNotAccept)
    ]
