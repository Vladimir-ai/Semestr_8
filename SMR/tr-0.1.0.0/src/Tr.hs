-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

import Data.Maybe
import Data.List
import GHC.Float

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.

tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs =
  -- Check if _outset arg is empty
  case _outset of
  Nothing ->
    -- Remove if there isn't any symbols.
    filter (\c -> isNothing (c `elemIndex` _inset)) xs
  Just value ->
    let rep_count = ceiling (int2Double(get_diff _inset _outset) / int2Double(length value))
    in
      let actual_outset = take (length _inset) (concat (replicate rep_count value))
      in map (\c -> if isNothing (c `elemIndex` _inset) then c else actual_outset !! fromMaybe 0 (c `elemIndex` _inset)) xs

    where get_diff _inset _outset = if (length _inset - length _outset) > 0 then length _inset else 1