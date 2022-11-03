module Flora.TagsSpec where

import Data.Function ((&))
import Data.List qualified as List
import Data.Text qualified as Text
import Flora.TestUtils (TestEff, assertEqual, testThese, testThis)
import NLP.RAKE.Text qualified as RAKE
import Test.Tasty

spec :: TestEff TestTree
spec =
  testThese
    "tags tests"
    [ testThis "Check the tags from the `text` package" testTextTags
    ]

testTextTags :: TestEff ()
testTextTags = do
  let description =
        "An efficient packed, immutable Unicode text type (both strict and lazy). \
        \ . \
        \ The 'Text' type represents Unicode character strings, in a time and \
        \ space-efficient manner. This package provides text processing \
        \ capabilities that are optimized for performance critical use, both \
        \ in terms of large data quantities and high speed. \
        \ . \
        \ The 'Text' type provides character-encoding, type-safe case \
        \ conversion via whole-string case conversion functions (see \"Data.Text\"). \
        \ It also provides a range of functions for converting 'Text' values to \
        \ and from 'ByteStrings', using several standard encodings \
        \ (see \"Data.Text.Encoding\"). \
        \ . \
        \ Efficient locale-sensitive support for text IO is also supported \
        \ (see \"Data.Text.IO\"). \
        \ . \
        \ These modules are intended to be imported qualified, to avoid name \
        \ clashes with Prelude functions, e.g. \
        \ . \
        \ > import qualified Data.Text as T \
        \ . \
        \ == ICU Support \
        \ . \
        \ To use an extended and very rich family of functions for working \
        \ with Unicode text (including normalization, regular expressions, \
        \ non-standard encodings, text breaking, and locales), see \
        \ the [text-icu package](https://hackage.haskell.org/package/text-icu) \
        \ based on the well-respected and liberally \
        \ licensed [ICU library](http://site.icu-project.org/)."
  let keywords =
        RAKE.keywords description
          & List.filter (\ws -> snd ws >= 1.1)
          & List.map
            ( \(kw, score) ->
                let newKw = Text.split (== ' ') kw & List.take 2 & Text.unwords
                 in (newKw, score)
            )
  assertEqual keywords []
