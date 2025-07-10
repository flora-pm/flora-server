module Flora.CategorySpec (spec) where

import Data.Set qualified as Set
import RequireCallStack
import Test.Tasty

import Flora.Normalise
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Category Normalisation"
    [ testThis "Mathematics categories" testNormalisationOfMathematicsCategories
    , testThis "CLI & TUI categories" testNormalisationOfCliAndTuiCategories
    , testThis "Parsers" testNormalisationOfParserCategories
    , testThis "Network" testNormalisationOfNetworkCategories
    , testThis "Web Development" testNormalisationOfWebDevelopmentCategories
    , testThis "Lenses" testNormalisationOfLensesCategories
    , testThis "Streaming" testNormalisationOfStreamingCategories
    , testThis "Monads" testNormalisationOfMonadsCategories
    , testThis "GUI" testNormalisationOfGUICategories
    , testThis "FFI" testNormalisationOfFFICategories
    , testThis "Testing" testNormalisationOfTestingCategories
    , testThis "Audio" testNormalisationOfAudioCategories
    , testThis "Compression" testNormalisationOfCompressionCategories
    , testThis "Cloud Computing" testNormalisationOfCloudComputingCategories
    , testThis "Compilers & Interpreters" testNormalisationOfCompilersCategories
    , testThis "NLP" testNormalisationOfNLPCategories
    , testThis "Systems Programming" testNormalisationOfSystemsProgrammingCategories
    , testThis "Debugging" testNormalisationOfDebuggingCategories
    , testThis "Text Processing" testNormalisationOfTextProcessingCategories
    ]

testNormalisationOfMathematicsCategories :: RequireCallStack => TestEff ()
testNormalisationOfMathematicsCategories = do
  let mathematicsExceptions =
        Set.fromList
          [ "Algebra"
          , "Arithmetic"
          , "Geometry"
          , "Graph"
          , "Graphs"
          , "Math"
          , "Mathematics"
          , "Maths"
          , "Number Theory"
          , "Numeric"
          , "Numerical"
          , "Numerics"
          , "Tropical Geometry"
          , "mathematics"
          ]
  assertEqual_
    (Set.singleton (Just "Mathematics"))
    (Set.map normaliseCategory mathematicsExceptions)

testNormalisationOfCliAndTuiCategories :: RequireCallStack => TestEff ()
testNormalisationOfCliAndTuiCategories = do
  let cliAndTuiExceptions =
        Set.fromList
          [ "CLI"
          , "TUI"
          ]
  assertEqual_
    (Set.singleton (Just "CLI & TUI Development"))
    (Set.map normaliseCategory cliAndTuiExceptions)

testNormalisationOfParserCategories :: RequireCallStack => TestEff ()
testNormalisationOfParserCategories = do
  let parserExceptions =
        Set.fromList
          [ "Parser Builder"
          , "Parser Combinators"
          , "Parser"
          , "ParserCombinators"
          , "Parsers"
          , "Parsing Text"
          ]
  assertEqual_
    (Set.singleton (Just "Parsers"))
    (Set.map normaliseCategory parserExceptions)

testNormalisationOfNetworkCategories :: RequireCallStack => TestEff ()
testNormalisationOfNetworkCategories = do
  let networkExceptions =
        Set.fromList
          [ "Network"
          , "Data Network"
          , "Network APIs"
          , "Network Control"
          , "NetworkAPI"
          , "NetworkAPIs"
          , "Networking"
          ]
  assertEqual_
    (Set.singleton (Just "Network Development"))
    (Set.map normaliseCategory networkExceptions)

testNormalisationOfWebDevelopmentCategories :: RequireCallStack => TestEff ()
testNormalisationOfWebDevelopmentCategories = do
  let webDevelopmentExceptions =
        Set.fromList
          [ "Web"
          , "Yesod"
          , "Javascript"
          , "OpenAPI"
          , "Snap"
          , "Servant"
          , "Servant Web"
          , "Web development"
          , "Happstack"
          , "Semantic Web"
          ]

  assertEqual_
    (Set.singleton (Just "Web Development"))
    (Set.map normaliseCategory webDevelopmentExceptions)

testNormalisationOfLensesCategories :: RequireCallStack => TestEff ()
testNormalisationOfLensesCategories = do
  let lensesExceptions =
        Set.fromList
          [ "Optics"
          , "Lens"
          ]

  assertEqual_
    (Set.singleton (Just "Lenses"))
    (Set.map normaliseCategory lensesExceptions)

testNormalisationOfStreamingCategories :: RequireCallStack => TestEff ()
testNormalisationOfStreamingCategories = do
  let streamingExceptions =
        Set.fromList
          [ "Conduit"
          , "Streamly"
          , "Pipes"
          ]

  assertEqual_
    (Set.singleton (Just "Streaming"))
    (Set.map normaliseCategory streamingExceptions)

testNormalisationOfMonadsCategories :: RequireCallStack => TestEff ()
testNormalisationOfMonadsCategories = do
  let monadsExceptions =
        Set.fromList
          [ "Monad"
          , "MonadIO"
          , "Transformers"
          , "Monad Transformers"
          , "Mtl"
          ]

  assertEqual_
    (Set.singleton (Just "Monads"))
    (Set.map normaliseCategory monadsExceptions)

testNormalisationOfGUICategories :: RequireCallStack => TestEff ()
testNormalisationOfGUICategories = do
  let guiExceptions =
        Set.fromList
          [ "User interfaces"
          , "User interface"
          , "UserInterface"
          , "UI"
          , "User Interfaces"
          ]

  assertEqual_
    (Set.singleton (Just "GUI"))
    (Set.map normaliseCategory guiExceptions)

testNormalisationOfFFICategories :: RequireCallStack => TestEff ()
testNormalisationOfFFICategories = do
  let ffiExceptions =
        Set.fromList
          [ "Code Generation"
          , "Elm"
          , "Erlang"
          , "Foreign binding"
          , "Foreign"
          , "JVM"
          , "Java"
          , "Jvm"
          , "PHP"
          , "TypeScript"
          , "language"
          ]

  assertEqual_
    (Set.singleton (Just "FFI"))
    (Set.map normaliseCategory ffiExceptions)

testNormalisationOfTestingCategories :: RequireCallStack => TestEff ()
testNormalisationOfTestingCategories = do
  let testingExceptions =
        Set.fromList
          [ "Validity"
          , "QuickCheck"
          , "Test"
          ]

  assertEqual_
    (Set.singleton (Just "Testing"))
    (Set.map normaliseCategory testingExceptions)

testNormalisationOfAudioCategories :: RequireCallStack => TestEff ()
testNormalisationOfAudioCategories = do
  let audioExceptions =
        Set.fromList
          [ "Algorithmic Music Composition"
          , "Automatic Music Generation"
          , "Sound"
          , "Music"
          ]

  assertEqual_
    (Set.singleton (Just "Audio"))
    (Set.map normaliseCategory audioExceptions)

testNormalisationOfCompressionCategories :: RequireCallStack => TestEff ()
testNormalisationOfCompressionCategories = do
  let compressionExceptions =
        Set.fromList
          [ "Zip"
          , "ZLib"
          , "Tar"
          ]

  assertEqual_
    (Set.singleton (Just "Compression"))
    (Set.map normaliseCategory compressionExceptions)

testNormalisationOfCloudComputingCategories :: RequireCallStack => TestEff ()
testNormalisationOfCloudComputingCategories = do
  let cloudComputingExceptions =
        Set.fromList
          [ "Cloud"
          , "Google"
          , "AWS"
          ]

  assertEqual_
    (Set.singleton (Just "Cloud Computing"))
    (Set.map normaliseCategory cloudComputingExceptions)

testNormalisationOfCompilersCategories :: RequireCallStack => TestEff ()
testNormalisationOfCompilersCategories = do
  let compilersExceptions =
        Set.fromList
          [ "Compilers/Interpreters"
          , "Interpreters"
          , "Compiler"
          , "DSL"
          ]

  assertEqual_
    (Set.singleton (Just "Compilers and Interpreters"))
    (Set.map normaliseCategory compilersExceptions)

testNormalisationOfNLPCategories :: RequireCallStack => TestEff ()
testNormalisationOfNLPCategories = do
  let nlpExceptions =
        Set.fromList
          [ "Japanese Natural Language Processing"
          , "Natural Language"
          , "Natural Language Processing"
          , "Stemming"
          , "Natural-language-processing"
          ]

  assertEqual_
    (Set.singleton (Just "Natural Language Processing"))
    (Set.map normaliseCategory nlpExceptions)

testNormalisationOfSystemsProgrammingCategories :: RequireCallStack => TestEff ()
testNormalisationOfSystemsProgrammingCategories = do
  let systemsProgrammingExceptions =
        Set.fromList
          [ "Filesystem"
          , "system"
          , "System"
          , "SYstem"
          , "Embedded"
          ]

  assertEqual_
    (Set.singleton (Just "Systems Programming"))
    (Set.map normaliseCategory systemsProgrammingExceptions)

testNormalisationOfDebuggingCategories :: RequireCallStack => TestEff ()
testNormalisationOfDebuggingCategories = do
  let debuggingExceptions =
        Set.fromList
          [ "Debug"
          , "Tracing"
          , "OpenTelemetry"
          , "Metrics"
          ]

  assertEqual_
    (Set.singleton (Just "Debugging"))
    (Set.map normaliseCategory debuggingExceptions)

testNormalisationOfTextProcessingCategories :: RequireCallStack => TestEff ()
testNormalisationOfTextProcessingCategories = do
  let textProcessingExceptions =
        Set.fromList
          [ "Regex"
          , "text"
          , "Text"
          ]

  assertEqual_
    (Set.singleton (Just "Text Processing"))
    (Set.map normaliseCategory textProcessingExceptions)
