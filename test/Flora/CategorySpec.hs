module Flora.CategorySpec (spec) where

import Data.Set qualified as Set
import Test.Tasty

import Flora.Normalise
import Flora.TestUtils

spec :: TestEff TestTree
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

testNormalisationOfMathematicsCategories :: TestEff ()
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
  assertEqual
    (Set.singleton (Just "Mathematics"))
    (Set.map normaliseCategory mathematicsExceptions)

testNormalisationOfCliAndTuiCategories :: TestEff ()
testNormalisationOfCliAndTuiCategories = do
  let cliAndTuiExceptions =
        Set.fromList
          [ "CLI"
          , "TUI"
          ]
  assertEqual
    (Set.singleton (Just "CLI & TUI Development"))
    (Set.map normaliseCategory cliAndTuiExceptions)

testNormalisationOfParserCategories :: TestEff ()
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
  assertEqual
    (Set.singleton (Just "Parsers"))
    (Set.map normaliseCategory parserExceptions)

testNormalisationOfNetworkCategories :: TestEff ()
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
  assertEqual
    (Set.singleton (Just "Network Development"))
    (Set.map normaliseCategory networkExceptions)

testNormalisationOfWebDevelopmentCategories :: TestEff ()
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

  assertEqual
    (Set.singleton (Just "Web Development"))
    (Set.map normaliseCategory webDevelopmentExceptions)

testNormalisationOfLensesCategories :: TestEff ()
testNormalisationOfLensesCategories = do
  let lensesExceptions =
        Set.fromList
          [ "Optics"
          , "Lens"
          ]

  assertEqual
    (Set.singleton (Just "Lenses"))
    (Set.map normaliseCategory lensesExceptions)

testNormalisationOfStreamingCategories :: TestEff ()
testNormalisationOfStreamingCategories = do
  let streamingExceptions =
        Set.fromList
          [ "Conduit"
          , "Streamly"
          , "Pipes"
          ]

  assertEqual
    (Set.singleton (Just "Streaming"))
    (Set.map normaliseCategory streamingExceptions)

testNormalisationOfMonadsCategories :: TestEff ()
testNormalisationOfMonadsCategories = do
  let monadsExceptions =
        Set.fromList
          [ "Monad"
          , "MonadIO"
          , "Transformers"
          , "Monad Transformers"
          , "Mtl"
          ]

  assertEqual
    (Set.singleton (Just "Monads"))
    (Set.map normaliseCategory monadsExceptions)

testNormalisationOfGUICategories :: TestEff ()
testNormalisationOfGUICategories = do
  let guiExceptions =
        Set.fromList
          [ "User interfaces"
          , "User interface"
          , "UserInterface"
          , "UI"
          , "User Interfaces"
          ]

  assertEqual
    (Set.singleton (Just "GUI"))
    (Set.map normaliseCategory guiExceptions)

testNormalisationOfFFICategories :: TestEff ()
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

  assertEqual
    (Set.singleton (Just "FFI"))
    (Set.map normaliseCategory ffiExceptions)

testNormalisationOfTestingCategories :: TestEff ()
testNormalisationOfTestingCategories = do
  let testingExceptions =
        Set.fromList
          [ "Validity"
          , "QuickCheck"
          , "Test"
          ]

  assertEqual
    (Set.singleton (Just "Testing"))
    (Set.map normaliseCategory testingExceptions)

testNormalisationOfAudioCategories :: TestEff ()
testNormalisationOfAudioCategories = do
  let audioExceptions =
        Set.fromList
          [ "Algorithmic Music Composition"
          , "Automatic Music Generation"
          , "Sound"
          , "Music"
          ]

  assertEqual
    (Set.singleton (Just "Audio"))
    (Set.map normaliseCategory audioExceptions)

testNormalisationOfCompressionCategories :: TestEff ()
testNormalisationOfCompressionCategories = do
  let compressionExceptions =
        Set.fromList
          [ "Zip"
          , "ZLib"
          , "Tar"
          ]

  assertEqual
    (Set.singleton (Just "Compression"))
    (Set.map normaliseCategory compressionExceptions)

testNormalisationOfCloudComputingCategories :: TestEff ()
testNormalisationOfCloudComputingCategories = do
  let cloudComputingExceptions =
        Set.fromList
          [ "Cloud"
          , "Google"
          , "AWS"
          ]

  assertEqual
    (Set.singleton (Just "Cloud Computing"))
    (Set.map normaliseCategory cloudComputingExceptions)

testNormalisationOfCompilersCategories :: TestEff ()
testNormalisationOfCompilersCategories = do
  let compilersExceptions =
        Set.fromList
          [ "Compilers/Interpreters"
          , "Interpreters"
          , "Compiler"
          , "DSL"
          ]

  assertEqual
    (Set.singleton (Just "Compilers and Interpreters"))
    (Set.map normaliseCategory compilersExceptions)

testNormalisationOfNLPCategories :: TestEff ()
testNormalisationOfNLPCategories = do
  let nlpExceptions =
        Set.fromList
          [ "Japanese Natural Language Processing"
          , "Natural Language"
          , "Natural Language Processing"
          , "Stemming"
          , "Natural-language-processing"
          ]

  assertEqual
    (Set.singleton (Just "Natural Language Processing"))
    (Set.map normaliseCategory nlpExceptions)

testNormalisationOfSystemsProgrammingCategories :: TestEff ()
testNormalisationOfSystemsProgrammingCategories = do
  let systemsProgrammingExceptions =
        Set.fromList
          [ "Filesystem"
          , "system"
          , "System"
          , "SYstem"
          , "Embedded"
          ]

  assertEqual
    (Set.singleton (Just "Systems Programming"))
    (Set.map normaliseCategory systemsProgrammingExceptions)

testNormalisationOfDebuggingCategories :: TestEff ()
testNormalisationOfDebuggingCategories = do
  let debuggingExceptions =
        Set.fromList
          [ "Debug"
          , "Tracing"
          , "OpenTelemetry"
          , "Metrics"
          ]

  assertEqual
    (Set.singleton (Just "Debugging"))
    (Set.map normaliseCategory debuggingExceptions)

testNormalisationOfTextProcessingCategories :: TestEff ()
testNormalisationOfTextProcessingCategories = do
  let textProcessingExceptions =
        Set.fromList
          [ "Regex"
          , "text"
          , "Text"
          ]

  assertEqual
    (Set.singleton (Just "Text Processing"))
    (Set.map normaliseCategory textProcessingExceptions)
