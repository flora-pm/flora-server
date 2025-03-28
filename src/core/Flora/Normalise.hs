module Flora.Normalise where

import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Text (Text)

normaliseCategory :: Text -> Maybe Text
normaliseCategory string =
  if isJust $ List.find (\(_, name, _) -> name == string) floraCategories
    then Just string
    else case string of
      "Algorithm" -> Just "Algorithms"
      "Crypto" -> Just "Cryptography"
      "CLI" -> Just "CLI & TUI Development"
      "TUI" -> Just "CLI & TUI Development"
      "Command Line" -> Just "CLI & TUI Development"
      "CommandLine" -> Just "CLI & TUI Development"
      "Algebra" -> Just "Mathematics"
      "Arithmetic" -> Just "Mathematics"
      "Geometry" -> Just "Mathematics"
      "Graph" -> Just "Mathematics"
      "Graphs" -> Just "Mathematics"
      "Math" -> Just "Mathematics"
      "Mathematics" -> Just "Mathematics"
      "Maths" -> Just "Mathematics"
      "Number Theory" -> Just "Mathematics"
      "Numeric" -> Just "Mathematics"
      "Numerical" -> Just "Mathematics"
      "Numerics" -> Just "Mathematics"
      "Tropical Geometry" -> Just "Mathematics"
      "mathematics" -> Just "Mathematics"
      "Parser Builder" -> Just "Parsers"
      "Parser Combinators" -> Just "Parsers"
      "Parser" -> Just "Parsers"
      "ParserCombinators" -> Just "Parsers"
      "Parsers" -> Just "Parsers"
      "Parsing Text" -> Just "Parsers"
      "Parsing" -> Just "Parsers"
      "Network" -> Just "Network Development"
      "Data Network" -> Just "Network Development"
      "Network APIs" -> Just "Network Development"
      "Network Control" -> Just "Network Development"
      "NetworkAPI" -> Just "Network Development"
      "NetworkAPIs" -> Just "Network Development"
      "Networking" -> Just "Network Development"
      "Web" -> Just "Web Development"
      "Yesod" -> Just "Web Development"
      "Javascript" -> Just "Web Development"
      "OpenAPI" -> Just "Web Development"
      "Snap" -> Just "Web Development"
      "Servant" -> Just "Web Development"
      "Servant Web" -> Just "Web Development"
      "Web development" -> Just "Web Development"
      "Happstack" -> Just "Web Development"
      "Semantic Web" -> Just "Web Development"
      "Optics" -> Just "Lenses"
      "Lens" -> Just "Lenses"
      "Conduit" -> Just "Streaming"
      "Streamly" -> Just "Streaming"
      "Pipes" -> Just "Streaming"
      "Monad" -> Just "Monads"
      "MonadIO" -> Just "Monads"
      "Transformers" -> Just "Monads"
      "Monad Transformers" -> Just "Monads"
      "Mtl" -> Just "Monads"
      "User interfaces" -> Just "GUI"
      "User interface" -> Just "GUI"
      "UserInterface" -> Just "GUI"
      "UI" -> Just "GUI"
      "User Interfaces" -> Just "GUI"
      "graphics" -> Just "Graphics"
      "Code Generation" -> Just "FFI"
      "Elm" -> Just "FFI"
      "Erlang" -> Just "FFI"
      "Foreign binding" -> Just "FFI"
      "Foreign" -> Just "FFI"
      "JVM" -> Just "FFI"
      "Java" -> Just "FFI"
      "Jvm" -> Just "FFI"
      "PHP" -> Just "FFI"
      "TypeScript" -> Just "FFI"
      "language" -> Just "FFI"
      "Types" -> Just "Type System"
      "QuickCheck" -> Just "Testing"
      "Test" -> Just "Testing"
      "Validity" -> Just "Testing"
      "Algorithmic Music Composition" -> Just "Audio"
      "Automatic Music Generation" -> Just "Audio"
      "Sound" -> Just "Audio"
      "Music" -> Just "Audio"
      "Zip" -> Just "Compression"
      "ZLib" -> Just "Compression"
      "Tar" -> Just "Compression"
      "Cloud" -> Just "Cloud Computing"
      "Google" -> Just "Cloud Computing"
      "AWS" -> Just "Cloud Computing"
      "Compilers/Interpreters" -> Just "Compilers and Interpreters"
      "Interpreters" -> Just "Compilers and Interpreters"
      "Compiler" -> Just "Compilers and Interpreters"
      "DSL" -> Just "Compilers and Interpreters"
      "Database" -> Just "Databases"
      "PostgreSQL" -> Just "Databases"
      "NLP" -> Just "Natural Language Processing"
      "Japanese Natural Language Processing" -> Just "Natural Language Processing"
      "Natural Language" -> Just "Natural Language Processing"
      "Natural Language Processing" -> Just "Natural Language Processing"
      "Stemming" -> Just "Natural Language Processing"
      "Natural-language-processing" -> Just "Natural Language Processing"
      "Containers" -> Just "Data Structures"
      "Game" -> Just "Game Development"
      "Game Engine" -> Just "Game Development"
      "Concurrent" -> Just "Concurrency"
      "Parallel" -> Just "Parallelism"
      "Distributed Computing" -> Just "Distributed Systems & Computation"
      "Filesystem" -> Just "Systems Programming"
      "system" -> Just "Systems Programming"
      "System" -> Just "Systems Programming"
      "SYstem" -> Just "Systems Programming"
      "Embedded" -> Just "Systems Programming"
      "Distribution" -> Just "Package Distribution"
      "Debug" -> Just "Debugging"
      "Tracing" -> Just "Debugging"
      "OpenTelemetry" -> Just "Debugging"
      "Metrics" -> Just "Debugging"
      "Regex" -> Just "Text Processing"
      "text" -> Just "Text Processing"
      "Text" -> Just "Text Processing"
      _ -> Nothing

floraCategories :: [(Text, Text, Text)]
floraCategories =
  [ ("algorithms", "Algorithms", "Algorithms implemented in Haskell, like sorting, searching")
  , ("audio", "Audio", "Process digital signal, make music")
  , ("bioinformatics", "Bioinformatics", "Methods and software for the analysis of biological data")
  , ("cloud", "Cloud Computing", "Bindings to Cloud Computing platforms")
  , ("command-line", "CLI & TUI tooling", "Libraries to develop command-line interfaces")
  , ("compilers-interpreters", "Compilers and Interpreters", "Tooling to create compilers and interpreters")
  , ("compression", "Data compression", "Reducing the size of things")
  , ("concurrency", "Concurrency", "Concurrent programming techniques and tools")
  , ("cryptography", "Cryptography", "Algorithms for encrypting and hashing data")
  , ("data-structures", "Data Structures", "Data structures, whether purely functional or mutable")
  , ("databases", "Databases", "Database drivers and interfaces")
  , ("debugging", "Debugging", "Observe the behaviour of your code with tracing, logging, and telemetry.")
  , ("development", "Development", "Development helpers, integration with other languages")
  , ("distributed", "Distributed Systems & Computation", "Tooling and techniques for writing distributed systems")
  , ("distribution", "Package Distribution", "Building, Packaging and Distributing software in Haskell")
  , ("ffi", "FFI", "Working with other languages and generating bindings")
  , ("frp", "FRP", "Functional Reactive Programming")
  , ("game-dev", "Game Development", "Libraries used for game development")
  , ("generics", "Generics", "Working with Haskell's Generics mechanism")
  , ("graphics", "Graphics", "Programming the system's rendering APIs")
  , ("gui", "GUI", "Creating graphical user interfaces")
  , ("hardware", "Hardware", "Digital circuit description and hardware interfacing")
  , ("json", "JSON", "Parsing, producing and manipulating JSON")
  , ("lenses", "Lenses", "Functional references such as Lenses, Folds and Traversals")
  , ("maths", "Mathematics", "Numerical and Mathematical packages")
  , ("monads", "Monads", "Effectful sequential computations")
  , ("network", "Network Development", "Connection pools, DNS, HTTP, API clients and network protocols")
  , ("nlp", "Natural Language Processing", "Tooling to work with natural languages")
  , ("parallelism", "Parallelism", "Parallel programming")
  , ("parser-implementations", "Parser Implementations", "Parsing data formats")
  , ("parsers", "Parsers", "Parser generators, combinators and tools to help with parsing")
  , ("physics", "Physics", "The study of matter, its consituents, motion, and behaviour")
  , ("prelude", "Prelude", "Libraries that provide default imports")
  , ("profiling", "Profiling", "Measure the performance of your programs")
  , ("streaming", "Streaming", "Data streaming for continuous processing")
  , ("system", "Systems Programming", "Programming and communicating with the Operating System")
  , ("template-haskell", "Template Haskell", "Metaprogramming with Template Haskell")
  , ("testing", "Testing", "Test frameworks")
  , ("text-processing", "Text Processing", "Regular expressions, Unicode, Input validation")
  , ("type-system", "Type System", "Enhancing the Haskell type system")
  , ("web", "Web Development", "Programming for the web")
  , ("xml", "XML", "Libraries to consume and produce XML documents")
  ]
