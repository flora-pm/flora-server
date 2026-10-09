module FloraWeb.Common.Terminal (blueMessage, redMessage) where

import Data.Text (Text)
import Data.Text.IO qualified as Text

-- | Print a message in vivid blue, reset the colour, and end the line.
blueMessage :: Text -> IO ()
blueMessage message = Text.putStrLn $ "\ESC[94m" <> message <> "\ESC[0m"

-- | Print a message in vivid red, reset the colour, and end the line.
redMessage :: Text -> IO ()
redMessage message = Text.putStrLn $ "\ESC[91m" <> message <> "\ESC[0m"
