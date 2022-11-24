{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Except
import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as LTIO
import Data.Yaml
import Options.Applicative
import System.Console.ANSI
import System.Exit
import Terminal
import VscExtFetcher

data VscExt = Ext
  { eName, ePublisher :: Text,
    eVersion :: Maybe Text
  }
  deriving (Show)

deriveFromJSON
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . tail,
      omitNothingFields = True
    }
  ''VscExt

main :: IO ()
main = do
  (o, i) <-
    execParser
      ( info
          ( ( (,)
                <$> option
                  (Just <$> str)
                  ( long "output"
                      <> short 'o'
                      <> value Nothing
                      <> help "Output file"
                  )
                <*> strArgument (metavar "INPUT")
            )
              <**> helper
          )
          mempty
      )

  r <- runFetcher $ do
    f <-
      liftIO (decodeFileEither i) >>= \case
        Right v -> return v
        Left e -> throwError ("failed to parse yaml: " ++ show e)
    getVscExts
      ( fmap
          ( \e ->
              VscExt
                { publisher = ePublisher e,
                  name = eName e,
                  version = fromMaybe "latest" (eVersion e)
                }
          )
          f
      )
  case r of
    Right v -> do
      case o of
        Just p -> LTIO.writeFile p v
        Nothing -> LTIO.putStrLn v
      withColor Green (putStrLn "[-] All done")
    Left e -> do
      withColor Red (putStrLn ("[!] " ++ e))
      exitWith (ExitFailure 1)
