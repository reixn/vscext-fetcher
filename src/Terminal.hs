module Terminal (withStyle, withColor) where

import Control.Exception
import System.Console.ANSI

withStyle :: [SGR] -> IO a -> IO a
withStyle sty a = finally (setSGR sty >> a) (setSGR [Reset])

withColor :: Color -> IO a -> IO a
withColor c = withStyle [SetColor Foreground Vivid c]