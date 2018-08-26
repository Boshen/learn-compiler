module Repl where

import           Control.Monad.Trans
import           System.Console.Haskeline

import           Codegen                  (gen)
import           Parser                   (parseExpr)

repl :: IO ()
repl = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "% "
  case minput of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> liftIO (process input) >> loop

process :: String -> IO ()
process line =
  case parseExpr line of
    Right expr -> print $ gen expr
    Left err   -> print err
