module Main (main) where

import Control.Monad (unless, when)
import System.IO (hFlush, isEOF, stdout)

import Repl (AppState, LineResult(..), initialState, processLine)

main :: IO ()
main = loop initialState

loop :: AppState -> IO ()
loop state = do
    putStr ">"
    hFlush stdout
    eof <- isEOF
    unless eof $ do
        line <- getLine
        let result = processLine state line
        mapM_ putStrLn (outputs result)
        when (continue result) (loop (nextState result))

