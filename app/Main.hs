module Main (main) where

import Control.Monad (unless, when)
import System.IO (hFlush, isEOF, stdout)

import Repl (AppState, LineResult(..), initialState, processLine)

-- Запуск главного цикла приложения
main :: IO ()
main = loop initialState

-- Читаем ввод пользователя и передаем в чистую функцию processLine
loop :: AppState -> IO ()
loop state = do
    putStr ">"
    hFlush stdout
    eof <- isEOF
    unless eof $ do
        line <- getLine
        let result = processLine state line
        -- Печатаем результат
        mapM_ putStrLn (outputs result)
        when (continue result) (loop (nextState result))

