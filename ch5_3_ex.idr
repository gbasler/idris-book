module Main

import System
import Data.Vect

readToBlank : IO(List String)
readToBlank = do line <- getLine
                 if line == ""
                   then pure (the (List String) [])
                   else do lines <- readToBlank
                           pure(line :: lines)

readToBlankMain : IO ()
readToBlankMain = do putStrLn "Type some lines!"
                     lines <- readToBlank
                     putStrLn("you typed: " ++ show(lines))

readAndSave : IO ()
readAndSave = do
  putStrLn "Type some lines!"
  lines <- readToBlank
  putStrLn "Type a file name!"
  fileName <- getLine
  putStrLn("Saving: " ++ show(lines) ++ " to " ++ fileName)
  res <- writeFile fileName (unlines lines)
  case res of
    Left e => putStrLn("Error: " ++ show(e))
    Right () => putStrLn "success"

main : IO ()
main = readAndSave
