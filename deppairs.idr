module Main

import System
import Data.Vect

readVect : IO(len ** Vect len String)
readVect = do x <- getLine
              if x == ""
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** (x :: xs))

zipInputs : IO()
zipInputs = do putStrLn("enter a vector")
               (len1 ** vec1) <- readVect
               putStrLn("enter another vector")
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                 Nothing => putStrLn("different lengths")
                 Just vec2' => printLn(zip vec1 vec2')
