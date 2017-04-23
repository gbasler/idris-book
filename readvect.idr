module Main

import System
import Data.Vect

readVecLen : (len : Nat) -> IO (Vect len String)
readVecLen Z = pure []
readVecLen (S k) = do x <- getLine
                      xs <- readVecLen k
                      pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVec : IO(VectUnknown String)
readVec = do x <- getLine
             if x == ""
               then pure (MkVect _ [])
               else do MkVect _ xs <- readVec
                       pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) =
  putStrLn (show(xs) ++ ( " (length " ++ show len ++ ")" ) )

anyVect : (n ** Vect n String)
anyVect = (3 ** ?wtf)

readVect : IO(len ** Vect len String)
readVect = do x <- getLine
              if x == ""
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** (x :: xs))
