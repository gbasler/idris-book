module Main

main : IO ()
main = getLine >>= \input => let len = length input in
              putStrLn (show len)
