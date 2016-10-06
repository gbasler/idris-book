module Main

main2 : IO ()
main2 = putStr "First string: " >>= \_ =>
          getLine >>= \input1 =>
          putStr "Second string: " >>= \_ =>
          getLine >>= \input2 =>
          let len1 = length input2
              len2 = length input2
              longer = if (len1 > len2) then input1 else input2 in
          putStrLn ("longer word: " ++ longer)

main3 : IO ()
main3 = do putStr "First string: "
           input1 <- getLine
           putStr "Second string: "
           input2 <- getLine
           let len1 = length input2
           let len2 = length input2
           let longer = if (len1 > len2) then input1 else input2
           putStrLn ("longer word is: " ++ longer)

main : IO ()
main = main2
