{-# OPTIONS_GHC -fplugin Data.BitCode.Plugin #-}
x, y :: Int
x = 1
y = 2

main = putStrLn $ "Hello World" ++ (show (x+y))
