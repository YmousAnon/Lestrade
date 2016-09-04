module UI.Input.Seed
(
    getSeed,
    onlyDigits,
) where

    import Data.Char

    import System.Random


    getSeed :: IO Int
    getSeed = putStrLn "Please input number to seed (blank for random):"
           >> trim <$> getLine >>= testSeed

    testSeed :: String -> IO Int
    testSeed str
        | null str       = let s = abs <$> randomIO
                            in s >>= putStrLn . ("Random seed: "++) . show >> s
        | onlyDigits str = return $ read str
        | otherwise      = getSeed


    trim :: String -> String
    trim = let f = reverse . dropWhile isSpace in f . f

    onlyDigits :: String -> Bool
    onlyDigits = foldr ((&&) . isDigit) True
