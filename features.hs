-- Extract features

-- An answer in a question
data Answer = Answer { file :: String
            , number :: Integer
            , question :: String
            , answer :: String
            , isCorrect :: Bool

            -- Features?
            --, hasAbsolutes :: Bool
} deriving (Show)

type Feature = String -> Integer

main = do
  putStrLn $ "Hi"
