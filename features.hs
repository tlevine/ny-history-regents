-- Extract features
import System.IO
import qualified Data.Set as S
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

-- An answer to a question
data Answer = Answer { file :: String
                     , number :: Integer
                     , question :: String
                     , answer :: String
                     , isCorrect :: Bool
           
                     -- Features?
                     --, hasAbsolutes :: Bool
} deriving (Show)

-- All of the answers for a question
data Question = Set Answer

levenshtein :: String -> String -> Int
levenshtein s t = d!!(length s)!!(length t) 
  where
    d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

sumLevenshtein :: [String] -> String -> Int
sumLevenshtein answers thisAnswer = sum $ map (levenshtein thisAnswer) $ answers

--isAbsolute :: String -> Bool
--isAbsolute word = S.member word absolutes
--  where
--    absolutes = S.fromList ["all", "every", "never"]

--nAbsolutes :: String -> Int
--nAbsolutes question = length $ filter $ map isAbsolute $ words question

a = [ "increasing factory employment opportunities placing blame only on civilian leaders", "encouraging immigration from Africa forcing nations to pay for war damages", "focusing attention on artistic contributions returning conquered territories to their", "bringing an end to legalized racial segregation holding individuals accountable for their war"]

questionQuery = "SELECT DISTINCT examfile, \"number\" FROM answer;"
answerQuery = "SELECT examfile, \"number\", question, answer, isCorrect FROM answer WHERE examfile = ? AND \"number\" = ?;"

convAnswer :: [SqlValue] -> Answer
convAnswer [examfile, number, question, answer, isCorrect] = Answer { file  = (fromSql examfile) :: String
                                                                 , number  = (fromSql number) :: Integer
                                                                 , question = (fromSql question) :: String
                                                                 , answer = (fromSql answer) :: String
                                                                 , isCorrect  = (fromSql isCorrect) :: Bool
}

main :: IO ()
main = do
  conn <- connectSqlite3 "/tmp/history-regents.db"
  
  rQuestions <- quickQuery' conn questionQuery []
  let rAnswers = mapM_ (\question -> quickQuery' conn answerQuery question) rQuestions

  --rAnswers
  --let answers = map (\rAnswer -> map convAnswer rAnswer) rAnswers
  
  -- Print the rows out
  -- mapM_ (\r -> putStrLn $ answer r) answers
  
  -- And disconnect from the database
  disconnect conn


