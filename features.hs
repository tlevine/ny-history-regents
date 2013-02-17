-- Extract features
import System.IO
import qualified Data.Set as S
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

-- An answer to a question
data Answer = Answer { file :: String
                     , number :: Int
                     , choice :: Int
                     , question :: String
                     , answer :: String
                     , isCorrect :: Bool
           
                     -- Features?
                     --, hasAbsolutes :: Bool
} deriving (Show)

-- All of the answers for a question
type Question = [Answer]

levenshtein :: String -> String -> Int
levenshtein s t = d!!(length s)!!(length t) 
  where
    d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

sumLevenshtein :: Question -> Answer -> Int
sumLevenshtein answers thisAnswer = sum $ map (levenshtein $ answer thisAnswer) $ map answer answers
  where
    thisAnswerStr = answer thisAnswer

questionQuery = "SELECT DISTINCT examfile, \"number\" FROM answer LIMIT 10;"
answerQuery = "SELECT examfile, \"number\", choice, question, answer, isCorrect FROM answer WHERE examfile = ? AND \"number\" = ?;"

convAnswer :: [SqlValue] -> Answer
convAnswer [examfile, number, choice, question, answer, isCorrect] = Answer { file  = (fromSql examfile) :: String
                                                                             , number  = (fromSql number) :: Int
                                                                             , choice = (fromSql choice) :: Int
                                                                             , question = (fromSql question) :: String
                                                                             , answer = (fromSql answer) :: String
                                                                             , isCorrect  = (fromSql isCorrect) :: Bool
}

guess :: Question -> (Int, Bool)
guess answers = (predictedChoice, predictionCorrect)
  where
    distances = map (sumLevenshtein answers) answers
    dMax = foldl max 0 distances
    dMin = foldl min dMax distances
    predictedChoice = fst $ head $ filter (\these -> snd these == dMin) $ zip [1..(length answers)] $ distances
    predictionCorrect = isCorrect $ last $ take predictedChoice answers

main :: IO ()
main = do
  conn <- connectSqlite3 "/tmp/history-regents.db"
  
  rQuestionIds <- quickQuery' conn questionQuery []
  rQuestions <- mapM (\question -> quickQuery' conn answerQuery question) rQuestionIds

  -- questions :: [Question]
  let questions = map (\rQuestion -> map convAnswer rQuestion) rQuestions
  
  -- Print the rows out
  -- putStrLn $ show $ head $ head questions

  -- Levenshtein distances
  let question = head $ questions
  putStrLn $ show $ guess $ question

  -- And disconnect from the database
  disconnect conn

