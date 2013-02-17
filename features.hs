-- Extract features
import System.IO
import qualified Data.Set as S
import qualified Data.Map as M
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
type Question = M.Map Int Answer

levenshtein :: String -> String -> Int
levenshtein s t = d!!(length s)!!(length t) 
  where
    d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

sumLevenshtein :: Question -> Answer -> Int
sumLevenshtein answers thisAnswer = sum $ map (levenshtein $ answer thisAnswer) $ map (\these -> answer $ snd these) $ M.toList answers
  where
    thisAnswerStr = answer thisAnswer
    answersStr = map answer $ map (\a -> snd a) $ M.toList answers

--isAbsolute :: String -> Bool
--isAbsolute word = S.member word absolutes
--  where
--    absolutes = S.fromList ["all", "every", "never"]

--nAbsolutes :: String -> Int
--nAbsolutes question = length $ filter $ map isAbsolute $ words question

a = [ "increasing factory employment opportunities placing blame only on civilian leaders", "encouraging immigration from Africa forcing nations to pay for war damages", "focusing attention on artistic contributions returning conquered territories to their", "bringing an end to legalized racial segregation holding individuals accountable for their war"]

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
  let question = last $ take 10 questions
  let questionAnswers = M.fromList $ map (\a -> (choice a, a)) question

  putStrLn $ show $ questionAnswers

  let choices = unzip $ M.toList $ M.map (sumLevenshtein questionAnswers) questionAnswers
  let dMax = foldl max 0 $ snd choices
  let dMin = foldl min dMax $ snd choices
  putStrLn $ show dMin
  putStrLn $ show choices
  putStrLn $ show $ filter (\c -> snd c == dMin) $ M.toList $ M.map (sumLevenshtein questionAnswers) questionAnswers 

  -- And disconnect from the database
  disconnect conn

