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
                     , nCharacters :: Maybe Int
                     , nWords :: Maybe Int
                     , sumLevenshtein :: Maybe Int
} deriving (Show)

-- All of the answers for a question
type Question = [Answer]

-- Helpers
questionQuery = "SELECT DISTINCT examfile, \"number\" FROM answer;"
answerQuery = "SELECT examfile, \"number\", choice, question, answer, isCorrect FROM answer WHERE examfile = ? AND \"number\" = ?;"

convAnswer :: [SqlValue] -> Answer
convAnswer [examfile, number, choice, question, answer, isCorrect] = Answer { file  = (fromSql examfile) :: String
                                                                             , number  = (fromSql number) :: Int
                                                                             , choice = (fromSql choice) :: Int
                                                                             , question = (fromSql question) :: String
                                                                             , answer = (fromSql answer) :: String
                                                                             , isCorrect  = (fromSql isCorrect) :: Bool
                                                                             , nCharacters = Nothing
                                                                             , nWords = Nothing
                                                                             , sumLevenshtein = Nothing
}

levenshtein :: String -> String -> Int
levenshtein s t = d!!(length s)!!(length t) 
  where
    d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

-- Features
----------------------------------------------------------------------------------

-- Sum of Levenshtein distances
getSumLevenshtein :: Question -> Answer -> Int
getSumLevenshtein answers thisAnswer = sum $ map (levenshtein $ answer thisAnswer) $ map answer answers

-- Count by word
wordCounts :: String -> M.Map String Int
wordCounts text = foldr (\a b -> M.insertWith (+) a 1 b) M.empty $ words text

-- Length of answer
getNCharacters = length

getNWords :: String -> Int
getNWords text = length $ filter (== ' ') text

-- Contains "and"
getContainsAnd :: String -> Bool
getContainsAnd text = (> 0) $ length $ filter (== "and") $ words text

-- Question and answer contain a same word
commonWords :: String -> String -> S.Set String
commonWords a b = S.difference (S.intersection (w a) (w b)) stopWords
  where
    w t = S.fromList $ words t
    stopWords = S.fromList ["in", "the", "of", "to", "and", "The", "are", "from", "is", "In", "be", "all", "an", "are", "as", "at", "In", "its", "was", "were", "than","that",]

getContainsCommonWord :: String -> String -> Bool
getContainsCommonWord a b = (> 0) $ S.length $ commonWords a b

-- Qualitative answer about a graph question
getIsQualitativeAnswerAboutGraph :: Answer -> Bool
getIsQualitativeAnswerAboutGraph a = graph && qualitative
  where
    graph = ((> 0) $ length $ filter (== "graph") $ words $ question a)
    qualitative = (== 0) $ S.length $ S.intersection (S.fromList "1234567890") (S.fromList $ answer a)


----------------------------------------------------------------------------------
-- Create a table with the features.
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
  --let results = map guess questions
  --putStrLn $ show $ results
  --putStrLn $ show $ score results

  --Word counts
  --putStrLn $ show $ foldl (M.unionWith (+)) M.empty $ map wordCount $ map answer $ head questions
  
  --putStrLn $ show $ map (length . answer) $ head questions

  -- List the common words to find stopwords
  let w = map (map (\a -> commonWords (question a) (answer a))) questions
  putStrLn $ show $ foldr S.union S.empty $ concat w

  -- And disconnect from the database
  disconnect conn

