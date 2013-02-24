CREATE TABLE IF NOT EXISTS question (
  examfile TEXT NOT NULL,
  "number" INTEGER NOT NULL,
  question TEXT NOT NULL, 
  answer1 TEXT NOT NULL,
  answer2 TEXT NOT NULL,
  answer3 TEXT NOT NULL,
  answer4 TEXT NOT NULL,
  correct_choice INTEGER,
  UNIQUE(examfile, "number")
);

CREATE TABLE IF NOT EXISTS answer_feature (
  examfile TEXT NOT NULL,
  "number" INTEGER NOT NULL,
  choice INTEGER NOT NULL,
  sumLevenshtein INTEGER NOT NULL,
  nCharacters INTEGER NOT NULL,
  nWords INTEGER NOT NULL,
  containsCommonWord INTEGER NOT NULL,
  isQualitativeAnswerAboutGraph INTEGER NOT NULL,
  UNIQUE(examfile, "number", choice)
);

CREATE VIEW IF NOT EXISTS question_notnull AS
SELECT * FROM question WHERE correct_choice NOT NULL;

CREATE VIEW IF NOT EXISTS answer_info AS
SELECT examfile, "number", question, answer1 AS 'answer', 1 AS 'choice', correct_choice = 1 AS 'isCorrect' FROM question_notnull
UNION
SELECT examfile, "number", question, answer2 AS 'answer', 2 AS 'choice', correct_choice = 2 AS 'isCorrect' FROM question_notnull
UNION
SELECT examfile, "number", question, answer3 AS 'answer', 3 AS 'choice', correct_choice = 3 AS 'isCorrect' FROM question_notnull
UNION
SELECT examfile, "number", question, answer4 AS 'answer', 4 AS 'choice', correct_choice = 4 AS 'isCorrect' FROM question_notnull;

CREATE VIEW IF NOT EXISTS answer AS
SELECT * FROM answer_feature
JOIN answer_info ON
answer_feature.examfile = answer_info.examfile AND
answer_feature."number" = answer_info."number" AND
answer_feature.choice = answer_info.choice;
