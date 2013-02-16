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
