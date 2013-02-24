library(sqldf)
answer <- sqldf('select * from answer;', dbname = '/tmp/history-regents.db')
