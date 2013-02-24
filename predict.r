library(sqldf)
library(reshape2)
library(ggplot2)

a <- (function() {
    a.wide <- sqldf('select * from answer;', dbname = '/tmp/history-regents.db')

    a.long <- melt(a.wide, c('examfile', 'number', 'choice', 'isCorrect'), variable.name = 'feature')
    a.long$isCorrect <- as.logical(a.long$isCorrect)

    a.long
})()

p.features <- ggplot(a) + aes(x = value, fill = factor(isCorrect)) +
  geom_histogram(position = 'dodge') + facet_grid(. ~ feature, scales = 'free')

p.questions <- dlply(a, c('examfile', 'number'), function(df){
  ggplot(df) + aes(x = choice, y = value, color = isCorrect) +
    facet_grid(. ~ feature, scale = 'free') + geom_point()
})

pdf('questions.pdf')
print(p.questions)
dev.off()
