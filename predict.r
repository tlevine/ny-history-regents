library(sqldf)
library(reshape2)
library(ggplot2)

a <- (function() {
  a.wide <- sqldf('SELECT * FROM answer;', dbname = '/tmp/history-regents.db')

  # Normalize
  features <- c('sumLevenshtein', 'nCharacters', 'nWords', 'containsCommonWord', 'isQualitativeAnswerAboutGraph')
  a.wide[features] <- lapply(a.wide[features], function(f) {
    (f - mean(f)) / sd(f)
  })

  a.long <- melt(a.wide, c('examfile', 'number', 'choice', 'isCorrect'), variable.name = 'feature')
  a.long$isCorrect <- as.logical(a.long$isCorrect)
  a.long$choice <- factor(a.long$choice)
  a.long$basename <- sapply(a.long$examfile, function(fn) {rev(strsplit(fn, '/')[[1]])[1]})

  a.long
})()

p.features <- ggplot(a) + aes(x = value, fill = factor(isCorrect)) +
  geom_histogram(position = 'dodge') + facet_grid(. ~ feature, scales = 'free')

p.questions <- dlply(a, c('examfile', 'number'), function(df){
  ggplot(df) + aes(x = choice, y = value, color = isCorrect) +
    scale_color_manual(values=c("#000000", "#FF0000")) +
    facet_grid(. ~ feature, scale = 'free', space = 'free') + geom_point() +
    labs(title = paste(df[1,c('basename', 'number')], collapse = ', question '))
})

pdf('questions.pdf')
print(p.questions)
dev.off()
