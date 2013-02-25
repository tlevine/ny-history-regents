#!/usr/bin/env Rscript
library(sqldf)
library(reshape2)
library(ggplot2)

a.wide <- sqldf('SELECT * FROM answer;', dbname = '/tmp/history-regents.db')
a.wide.training <- subset(a.wide, examfile != 'downloads/www.nysedregents.org/USHistoryGov/Archive/20100127exam.pdf')
a.wide.test <- subset(a.wide, examfile == 'downloads/www.nysedregents.org/USHistoryGov/Archive/20100127exam.pdf')

m <- glm(
    isCorrect ~ nWords * containsCommonWord * isQualitativeAnswerAboutGraph,
    family = 'binomial',
    data = a.wide.training
)
m.predictions <- predict(m, newdata = a.wide.test, type = 'response')

a <- function(normalize = F) {
  # Normalize
  if (normalize) {
    features <- c('sumLevenshtein', 'nCharacters', 'nWords', 'containsCommonWord', 'isQualitativeAnswerAboutGraph')
    a.wide[features] <- lapply(a.wide[features], function(f) {
      (f - mean(f)) / sd(f)
    })
  }

  a.long <- melt(a.wide, c('examfile', 'number', 'choice', 'isCorrect'), variable.name = 'feature')
  a.long$isCorrect <- as.logical(a.long$isCorrect)
  a.long$choice <- factor(a.long$choice)
  a.long$basename <- sapply(a.long$examfile, function(fn) {rev(strsplit(fn, '/')[[1]])[1]})

  a.long
}

main <- function() {
  p.features <- ggplot(a(normalize = F)) + aes(x = value, fill = factor(isCorrect)) +
    geom_histogram(position = 'dodge') + facet_grid(. ~ feature, scales = 'free')
  
  pdf('features.pdf', width = 11, height = 8.5)
  print(p.features)
  dev.off()
  
  p.questions <- dlply(a(normalize = T), c('examfile', 'number'), function(df){
    ggplot(df) + aes(x = choice, y = value, color = isCorrect) +
      scale_color_manual(values=c("#000000", "#FF0000")) +
      facet_grid(. ~ feature, scale = 'free', space = 'free') + geom_point() +
      labs(title = paste(df[1,c('basename', 'number')], collapse = ', question '))
  })
  
  pdf('questions.pdf')
  print(p.questions)
  dev.off()
}
