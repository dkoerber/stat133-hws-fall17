# -------------------------------------------------------
# Title: clean-data-script
# Description: script to produce clean data for hw04
# Input: rawscores.csv
# Output: multiple summaries and cleanscores.csv
# Author: Doug Koerber
# -------------------------------------------------------

# load packages and source functions
library(readr)
library(dplyr)
source('functions.R')

# load data, define a border to be used in text output, set size of data
scores <- as.data.frame(read.csv('../data/rawdata/rawscores.csv'))
border <- "-------------------------------------------------------------------"
n_rows <- nrow(scores)
n_cols <- ncol(scores)

# sink structure of the data and summary stats for all variables
sink('../output/summary-rawscores.txt')
  cat(paste0("structure of the scores data.frame\n", border, "\n"))
  str(scores)
  for (i in 1:n_cols) {
    col <- names(scores)[i]
    cat(paste0("\n", col, "\n", border, "\n"))
    stats <- summary_stats(scores[ , i])
    print_stats(stats)
  }
sink()

# for loop to replace NA values with 0
for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    if (is.na(scores[i, j])) {
      scores[i, j] = 0
    }
  }
}

# rescaling all quiz scores to be in percent
scores[ , 'QZ1'] <- rescale100(scores[ , 'QZ1'], xmin = 0, xmax = 12)
scores[ , 'QZ2'] <- rescale100(scores[ , 'QZ2'], xmin = 0, xmax = 18)
scores[ , 'QZ3'] <- rescale100(scores[ , 'QZ3'], xmin = 0, xmax = 20)
scores[ , 'QZ4'] <- rescale100(scores[ , 'QZ4'], xmin = 0, xmax = 20)

# rescaling all test scores to be in percent
scores <- scores %>%
            mutate(Test1 = rescale100(scores[ , 'EX1'], xmin = 0, xmax = 80))
scores <- scores %>%
            mutate(Test2 = rescale100(scores[ , 'EX2'], xmin = 0, xmax = 90))

# calculating the total homework score while dropping the lowest grade
hws <- c('HW1', 'HW2', 'HW3', 'HW4', 'HW5', 'HW6', 'HW7', 'HW8', 'HW9')
scores <- mutate(scores, Homework = 0)
for (i in 1:n_rows) {
  grades = as.numeric(scores[i, hws])
  scores[i, 'Homework'] = score_homework(grades, drop = TRUE)
}

# calculating the total quiz score while dropping the lowest grade
qzs <- c('QZ1', 'QZ2', 'QZ3', 'QZ4')
scores <- mutate(scores, Quiz = 0)
for (i in 1:n_rows) {
  grades = as.numeric(scores[i, qzs])
  scores[i, 'Quiz'] = score_quiz(grades, drop = TRUE)
}

# calculating the total lab attendance score
lab <- c('ATT')
scores <- mutate(scores, Lab = 0)
for (i in 1:n_rows) {
  grades = as.numeric(scores[i, lab])
  scores[i, 'Lab'] = score_lab(grades)
}

# add overall grade to the data
scores <- scores %>%
            mutate(Overall = (0.1 * Lab) +
                             (0.3 * Homework) +
                             (0.15 * Quiz) +
                             (0.2 * Test1) +
                             (0.25 * Test2)
                   )

# convert overall grade from numeric to character
scores <- mutate(scores, Grade = 'n')
for (i in 1:n_rows) {
  grade = scores[i, 'Overall']
  if (grade >= 95) {
    scores[i, 'Grade'] = 'A+'
  } else if (grade >= 90) {
      scores[i, 'Grade'] = 'A'
  } else if (grade >= 88) {
      scores[i, 'Grade'] = 'A-'
  } else if (grade >= 86) {
      scores[i, 'Grade'] = 'B+'
  } else if (grade >= 82) {
      scores[i, 'Grade'] = 'B'
  } else if (grade >= 79.5) {
      scores[i, 'Grade'] = 'B-'
  } else if (grade >= 77.5) {
      scores[i, 'Grade'] = 'C+'
  } else if (grade >= 70) {
      scores[i, 'Grade'] = 'C'
  } else if (grade >= 60) {
      scores[i, 'Grade'] = 'C-'
  } else if (grade >= 50) {
      scores[i, 'Grade'] = 'D'
  } else {
      scores[i, 'Grade'] = 'F'
  }
}

# for loop to sink summary stats for each of the listed assignments
summaries <- c('Lab', 'Homework', 'Quiz', 'Test1', 'Test2', 'Overall')
for (i in 1:length(summaries)) {
  sink(paste0('../output/', summaries[i], '-stats.txt'))
    cat(paste0(summaries[i], "\n", border, "\n"))
    stats <- summary_stats(scores[ , i])
    print_stats(stats)
  sink()
}

# sink the structure of the cleaned data
sink('../output/summary-cleanscores.txt')
  cat(paste0("clean scores\n", border, "\n"))
  str(scores)
sink()

# export the cleaned data
write.csv(scores, '../data/cleandata/cleanscores.csv')