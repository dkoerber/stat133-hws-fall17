-------------------------------------------------------
# Title: hw03 code
# Description: script to produce clean data for hw03
# Input: nba2017-roster.csv and nba2017-stats.csv
# Output: multiple summaries and graphs, nba2017-teams.csv
-------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/stat133-hws-fall17/hw03/code")
    
roster <- read_csv("../data/nba2017-roster.csv")
stats <- read_csv("../data/nba2017-stats.csv")

stats <- mutate(stats, missed_fg = field_goals_atts - field_goals_made, 
       missed_ft = points1_atts - points1_made,
       points = points1_made + (2 * points2_made) + (3 * points3_made),
       rebounds = off_rebounds + def_rebounds,
       efficiency = (points + rebounds + assists + steals + blocks
                     - missed_fg - missed_ft - turnovers) / games_played)

sink(file = "../output/efficiency-summary.txt")
summary(stats[ , 'efficiency'])
sink()

roster_stats <- inner_join(roster, stats)

teams <- roster_stats %>%
           group_by(team) %>%
             summarise(experience = round(sum(experience), 2),
               salary = round(sum(salary) / 1000000, 2),
               points3 = sum(points3_made),
               points2 = sum(points2_made),
               free_throws = sum(points1_made),
               points = sum(points),
               off_rebounds = sum(off_rebounds),
               def_rebounds = sum(def_rebounds),
               assists = sum(assists),
               steals = sum(steals),
               blocks = sum(blocks),
               turnovers = sum(turnovers),
               fouls = sum(fouls),
               efficiency = sum(efficiency)
             )

sink(file = "../data/teams-summary.txt")
summary(teams)
sink()

write.csv(teams, file = "../data/nba2017-teams.csv", row.names = FALSE)

pdf(file = "../images/teams_star_plot.pdf", width = 7, height = 5)
stars(teams[ ,-1], labels = teams$team)
dev.off()

pdf(file = "../images/experience-salary.pdf")
ggplot(data = teams, aes(x = experience, y = salary)) +
  geom_point() +
  geom_label(aes(label = team))
dev.off()