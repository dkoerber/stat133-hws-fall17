hw03-doug-koerber
================
Doug Koerber

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

### Basic Rankings

#### Salary

``` r
teams <- read_csv("../data/nba2017-teams.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   team = col_character(),
    ##   experience = col_integer(),
    ##   salary = col_double(),
    ##   points3 = col_integer(),
    ##   points2 = col_integer(),
    ##   free_throws = col_integer(),
    ##   points = col_integer(),
    ##   off_rebounds = col_integer(),
    ##   def_rebounds = col_integer(),
    ##   assists = col_integer(),
    ##   steals = col_integer(),
    ##   blocks = col_integer(),
    ##   turnovers = col_integer(),
    ##   fouls = col_integer(),
    ##   efficiency = col_double()
    ## )

``` r
ggplot(data = teams, aes(x = reorder(team, salary), y = salary)) +
  geom_bar(stat = "identity", fill = "gray60") +
  geom_hline(aes(yintercept = mean(salary)), col = rgb(50, 172, 150, maxColorValue = 255), size = 2, alpha = 0.7) +
  coord_flip() +
  labs(x = "Team", y = "Salary (in millions)", title = "NBA Teams Ranked by Total Salary")
```

![](hw03-doug-koerber_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

#### Total Points

``` r
ggplot(data = teams, aes(x = reorder(team, points), y = points)) +
  geom_bar(stat = "identity", fill = "gray60") +
  geom_hline(aes(yintercept = mean(points)), col = rgb(50, 172, 150, maxColorValue = 255), size = 2, alpha = 0.7) +
  coord_flip() +
  labs(x = "Team", y = "Total Points", title = "NBA Teams Ranked by Total Points")
```

![](hw03-doug-koerber_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

#### Efficiency

``` r
ggplot(data = teams, aes(x = reorder(team, efficiency), y = efficiency)) +
  geom_bar(stat = "identity", fill = "gray60") +
  geom_hline(aes(yintercept = mean(efficiency)), col = rgb(50, 172, 150, maxColorValue = 255), size = 2, alpha = 0.7) +
  coord_flip() +
  labs(x = "Team", y = "Total Efficiency", title = "NBA Teams Ranked by Total Points")
```

![](hw03-doug-koerber_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
pc1 <- data.frame(teams$points3, teams$points2, teams$free_throws, teams$off_rebounds, teams$def_rebounds, teams$assists, teams$steals, teams$blocks, teams$turnovers, teams$fouls)
pca <- prcomp(pc1, scale. = TRUE)
names(pca)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
eigs <- data.frame(eigenvalue = pca$sdev^2, prop = pca$sdev^2 / sum(pca$sdev^2))
eigs <- round(mutate(eigs, cumprop = cumsum(prop)), 4)
```

``` r
pca_plot <- data.frame(teams$team, pca$x)
ggplot(data = pca_plot, aes(x = PC1, y = PC2)) +
  geom_text(aes(label = teams$team)) +
  geom_vline(xintercept = 0, col = "gray50") +
  geom_hline(yintercept = 0, col = "gray50") +
  labs(title = "PCA Plot: PC1 and PC2")
```

![](hw03-doug-koerber_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

### Scaled PC1 Scores

``` r
pca_plot <- mutate(pca_plot, PC1_scaled = 100 * ((PC1 - min(PC1)) / (max(PC1) - min(PC1))))
ggplot(data = pca_plot, aes(x = reorder(teams.team, PC1_scaled), y = PC1_scaled)) +
  geom_bar(stat = "identity", fill = "gray60") +
  coord_flip() +
  labs(x = "Team", y = "PC1 (Scaled to 100)", title = "NBA Teams Ranked by Scaled PC1")
```

![](hw03-doug-koerber_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)
