---
title: "Exercise Sheet 2"
fontsize: 11pt
header-includes: \usepackage[german]{babel}
output:
  html_document: default
  pdf_document:
    highlight: tango
fig_caption: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, # -> Should code chunks be displayed in the rendered document?
                      eval = TRUE, # -> Should R Code Chunks be executed?
                      warning = FALSE, # -> Warnings should only be set to FALSE at the end of rendering.
                      message = FALSE) # -> Notes should only be set to FALSE at the end of rendering.
```

This exercise sheet is an [R Markdown](https://rmarkdown.rstudio.com/) file. To generate an HTML file from it, use the __Knit__ button in RStudio.

The _European Soccer Database_ contains data on more than 25.000 national football matches from the best European leagues. The aim of this exercise is to present interesting relationships in R using exploratory data analysis and visualization.

First you need to access some tables in the database. To do so, [download the database](https://1drv.ms/u/s!AlrZt1pKHg25gch_i-b1mAbOtWU44Q?e=AMhg1B) and place it in the same folder as this .Rmd file. You can then use the `RSQLite::dbConnect()` function to connect to the database. To access a particular database table and convert it to a `data.frame`, you can use the `tbl_df(dbGetQuery(connection, 'SELECT * FROM table_xyz'))` command as displayed below.



```{r}
# Load libraries (they need to be installed on the first run via install.packages)
# You do not need to use these libraries, though
library(RSQLite)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)

# connect to database
con <- dbConnect(SQLite(), dbname = "EuropeanSoccer.sqlite")

# table queries
match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
```

_Example code for a visualization: Below, you can find a code chunk that contains code to create a basic scatterplot._
```{r}
# Example visualization...
match %>%
  ggplot(aes(x = home_team_goal, y = away_team_goal)) +
  geom_point(alpha = 1/50, size = 10)
```

1. The first leagues of Spain, England, Germany and Italy are considered the four most attractive football leagues in Europe. In which of the four leagues were the most or the fewest goals scored per game on average? 

```{r}
# Solution for task 1...
match_top4 <- league %>%
  filter(name %in% c("Spain LIGA BBVA", 
                     "England Premier League",  
                     "Germany 1. Bundesliga", 
                     "Italy Serie A")) %>%
  select(league_id = id, league_name = name) %>%
  inner_join(match, by = "league_id")

match_top4 %>%
  group_by(league_name) %>%
  filter(!is.na(home_team_goal) | !is.na(away_team_goal)) %>%
  summarize(avg_match_goals = mean(home_team_goal + away_team_goal)) %>%
  arrange(-avg_match_goals)
```

2. In this task, we refer again to the four most attractive European leagues from Task 1. Compare the average and the standard deviation of goals scored per match between the four most attractive European leagues on one side and the remaining leagues on the other side.

```{r}
# Solution for task 2...
league %>%
  mutate(name = fct_collapse(name, 
                             top4 = c("Spain LIGA BBVA", 
                                      "England Premier League",  
                                      "Germany 1. Bundesliga", 
                                      "Italy Serie A"))) %>%
  mutate(name = fct_other(name, keep = "top4", other_level = "rest")) %>%
  select(league_id = id, league_name = name) %>%
  inner_join(match, by = "league_id") %>%
  group_by(league_name) %>%
  mutate(match_goals = home_team_goal + away_team_goal) %>%
  summarise(
    avg_match_goals = mean(match_goals),
    sd_match_goals = sd(match_goals)
  ) %>% knitr::kable()
```

3. Is there really a home advantage? Use a box plot to show the number of goals scored by home and away teams.

```{r}
# Solution for task 3...
match %>%
  pivot_longer(c(home_team_goal, away_team_goal), names_to = "side", values_to = "goals_scored") %>%
  ggplot(aes(x = side, y = goals_scored)) + geom_boxplot() +
  stat_summary(geom = "point", fun = mean, pch = 23)
```

4. _"All soccer players are fair-weather players!"_ Check the assertion with a line chart: Do on average more goals fall per game in the summer months than in the rest of the year?


```{r}
# Solution for task 4...
match_top4 %>%
  mutate(match_month = month(as_date(date), label = T)) %>%
  group_by(match_month) %>%
  filter(!is.na(home_team_goal) | !is.na(away_team_goal)) %>%
  summarize(avg_match_goals = mean(home_team_goal + away_team_goal)) %>%
  ggplot(aes(x = match_month, y = avg_match_goals, group = 1)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(0,4))
```

5. Use an estimated density function curve AND a QQ-Plot to check whether the `home_team_possession` variable is (approximately) normally distributed.

```{r}
# Solution for task 5...
match %>%
  ggplot(aes(x = home_team_possession)) +
  geom_density()

qqnorm(match$home_team_possession)
qqline(match$home_team_possession)
```


------
Dataset:

- https://1drv.ms/u/s!AlrZt1pKHg25gch_i-b1mAbOtWU44Q?e=AMhg1B  
(For database schema and explanation of variables, see: https://www.kaggle.com/hugomathien/soccer)