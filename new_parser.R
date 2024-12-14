library(rvest)
library(dplyr)
library(lubridate)

odds_to_pwin <- function(v){
  odds <- median(v, na.rm = TRUE)
  pwin <- as.numeric(NA)
  if (is.na(odds)){
    pwin <- as.numeric(NA)
  }
  else if (odds < 0){
    pwin <- -1*odds/(-1*odds + 100)
  }
  else if (odds > 1){
    pwin <- 100/(odds + 100)
  }
  return(pwin)
}

# https://www.vegasodds.com/ncaaf/odds/
# moneyline
# https://stackoverflow.com/questions/6364138/how-to-get-fully-computed-html-instead-of-source-html

# items (games)
items <- read_html('odds.txt') %>%
  html_element('div.seperator') %>%
  html_elements('div.item')

# Dates
dates <- read_html('odds.txt') %>%
  html_element('div.seperator') %>%
  html_elements('div.date') %>%
  html_text2
dates <- mapply(rep, dates, c(7,1,1,1,1,7,3,4,4,4,4,5))
dates <- Reduce(c, dates)

# Times
times <- items %>%
  html_element('div.time') %>%
  html_text2

# Teams
teams <- items %>%
  html_elements('div.teams') %>%
  html_elements('span') %>%
  html_text2

# Games
games <- matrix(teams, ncol = 2, byrow = TRUE)
team1 <- games[,1]
team2 <- games[,2]
games <- apply(games, 1, function(i){paste(i[1], i[2], sep = ' vs. ')})

# Moneyline
moneylines <- items %>% 
  html_elements('div.odd') %>%
  html_element('span.moneyline') %>%
  html_text2
moneylines[moneylines == '-'] <- NA_character_
moneylines <- as.numeric(moneylines)
moneylines <- split(moneylines, ceiling(seq_along(moneylines)/12))
names(moneylines) <- games
moneylines <- lapply(moneylines, matrix, nrow = 2)
moneylines <- lapply(moneylines, function(i){
  apply(i, 1, median, na.rm = TRUE)})
moneylines <- Reduce(c, moneylines)
moneylines <- matrix(moneylines, ncol = 2, byrow = TRUE)
ml1 <- moneylines[,1]
ml2 <- moneylines[,2]


# Data.frame
df <- data.frame(date = dates,
           time = times,
           #game = games,
           team1 = team1,
           team2 = team2,
           ml1 = ml1,
           ml2 = ml2)

write.csv(df, 'df.csv', row.names = FALSE)
# light editing for missing moneylines
df <- read.csv('df.csv')

df <- df %>%
  rowwise %>%
  mutate(pwin1 = odds_to_pwin(ml1),
         pwin2 = odds_to_pwin(ml2)) %>%
  ungroup %>%
  mutate(old_sum = pwin1 + pwin2) %>%
  mutate(pwin1 = pwin1 / old_sum,
         pwin2 = pwin2 / old_sum) %>%
  select(-old_sum) %>%
  mutate(rank = rank(abs(pwin1 - pwin2))) %>%
  mutate(order = 1:42) %>%
  arrange(rank) %>%
  mutate(rank = 2:43) %>% # save 1 for tiebreaker
  arrange(order)

# simulate winners for final pick list
df$pick = as.character(NA)
for (i in seq_len(nrow(df))){
  trial <- rbinom(n = 1, size = 1, prob = df$pwin1[i])
  if (trial == 1){
    df$pick[i] <- df$team1[i]
  } else if (trial == 0){
    df$pick[i] <- df$team2[i]
  }
}

write.csv(df, 'picks.csv', row.names = FALSE)

pick_from_moneylines <- function(m1, m2){
  pwin1 <- odds_to_pwin(m1)
  pwin2 <- odds_to_pwin(m2)
  old_sum <- pwin1 + pwin2
  pwin1 <- pwin1 / old_sum
  pwin2 <- pwin2 / old_sum
  trial <- rbinom(n = 1, size = 1, prob = pwin1)
  ifelse(trial == 1, 1, 2)
}
