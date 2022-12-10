# simulate college football bowl game outcomes based on vegas odds, and assign confidence ranks

library(data.table)
library(dplyr)

# Feeling lucky...
set.seed(pi^exp(1))

# https://www.vegasodds.com/ncaaf/odds/
# moneyline
# copy-paste into LibreOffice Calc from Firefox table

odds <- fread('odds.csv', header = FALSE, na.strings = c('-', 'Vegas', 'ONLINE'))
row_indices <- lapply(1:42, function(i){ (i*3-2):(i*3)})

odds_to_pwin <- function(v){
  odds <- mean(v, na.rm = TRUE)
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

final <- lapply(row_indices, function(i){
  df <- odds[i,]
  game <- paste(df$V1[2], df$V1[3], sep = ' vs. ')
  team1 <- df$V1[2]
  team2 <- df$V1[3]
  gametime <- df$V1[1]
  pwin1 <- odds_to_pwin(unname(sapply(df[2,2:7], c)))
  pwin2 <- odds_to_pwin(unname(sapply(df[3,2:7], c)))
  # if odds are NA for a team, use 1 - the other team
  if (is.na(pwin1) && !is.na(pwin2)) {pwin1 <- 1 - pwin2}
  if (is.na(pwin2) && !is.na(pwin1)) {pwin2 <- 1 - pwin2}
  # normalize the odds to add up to 1
  old_sum <- (pwin1 + pwin2)
  pwin1 <- pwin1 / old_sum
  pwin2 <- pwin2 / old_sum
  return(data.frame('gametime' = gametime, 'game' = game, 'team1' = team1, 'team2' = team2, 'pwin1' = pwin1, 'pwin2' = pwin2))
}) %>% rbindlist

# higher ranks for higher differences in probability
final$rank = rank(abs(final$pwin1 - final$pwin2))

# simulate winners for final pick list
final$pick = as.character(NA)
for (i in seq_len(nrow(final))){
  trial <- rbinom(n = 1, size = 1, prob = final$pwin1[i])
  if (trial == 1){
    final$pick[i] <- final$team1[i]
  } else if (trial == 0){
    final$pick[i] <- final$team2[i]
  }
}

final %>% arrange(rank)
