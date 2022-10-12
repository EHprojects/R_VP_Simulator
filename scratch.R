# match_ranks Output (sorted):
# four of kind  = 17  # [1] 1 4 4 4 4
# full house    = 13  # [1] 2 2 3 3 3
# three of kind = 11  # [1] 1 1 3 3 3
# two pair      = 9   # [1] 1 2 2 2 2
# one pair      = 7   # [1] 1 1 1 2 2
# nothing       = 5   # [1] 1 1 1 1 1

# JoB Hand Ranks:
# Royal Flush > Straight Flush > Four of a Kind > Full House > Flush >
# Straight > Three of a Kind > Two Pair > Jacks or Better

# Face Card Ranks:
# J = 11
# Q = 12
# K = 13
# A = 14

options("scipen" = 10) # increase number of displayed digits in dataframe
options(pillar.sigfig = 6) # increase sig figs in tibbles

test_hand <- c("10D", "4S", "QD", "7C", "AD")


testing_func <- function() {
  
  deck <- create_deck()
  hand <- deal_hand(deck)
  #print(hand)
  hand_eval(hand)
  
}

system.time(results <- replicate(n = 2000000, testing_func()))
table(results)

saveRDS(results, file = "rslts_two_mil_deal.RDS")

rslts_table <- table(results)
rslts <- as.data.frame(rslts_table)

rslts <- rslts %>%
    arrange(Freq)

rslts %>%
  mutate(prob = Freq / 2000000) %>%
  mutate(pct = prob * 100) %>%
  mutate(cum_pct = cumsum(pct))
