# match_ranks Output (sorted):
# four of kind  = 17  # [1] 1 4 4 4 4
# full house    = 13  # [1] 2 2 3 3 3
# three of kind = 11  # [1] 1 1 3 3 3
# two pair      = 9   # [1] 1 2 2 2 2
# one pair      = 7   # [1] 1 1 1 2 2
# nothing       = 5   # [1] 1 1 1 1 1

# match_suits Output (sorted):
# flush         = 25  # [1] 5 5 5 5 5

# JoB Hand Ranks:
# Royal Flush > Straight Flush > Four of a Kind > Full House > Flush >
# Straight > Three of a Kind > Two Pair > Jacks or Better

# Face Card Ranks:
# J = 11
# Q = 12
# K = 13
# A = 14

# To Do: Abstract out rank matching

options("scipen" = 10) # increase number of displayed digits in dataframe
options(pillar.sigfig = 6) # increase sig figs in tibbles



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


three_kind_hold <- function(hand) {
  
  
  
}



tst_hand <- c("3H", "2H", "4H", "QS", "7H") # 1 gap

tst_suit_match <- c(4, 4, 1, 4, 4)
tst_suits <- c("H", "H", "C", "H", "H")
tst_ranks <- c(10,  9,  4,  7,  8)

combn(tst_ranks, 4)
