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


two_unsuited_hold <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand)
  rank_vals_sort <- sort(rank_vals)
  print(rank_vals_sort)
  high_ranks <- 11:14
  
  high_cards <- which(rank_vals_sort %in% high_ranks)
  hihg_rank_vals <- rank_vals_sort[high_cards]
  
  print(high_cards)

}

two_unsuited_hold(tst_hand)


tst_hand <- c("3H", "4H", "KC", "QS", "AH")

tst_ranks <- c(3, 4, 5, 14, 7)
tst_rank_match <- c(1, 1, 2, 2, 1)



rank_vect <- NULL

for (i in 1:10) {
  rank_vect <- c(rank_vect, i:(i + 4))
}

rank_matrix <- matrix(rank_vect, nrow = 5, ncol = 10)


