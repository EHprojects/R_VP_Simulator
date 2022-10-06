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


test_hand <- c("AD", "AS", "8C", "7H", "2S")




jacks_better(test_hand)



test_vect <- c(NA,NA,TRUE,TRUE,NA)
any(test_vect, na.rm = TRUE)


match_ranks <- function(hand) {
  
  ranks <- get_ranks(hand)
  rank_match <- rep(NA, length(ranks))
  
  for (i in 1:length(ranks)) {
    
    rank_match[i] <- sum(ranks == ranks[i])
    
  }
  
  if(sum(rank_match) == 7) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

match_ranks(test_hand)

