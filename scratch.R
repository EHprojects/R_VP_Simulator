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


test_hand <- c("10D", "AS", "6C", "7H", "7S")


jacks_better <- function(hand) {
  
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

jacks_better(test_hand)




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

