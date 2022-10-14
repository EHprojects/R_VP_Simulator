# 1. Four of a kind, straight flush, royal flush
# 2. 4 to a royal flush
# 3. Three of a kind, straight, flush, full house
# 4. 4 to a straight flush
# 5. Two pair
# 6. High pair
# 7. 3 to a royal flush
# 8. 4 to a flush
# 9. Low pair
# 10. 4 to an outside straight
# 11. 2 suited high cards
# 12. 3 to a straight flush
# 13. 2 unsuited high cards (if  > 2, pick the low 2)
# 14. Suited 10/J, 10/Q, or 10/K
# 15. One high card
# 16. Discard everything

four_kind_hold <- function(hand) {
  
  ranks <- get_ranks(hand)
  # print(ranks)
  
  rank_match <- rep(NA, length(ranks))
  
  for (i in 1:length(ranks)) {
    rank_match[i] <- sum(ranks == ranks[i])
  }
  
  # print(rank_match)
  
  cards_held <- NULL
  j <- 1
  
  for (i in 1:length(rank_match)) {
    if(rank_match[i] == 4) {
      cards_held[j] <- i
      j <- j + 1
    }
  }
  
  # print(cards_held)
  return(cards_held)
  
}

tst_hand <- c("2S", "5D", "2H", "2C", "2D")

four_kind_hold(tst_hand)


straight_flush_hold <- function(hand) {
  
  cards_held <- c(1, 2, 3, 4, 5)
  return(cards_held)
  
}


royal_hold <- function(hand) {
  
  cards_held <- c(1, 2, 3, 4, 5)
  return(cards_held)
  
}


# Test for a 4 to a Royal Flush and return the appropriate hold cards if found
# else return FALSE
four_to_royal <- function(hand) {
  
  # Create a matrix of possible royal flush hands
  suits <- c("D", "C", "H", "S")
  royal_ranks <- c("10", "J", "Q", "K", "A")
  royal_flushes <- expand.grid(rank = royal_ranks, suit = suits)
  royal_flushes <- paste0(royal_flushes$rank, royal_flushes$suit)
  royal_flushes <- matrix(royal_flushes, nrow = 5, ncol = 4)
  
  # Check to see if 4 of the hand cards are in a possible royal flush hand
  for (i in 1:length(suits)) {
    # Return the 4 cards to the royal if found
    if(sum(hand %in% royal_flushes[ , i]) == 4) {
      cards_held <- which(hand %in% royal_flushes[ , i])
      return(cards_held)
    }
  }
  return(FALSE) # False is returned by default
}

four_to_royal(tst_cards)

tst_cards <- c("10H", "JC", "AS", "QS", "KS")
