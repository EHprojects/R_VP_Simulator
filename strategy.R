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


# To Do:  Clean up four_strght_flush()
#         
#         


# Check for a four of kind and return the appropriate hold cards if true
four_kind_hold <- function(hand) {
  
  if(four_kind(hand)) {
    
    ranks <- get_ranks(hand)
    rank_match <- rep(NA, length(ranks))
    
    for (i in 1:length(ranks)) {
      rank_match[i] <- sum(ranks == ranks[i])
    }
    
    cards_held <- which(rank_match == 4)
    return(cards_held)
    
  } else {
    return(FALSE)
  }
}


# Check for a Straight Flush and return appropriate hold cards if true
straight_flush_hold <- function(hand) {
  
  if(straight_flush(hand)) {
    cards_held <- c(1, 2, 3, 4, 5)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


# Check for Royal Flush and return appropriate hold cards if true
royal_hold <- function(hand) {
  
  if(royal_flush(hand)) {
    cards_held <- c(1, 2, 3, 4, 5)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


# Test for a 4 to a Royal Flush and return the appropriate hold cards if true
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
    # Return the card positions for 4 to a royal if found
    if(sum(hand %in% royal_flushes[ , i]) == 4) {
      cards_held <- which(hand %in% royal_flushes[ , i])
      return(cards_held)
    }
  }
  return(FALSE) # False is returned by default
}


straight_hold <- function(hand) {
  
  if(straight(hand)) {
    cards_held <- c(1, 2, 3, 4, 5)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


full_house_hold <- function(hand) {
  
  if(full_house(hand)) {
    cards_held <- c(1, 2, 3, 4, 5)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


flush_hold <- function(hand) {
  
  if(flush_hand(hand)) {
    cards_held <- c(1, 2, 3, 4, 5)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


four_strght_flush <- function(hand) {
  
  # Runs after flush check, i.e. cannot be a flush
  # This function relies on not being passed a flush hand - needs updating
  
  ranks <- assign_rank_vals(hand)
  suits <- get_suits(hand)
  # print(ranks)
  # print(suits)
  
  max_num <- max(table(suits))
  # max_num: 5 = flush
  # max_num: 4 = 4 to a flush
  max_suit <- names(which.max(table(suits)))
  
  # print(max_num)
  # print(max_suit)
  
  four_flush <- FALSE
  
  # If there is not four to a flush, return false, else continue
  if(max_num >= 4) {
    four_flush <- TRUE
  } else {
    return(FALSE)
  }
  
  flush_card_pos <- which(suits == max_suit)
  # print(flush_card_pos)
  
  flush_cards <- hand[flush_card_pos]
  # print(flush_cards)
  
  flush_ranks <- assign_rank_vals(flush_cards)
  # print(flush_ranks)
  
  ace_hand <- FALSE
  if(14 %in% flush_ranks) { ace_hand <- TRUE }
  
  if(ace_hand != TRUE) {
    
    strt_gaps <- length((setdiff(min(flush_ranks):max(flush_ranks), flush_ranks)))
    # strt_gaps: 0 = straight
    # strt_gaps: 1 = 4 to a straight
    # print(strt_gaps)
    
    if(strt_gaps <= 1) {
      return(flush_card_pos)
    } else {
      return(FALSE)
    }
    
  }
  
  if(ace_hand == TRUE) {
    
    ace_high <- FALSE
    ace_low <- FALSE
    
    strt_gaps <- length((setdiff(min(flush_ranks):max(flush_ranks), flush_ranks)))
    # strt_gaps: 0 = straight
    # strt_gaps: 1 = 4 to a straight
    # print(strt_gaps)
    
    if(strt_gaps <= 1) {
      ace_high <- TRUE # return here instead?
    }
    
    flush_ranks <- replace(flush_ranks, flush_ranks == 14, 1)
    
    strt_gaps <- length((setdiff(min(flush_ranks):max(flush_ranks), flush_ranks)))
    # strt_gaps: 0 = straight
    # strt_gaps: 1 = 4 to a straight
    # print(strt_gaps)
    
    if(strt_gaps <= 1) {
      ace_low <- TRUE # return here instead?
    }
    
    if(ace_high | ace_low) {
      return(flush_card_pos)
    } else {
      return(FALSE)
    }
    
  }
  
}
