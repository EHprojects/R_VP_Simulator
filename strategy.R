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
four_to_royal_hold <- function(hand) {
  
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


three_kind_hold <- function(hand) {
  
  if(three_kind(hand)) {
    
    ranks <- get_ranks(hand)
    rank_match <- rep(NA, length(ranks))
    
    for (i in 1:length(ranks)) {
      rank_match[i] <- sum(ranks == ranks[i])
    }
    
    cards_held <- which(rank_match == 3)
    return(cards_held)
    
  } else {
    return(FALSE)
  }
  
}


four_strght_flush_hold <- function(hand) {
  
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
  
  if(ace_hand == FALSE) {
    
    # How many ranks (i.e. cards) exist between the hand low card and hand high
    # card that are not in the hand ranks
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


two_pair_hold <- function(hand) {
  
  if(two_pair(hand)) {
    
    ranks <- get_ranks(hand)
    rank_match <- rep(NA, length(ranks))
    
    for (i in 1:length(ranks)) {
      rank_match[i] <- sum(ranks == ranks[i])
    }
    
    cards_held <- which(rank_match == 2)
    return(cards_held)
    
  } else {
    return(FALSE)
  }
  
}


high_pair_hold <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand)
  
  if(jacks_better(hand)) {
    
    rank_match <- rep(NA, length(rank_vals))
    
    for (i in 1:length(rank_vals)) {
      rank_match[i] <- sum(rank_vals == rank_vals[i])
    }
    
    cards_held <- which(rank_match == 2) # assumes any pair is high pair
    
    return(cards_held)
    
  } else {
    return(FALSE)
  }
  
}


three_to_royal_hold <- function(hand) {
  
  # Create a matrix of possible royal flush hands
  suits <- c("D", "C", "H", "S")
  royal_ranks <- c("10", "J", "Q", "K", "A")
  royal_flushes <- expand.grid(rank = royal_ranks, suit = suits)
  royal_flushes <- paste0(royal_flushes$rank, royal_flushes$suit)
  royal_flushes <- matrix(royal_flushes, nrow = 5, ncol = 4)
  
  # Check to see if 4 of the hand cards are in a possible royal flush hand
  for (i in 1:length(suits)) {
    # Return the card positions for 4 to a royal if found
    if(sum(hand %in% royal_flushes[ , i]) == 3) {
      cards_held <- which(hand %in% royal_flushes[ , i])
      return(cards_held)
    }
  }
  return(FALSE) # False is returned by default
}


four_to_flush_hold <- function(hand) {
  
  suits <- get_suits(hand)
  
  max_num <- max(table(suits))
  # max_num: 5 = flush
  # max_num: 4 = 4 to a flush
  max_suit <- names(which.max(table(suits)))
  
  if(max_num == 4) {
    cards_held <- which(suits == max_suit)
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


low_pair_hold <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand)
  rank_match <- rep(NA, length(rank_vals))
  
  for (i in 1:length(rank_vals)) {
    rank_match[i] <- sum(rank_vals == rank_vals[i])
  }
  
  if(any(rank_match == 2)) {
    cards_held <- which(rank_match == 2) # assumes only one pair
    return(cards_held)
  } else {
    return(FALSE)
  }
  
}


four_out_strght_hold <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand)
  
  # Slow? solution is to check through all combinations of 4 card inside straight
  
  all_ranks <- 2:14
  
  # rank_vect <- NULL
  # 
  # for (i in 2:10) {
  #   rank_vect <- c(rank_vect, i:(i + 3))
  # }
  
  # Vector that would be created by for loop above
  rank_vect <- c(2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7, 5, 6, 7, 8, 6, 7, 8,
                 9, 7, 8, 9, 10, 8, 9, 10, 11, 9, 10, 11, 12, 10, 11, 12, 13)
  
  rank_matrix <- matrix(rank_vect, nrow = 4, ncol = 9)
  
  for (i in 1:ncol(rank_matrix)) {
    # Return the card positions for 4 to a straight if found
    if(sum(rank_vals %in% rank_matrix[ , i]) == 4) {
      cards_held <- which(rank_vals %in% rank_matrix[ , i])
      return(cards_held)
    }
  }
  return(FALSE) # False is returned by default
  
}


two_suited_hold <- function(hand) {
  
  # 2 Suited High Cards = 2 to a Royal Flush
  
  # Create a matrix of possible royal flush hands
  royal_flushes <- c("10D", "JD", "QD", "KD", "AD", "10C", "JC", "QC", "KC",
                     "AC", "10H", "JH", "QH", "KH", "AH", "10S", "JS", "QS",
                     "KS", "AS")
  royal_flushes <- matrix(royal_flushes, nrow = 5, ncol = 4)
  
  # Check to see if 4 of the hand cards are in a possible royal flush hand
  for (i in 1:ncol(rank_matrix)) {
    # Return the card positions for 4 to a royal if found
    if(sum(hand %in% royal_flushes[ , i]) == 2) {
      cards_held <- which(hand %in% royal_flushes[ , i])
      return(cards_held)
    }
  }
  return(FALSE) # False is returned by default
  
  # rank_vals <- assign_rank_vals(hand)
  # high_cards <- 11:14
  
}


three_strght_flush_hold <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand)
  suits <- get_suits(hand)
  
  max_num <- max(table(suits))
  # max_num: 5 = flush
  # max_num: 4 = 4 to a flush
  # max_num: 3 = 3 to a flush
  max_suit <- names(which.max(table(suits)))
  
  flush_card_pos <- which(suits == max_suit)
  flush_cards <- hand[flush_card_pos]
  flush_ranks <- assign_rank_vals(flush_cards)
  
  # If there is not 3 to a flush, return false
  if(max_num != 3) {
    return(FALSE)
  }
  
  ace_hand <- FALSE
  if(14 %in% flush_ranks) { ace_hand <- TRUE }
  
  rank_vect <- NULL
  
  for (i in 1:10) {
    rank_vect <- c(rank_vect, i:(i + 4))
  }
  
  rank_matrix <- matrix(rank_vect, nrow = 5, ncol = 10)
  
  ace_high <- FALSE
  ace_low <- FALSE
  
  for (i in 1:ncol(rank_matrix)) {
    # Return the card positions for 3 to a straight if found
    if(sum(flush_ranks %in% rank_matrix[ , i]) == 3) {
      cards_held <- flush_card_pos
      return(cards_held)
    }
  }
  
  flush_ranks <- replace(flush_ranks, flush_ranks == 14, 1)
  
  for (i in 1:ncol(rank_matrix)) {
    # Return the card positions for 3 to a straight if found
    if(sum(flush_ranks %in% rank_matrix[ , i]) == 3) {
      cards_held <- flush_card_pos
      return(cards_held)
    }
  }
  
  return(FALSE) # False is returned by default

}


two_unsuited_hold <- function(hand) {
  
  # Hold lowest two unsuited high cards
  
  rank_vals <- assign_rank_vals(hand)
  
  high_rank_vals <- rank_vals[which(rank_vals >= 11)]
  
  if(length(high_rank_vals >= 2)) {
    
    hold_vals <- sort(high_rank_vals)[1:2]
    
    cards_held <- which(rank_vals == hold_vals[1])
    cards_held <- c(cards_held, which(rank_vals == hold_vals[2]))
    cards_held <- sort(cards_held)
    
    return(cards_held)
    
  } else {
    return(FALSE)
  }
  
}