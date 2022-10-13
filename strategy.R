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


four_to_royal <- function(hand) {
  
  # four to "royal straight"
  
  # Face Card Ranks:
  # J = 11
  # Q = 12
  # K = 13
  # A = 14
  
  
  
  rank_vals <- as.numeric(assign_rank_vals(hand))
  print(rank_vals)
  royal_ranks <- c(10, 11, 12, 13, 14)
  
  # j <- 0
  # 
  # for(i in rank_vals) {
  #   
  #   print(paste0("i: ", i))
  #   print(paste0("j: ", j))
  #   
  #   if(i %in% royal_ranks) {
  #     
  #     royal_ranks <- royal_ranks[ !royal_ranks == i ]
  #     j <- j + 1
  #     
  #   }
  #   
  # }
  # 
  # print(j)
  # 
  
  suits <- get_suits(hand)
  print(suits)
  
  suit_match <- rep(NA, length(suits))
  
  for (i in 1:length(suits)) {
    
    suit_match[i] <- sum(suits == suits[i])
    
  }
  
  print(suit_match)
  
  print(sum(suit_match))
  
  # cards that match in suit and in "royal straight"
  # Given that it is not a Royal Flush, then:
  # if all cards are the same suit, one will not be in the "royal straight"
  # if all cards are in the royal straight, one must be a different suit
  
  cards_in_royal <- NULL
  
  # If all suits are the same (i.e. a flush)
  if(sum(suit_match) == 25) {
    
    for (i in 1:length(rank_vals)) {
      
      if(rank_vals[i] %in% royal_ranks) {
        
        cards_in_royal <- c(cards_in_royal, i)
        
      }
      
    }
    
    if(cards_in_royal == 4) {
      return(cards_in_royal)
    } else {
      return(FALSE)
    }
    
  }
  
  print(cards_in_royal)
  
 
  
}

four_to_royal(tst_cards)

tst_cards <- c("10S", "2S", "AS", "QS", "KS")

tst_cmbn <- combn(tst_hand, 4)
tst_cmbn[ , 1]

royal_ranks <- c(10, 11, 12, 13, 14)
tst_hand <- c(10, 10, 11, 12, 13)

tst_hand %in% royal_ranks
tst_hand == royal_ranks

setdiff(royal_ranks, tst_hand)
length(setdiff(royal_ranks, tst_hand))

royal_ranks[ !royal_ranks == tst_card  ]