library(tidyverse)
library(stringr)

create_deck <- function() {
  
  suits <- c("D", "C", "H", "S")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  deck <- expand.grid(rank = ranks, suit = suits)
  deck <- paste0(deck$rank, deck$suit)
  deck <- sample(deck)
  return(deck)
  
}


deal_hand <- function(deck) {
  
  hand <- deck[1:5]
  
  return(hand)
}


hand_eval <- function(hand) {
  
  if(royal(hand)) {
    return("Royal Flush")
  } else if(straight_flush(hand)) {
    return("Straight Flush")
  } else if(four_kind(hand)) {
    return("Four of a Kind")
  } else if(full_house(hand)) {
    return("Full House")
  } else if(flush_hand(hand)) {
    return("Flush")
  } else if(straight(hand)) {
    return("Straight")
  } else if(three_kind(hand)) {
    return("Three of a Kind")
  } else if(two_pair(hand)) {
    return("Two Pair")
  } else if(jacks_better(hand)) {
    return("Jacks or Better")
  } else {
    return("Nothing")
  }
  
}


deck <- create_deck()
hand <- deal_hand(deck)
deck <- deck[-(1:5)]

print_hand <- function(hand) {
  
  print("Current hand:")
  print(hand)
  
}

print_hand(hand)


hold_input <- function(hand) {
  
  print_hand(hand)
  cat("\n")
  
  cards_held <- readline(prompt="Enter cards to hold: ")
  cards_held <- strsplit(cards_held, "")[[1]]
  cards_held <- as.numeric(cards_held)
  
  return(cards_held)
  
}

hold_input(hand)


hold_cards <- function(hand, cards_held) {
  
  discard_cards <- c(1, 2, 3, 4, 5)
  
  # tst_disc[!discard_cards %in% cards_held]
  discard_cards <- setdiff(discard_cards, cards_held)
  
  print(discard_cards)
  
  for (i in discard_cards) {
    
    hand[i] <- "--"
    
  }
  
  return(hand)
  
}

hold_cards(test_hand, tst_hold)