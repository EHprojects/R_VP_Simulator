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

deck <- create_deck()

deal_hand <- function(deck) {
  
  hand <- deck[1:5]
  
  return(hand)
}

hand <- deal_hand(deck)

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