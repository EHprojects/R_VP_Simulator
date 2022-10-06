
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

test_hand <- c("KD", "9D", "JS", "10H", "QC")

