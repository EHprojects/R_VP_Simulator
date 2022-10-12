library(tidyverse)
library(stringr)

# Creates and randomizes a standard deck of 52 cards
create_deck <- function() {
  
  suits <- c("D", "C", "H", "S")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  deck <- expand.grid(rank = ranks, suit = suits)
  deck <- paste0(deck$rank, deck$suit)
  deck <- sample(deck)
  return(deck)
  
}


# Returns a hand of the given size
deal_hand <- function(deck, size) {
  
  hand <- deck[1:size]
  
  return(hand)
}


# Updates the deck to remove the already dealt cards
update_deck <- function(deck, hand) {
  
  card_diff <- setdiff(deck, hand)
  
  # print(card_diff)
  
  return(card_diff)
  
}


# Evaluate the hand for a winning / losing condition
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


# Print the hand to the console in a readable format
print_hand <- function(hand) {
  
  print("Current hand:")
  print(hand)
  
}


# Prompts the user for cards to be held
hold_input <- function(hand) {
  
  print_hand(hand)
  cat("\n")
  
  cards_held <- readline(prompt="Enter cards to hold: ")
  cards_held <- strsplit(cards_held, "")[[1]]
  cards_held <- as.numeric(cards_held)
  
  return(cards_held)
  
}


# Returns the hand with the non-held cards removed
hold_cards <- function(hand, cards_held) {
  
  discard_cards <- c(1, 2, 3, 4, 5)
  
  # tst_disc[!discard_cards %in% cards_held]
  discard_cards <- setdiff(discard_cards, cards_held) # remove cards held from discard cards
  
  # print(discard_cards)
  
  for (i in discard_cards) {
    
    hand[i] <- "--"
    
  }
  
  return(hand)
  
}


# Draws cards from the deck to replace the discarded cards and returns the hand
draw_cards <- function(deck, hand) {
  
  # print(hand)
  j <- 1
  
  for (i in 1:length(hand)) {
    
    if(hand[i] == "--") {
      hand[i] <- deck[j]
      j <- j + 1
    }
    
  }
  
  return(hand)
  
}


#### Control Logic ####
play_hand <- function() {
  
  deck <- create_deck()
  hand <- deal_hand(deck, 5)
  deck <- update_deck(deck, hand)
  held <- hold_input(hand)
  hand <- hold_cards(hand, held)
  hand <- draw_cards(deck, hand)
  print_hand(hand)
  deck <- update_deck(deck, hand)
  result <- hand_eval(hand)
  return(result)
  
}

# play_hand()
