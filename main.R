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
  
  if(royal_flush(hand)) {
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


# Play Strategy
job_simple <- function(hand) {
  
  if(!FALSE %in% royal_hold(hand)) {
    return(royal_hold(hand))
  } else if(!FALSE %in% straight_flush_hold(hand)) {
    return(straight_flush_hold(hand))
  } else if(!FALSE %in% four_kind_hold(hand)) {
    return(four_kind_hold(hand))
  } else if(!FALSE %in% four_to_royal_hold(hand)) {
    return(four_to_royal_hold(hand))
  } else if(!FALSE %in% full_house_hold(hand)) {
    return(full_house_hold(hand))
  } else if(!FALSE %in% flush_hold(hand)) {
    return(flush_hold(hand))
  } else if(!FALSE %in% straight_hold(hand)) {
    return(straight_hold(hand))
  } else if(!FALSE %in% three_kind_hold(hand)) {
    return(three_kind_hold(hand))
  } else if(!FALSE %in% four_strght_flush_hold(hand)) {
    return(four_strght_flush_hold(hand))
  } else if(!FALSE %in% two_pair_hold(hand)) {
    return(two_pair_hold(hand))
  } else if(!FALSE %in% high_pair_hold(hand)) {
    return(high_pair_hold(hand))
  } else if(!FALSE %in% three_to_royal_hold(hand)) {
    return(three_to_royal_hold(hand))
  } else if(!FALSE %in% four_to_flush_hold(hand)) {
    return(four_to_flush_hold(hand))
  } else if(!FALSE %in% low_pair_hold(hand)) {
    return(low_pair_hold(hand))
  } else if(!FALSE %in% four_out_strght_hold(hand)) {
    return(four_out_strght_hold(hand))
  } else if(!FALSE %in% two_suited_hold(hand)) {
    return(two_suited_hold(hand))
  } else if(!FALSE %in% three_strght_flush_hold(hand)) {
    return(three_strght_flush_hold(hand))
  } else if(!FALSE %in% two_unsuited_hold(hand)) {
    return(two_unsuited_hold(hand))
  } else if(!FALSE %in% ten_high_suited(hand)) {
    return(ten_high_suited(hand))
  } else if(!FALSE %in% one_high_card(hand)) {
    return(one_high_card(hand))
  } else {
    return(NULL)
  }
  
}


# Play Strategy
job_simple_stats <- function(hand) {
  
  if(!FALSE %in% royal_hold(hand)) {
    return(list(royal_hold(hand), "Royal Hold"))
  } else if(!FALSE %in% straight_flush_hold(hand)) {
    return(list(straight_flush_hold(hand), "Straight Flush Hold"))
  } else if(!FALSE %in% four_kind_hold(hand)) {
    return(list(four_kind_hold(hand), "Four Kind Hold"))
  } else if(!FALSE %in% four_to_royal_hold(hand)) {
    return(list(four_to_royal_hold(hand), "Four to Royal Hold"))
  } else if(!FALSE %in% full_house_hold(hand)) {
    return(list(full_house_hold(hand), "Full House Hold"))
  } else if(!FALSE %in% flush_hold(hand)) {
    return(list(flush_hold(hand), "Flush Hold"))
  } else if(!FALSE %in% straight_hold(hand)) {
    return(list(straight_hold(hand), "Straight Hold"))
  } else if(!FALSE %in% three_kind_hold(hand)) {
    return(list(three_kind_hold(hand), "Three Kind Hold"))
  } else if(!FALSE %in% four_strght_flush_hold(hand)) {
    return(list(four_strght_flush_hold(hand), "Four Straight Flush Hold"))
  } else if(!FALSE %in% two_pair_hold(hand)) {
    return(list(two_pair_hold(hand), "Two Pair Hold"))
  } else if(!FALSE %in% high_pair_hold(hand)) {
    return(list(high_pair_hold(hand), "High Pair Hold"))
  } else if(!FALSE %in% three_to_royal_hold(hand)) {
    return(list(three_to_royal_hold(hand), "Three to Royal Hold"))
  } else if(!FALSE %in% four_to_flush_hold(hand)) {
    return(list(four_to_flush_hold(hand), "Four to Flush Hold"))
  } else if(!FALSE %in% low_pair_hold(hand)) {
    return(list(low_pair_hold(hand), "Low Pair Hold"))
  } else if(!FALSE %in% four_out_strght_hold(hand)) {
    return(list(four_out_strght_hold(hand), "Four Outside Straight Hold"))
  } else if(!FALSE %in% two_suited_hold(hand)) {
    return(list(two_suited_hold(hand), "Two Suited Hold"))
  } else if(!FALSE %in% three_strght_flush_hold(hand)) {
    return(list(three_strght_flush_hold(hand), "Three Straight Flush Hold"))
  } else if(!FALSE %in% two_unsuited_hold(hand)) {
    return(list(two_unsuited_hold(hand), "Two Unsuited Hold"))
  } else if(!FALSE %in% ten_high_suited(hand)) {
    return(list(ten_high_suited(hand), "Ten High Suited Hold"))
  } else if(!FALSE %in% one_high_card(hand)) {
    return(list(one_high_card(hand), "One High Card Hold"))
  } else {
    return(NULL)
  }
  
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


full_pay_table <- function(bet, result) {
  
  if(result == "Royal Flush") {
    return(bet * 800)
  } else if(result == "Straight Flush") {
    return(bet * 50)
  } else if(result == "Four of a Kind") {
    return(bet * 25)
  } else if(result == "Full House") {
    return(bet * 9)
  } else if(result == "Flush") {
    return(bet * 6)
  } else if(result == "Straight") {
    return(bet * 4)
  } else if(result == "Three of a Kind") {
    return(bet * 3)
  } else if(result == "Two Pair") {
    return(bet * 2)
  } else if(result == "Jacks or Better") {
    return(bet * 1)
  } else if(result == "Nothing") {
    return(bet * 0)
  }
  
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


play_hand_auto <- function(show_output = FALSE) {
  
  deck <- create_deck()
  hand <- deal_hand(deck, 5)
  deck <- update_deck(deck, hand)
  
  if(show_output) {
    print_hand(hand)
  }
  
  held <- job_simple(hand)
  
  if(show_output) {
    cat("\n")
    cat("Cards Held:", held, "\n")
  }
  
  hand <- hold_cards(hand, held)
  hand <- draw_cards(deck, hand)
  
  if(show_output) {
    cat("\n")
    print_hand(hand)
  }
  
  deck <- update_deck(deck, hand)
  
  if(show_output) {
    cat("\n")
  }
  result <- hand_eval(hand)
  #print(result)
  
  return(result)
  
}

# play_hand_auto(TRUE)

bank_roll <- 1000

auto_play_stats <- function(bet)  {
  
  bank_roll <<- bank_roll - bet
  
  deck <- create_deck()
  hand <- deal_hand(deck, 5)
  dealt_hand <- hand
  deck <- update_deck(deck, hand)
  
  held <- job_simple_stats(hand)
  held_cards <- held[[1]]
  strat_match <- held[[2]]
  # cards_held <- held
  
  hand <- hold_cards(hand, held_cards)
  held_hand <- hand
  hand <- draw_cards(deck, hand)
  draw_hand <- hand
  
  deck <- update_deck(deck, hand)

  result <- hand_eval(hand)
  
  win_loss <- full_pay_table(bet = bet, result = result)

  bank_roll <<- bank_roll + win_loss

  # return(result)
  
  hand_stats <- list(dealt_hand, strat_match,
                     held_cards, held_hand, draw_hand, result, bank_roll)
  
  return(hand_stats)
  
  
}
