get_ranks <- function(hand) {
  
  ranks <- NULL
  
  for (i in 1:length(hand)) {
    
    ranks <- c(ranks, str_sub(hand[i], 1, -2))
    
  }
  return(ranks)
}

get_ranks(test_hand)

get_suits <- function(hand) {
  
  suits <- NULL
  
  for (i in 1:length(hand)) {
    suits <- c(suits, str_sub(hand[i], -1, -1))
  }
  return(suits)
}

assign_rank_vals <- function(hand) {
  
  ranks <- get_ranks(hand)
  vals <- NULL
  
  for (i in 1:length(ranks)) {
    
    if(ranks[i] == "J") {
      vals <- c(vals, 11)
    } else if(ranks[i] == "Q") {
      vals <- c(vals, 12)
    } else if(ranks[i] == "K") {
      vals <- c(vals, 13)
    } else if(ranks[i] == "A") {
      vals <- c(vals, 14)
    } else {
      vals <- c(vals, ranks[i])
    }
    
  }
  
  return(vals)
  
}

assign_rank_vals(test_hand)


eval_simple <- function(hand) {
  
}

#### Evaluate for a Four of a Kind - Deprecated####
# four_kind <- function(hand) {
#   
#   ranks <- get_ranks(hand)
#   
#   match_pos1 <- 0
#   match_pos2 <- 0
#   
#   match_pos1 <- sum(ranks == ranks[1])
#   match_pos2 <- sum(ranks == ranks[2])
#   
#   if(match_pos1 == 4 | match_pos2 == 4) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
#   
# }

#### Evaluate for a Four of a Kind ####
four_kind <- function(hand) {
  
  ranks <- get_ranks(hand)
  rank_match <- rep(NA, length(ranks))
  
  for (i in 1:length(ranks)) {
    
    rank_match[i] <- sum(ranks == ranks[i])
    
  }
  
  if(4 %in% rank_match) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

four_kind(test_hand)


#### Evaluate for a Straight ####
straight <- function(hand) {
  
  rank_vals <- assign_rank_vals(hand) # get the rank vals
  rank_vals <- as.numeric(rank_vals)
  rank_vals <- sort(rank_vals) # sort ascending
  # print(rank_vals)
  
  gaps <- 0
  max_gap <- 0
  
  if(length(unique(rank_vals)) != 5) {
    
    return(FALSE)
    
  }
  
  # Ace in the hand
  ace_hand <- FALSE
  
  if(14 %in% rank_vals) { ace_hand <- TRUE }
  
  # print(paste0("ace_hand: ", ace_hand))
  
  
  # Evaluation for no Ace in the hand
  if(ace_hand == FALSE) {
    
    # print("No Ace eval")
    
    for (i in 1:(length(rank_vals) - 1)) {
      
      gap <- (rank_vals[i + 1] - rank_vals[i]) - 1
      # print(paste0("gap: ", gap))
      
      if(gap > 0) { gaps <- gaps + 1 } # if gap > 0 (i.e. a missing card), increase the # of gaps
      if(gap > max_gap) { max_gap <- gap } # if the current gap is > the max gap, increase the max gap
      
    }
    
    # print(paste0("gaps: ", gaps))
    # print(paste0("max_gap: ", max_gap))
    
    if(gaps < 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  }
  
  # Variables for ace low / ace hi straights
  
  ace_hi_gaps <- 0
  max_hi_gap <- 0
  ace_lo_gaps <- 0
  max_lo_gap <- 0
  ace_hi <- FALSE
  ace_lo <- FALSE
  
  
  # Evaluation for Ace in the hand
  if(ace_hand == TRUE) {
    
    # Ace hi eval
    
    # print("Ace hi eval")
    # print(rank_vals)
    
    for (i in 1:(length(rank_vals) - 1)) {
      
      gap <- (rank_vals[i + 1] - rank_vals[i]) - 1
      # print(paste0("gap: ", gap))
      
      if(gap > 0) { ace_hi_gaps <- ace_hi_gaps + 1 } # if gap > 0 (i.e. a missing card), increase the # of gaps
      if(gap > max_hi_gap) { max_hi_gap <- gap } # if the current gap is > than max gap, increase the max gap
      
    }
    
    # print(paste0("ace_hi_gaps: ", ace_hi_gaps))
    # print(paste0("max_hi_gap: ", max_hi_gap))
    
    if(ace_hi_gaps < 1) {
      return(TRUE)
    }
    
    
    # Ace lo eval
    
    # print("Ace lo eval")
    
    rank_vals <- replace(rank_vals, rank_vals == 14, 1)
    rank_vals <- sort(rank_vals) # sort ascending
    # print(rank_vals)
    
    for (i in 1:(length(rank_vals) - 1)) {
      
      gap <- (rank_vals[i + 1] - rank_vals[i]) - 1
      # print(paste0("gap: ", gap))
      
      if(gap > 0) { ace_lo_gaps <- ace_lo_gaps + 1 } # if gap > 0 (i.e. a missing card), increase the # of gaps
      if(gap > max_lo_gap) { max_lo_gap <- gap } # if the current gap is > than max gap, increase the max gap
      
    }
    
    # print(paste0("ace_lo_gaps: ", ace_lo_gaps))
    # print(paste0("max_lo_gap: ", max_lo_gap))
    
    if(ace_lo_gaps < 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  }
  
}

#### Evaluate for a Three of a Kind - Deprecated ####
# three_kind <- function(hand) {
#   
#   ranks <- get_ranks(hand)
#   
#   match_pos1 <- 0
#   match_pos2 <- 0
#   match_pos3 <- 0
#   
#   match_pos1 <- sum(ranks == ranks[1])
#   match_pos2 <- sum(ranks == ranks[2])
#   match_pos3 <- sum(ranks == ranks[3])
#   
#   
#   if(match_pos1 == 3 | match_pos2 == 3 | match_pos3 == 3) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
#   
# }


#### Evaluate for a Three of a Kind ####
three_kind <- function(hand) {
  
  ranks <- get_ranks(hand)
  rank_match <- rep(NA, length(ranks))
  
  for (i in 1:length(ranks)) {
    
    rank_match[i] <- sum(ranks == ranks[i])
    
  }
  
  if(3 %in% rank_match) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#### Evaluate for a Flush ####
flush_hand <- function(hand) {
  
  suits <- get_suits(hand)
  
  if(length(unique(suits)) == 1)  {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#### Evaluate for a Royal Flush ####
royal <- function(hand) {
  
  rank_vals <- as.numeric(assign_rank_vals(hand))
  
  royal_str <- setequal(c(10, 11, 12, 13, 14), rank_vals) # test for "royal straight"
  
  if(royal_str & flush_hand(hand)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  
  
}

