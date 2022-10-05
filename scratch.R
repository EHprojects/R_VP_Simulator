test_hand <- c("10D", "JD", "KD", "QD", "AS")

royal <- function(hand) {
  
  rank_vals <- as.numeric(assign_rank_vals(hand))
  
  royal_str <- setequal(c(10, 11, 12, 13, 14), rank_vals) # test for "royal straight"
  
  if(royal_str & flush_hand(hand)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  

}

royal(test_hand)
