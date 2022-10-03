test_hand <- c("KD", "9D", "JS", "10H", "QC")

four_kind <- function(hand) {
  
  ranks <- get_ranks(hand)
  print(ranks)
  
  if(length(unique(ranks) == 2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

four_kind(test_hand)

