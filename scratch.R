test_hand <- c("9D", "9D", "KS", "KH", "KC")

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

test_ranks <- get_ranks(test_hand)


for (i in 1:2) {
  
  print("test")
  
}

sum(test_ranks == test_ranks[2])

anyDuplicated(test_ranks)
duplicated(test_ranks)

tst2 <- rep(NA, 5)
tst2[1] <- 1
