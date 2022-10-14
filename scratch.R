# match_ranks Output (sorted):
# four of kind  = 17  # [1] 1 4 4 4 4
# full house    = 13  # [1] 2 2 3 3 3
# three of kind = 11  # [1] 1 1 3 3 3
# two pair      = 9   # [1] 1 2 2 2 2
# one pair      = 7   # [1] 1 1 1 2 2
# nothing       = 5   # [1] 1 1 1 1 1

# match_suits Output (sorted):
# flush         = 25  # [1] 5 5 5 5 5

# JoB Hand Ranks:
# Royal Flush > Straight Flush > Four of a Kind > Full House > Flush >
# Straight > Three of a Kind > Two Pair > Jacks or Better

# Face Card Ranks:
# J = 11
# Q = 12
# K = 13
# A = 14

# To Do: Abstract out rank matching

options("scipen" = 10) # increase number of displayed digits in dataframe
options(pillar.sigfig = 6) # increase sig figs in tibbles



testing_func <- function() {
  
  deck <- create_deck()
  hand <- deal_hand(deck)
  #print(hand)
  hand_eval(hand)
  
}

system.time(results <- replicate(n = 2000000, testing_func()))
table(results)

saveRDS(results, file = "rslts_two_mil_deal.RDS")

rslts_table <- table(results)
rslts <- as.data.frame(rslts_table)

rslts <- rslts %>%
    arrange(Freq)

rslts %>%
  mutate(prob = Freq / 2000000) %>%
  mutate(pct = prob * 100) %>%
  mutate(cum_pct = cumsum(pct))






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
  
  if(length(cards_in_royal) == 4) {
    return(cards_in_royal)
  } else {
    return(FALSE)
  }
  
}

print(cards_in_royal)

# If all cards are in the "royal straight"
if(all(rank_vals %in% royal_ranks)) {
  print("Yup")
}


tst_cards <- c("10S", "10C", "AS", "QS", "KS")

suits <- c("D", "C", "H", "S")
royal_ranks <- c("10", "J", "Q", "K", "A")
royal_flushes <- expand.grid(rank = royal_ranks, suit = suits)
royal_flushes <- paste0(royal_flushes$rank, royal_flushes$suit)
royal_flushes <- matrix(royal_flushes, nrow = 5, ncol = 4)


tst_cards %in% royal_flushes[ , 4]
sum(tst_cards %in% royal_flushes[ , 4])

which(!tst_cards %in% royal_flushes[ , 4])


tst_cmbn <- combn(tst_hand, 4)
tst_cmbn[ , 1]

royal_ranks <- c(10, 11, 12, 13, 14)
tst_hand <- c(10, 10, 11, 12, 13)

tst_hand %in% royal_ranks
tst_hand == royal_ranks
all(tst_hand %in% royal_ranks)

setdiff(royal_ranks, tst_hand)
length(setdiff(royal_ranks, tst_hand))

royal_ranks[ !royal_ranks == tst_card  ]
