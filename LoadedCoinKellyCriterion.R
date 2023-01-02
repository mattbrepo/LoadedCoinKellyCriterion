
#
# Get head/tail flips of a loaded coin (60% head, 40% tail)
get_loaded_flips <- function(n_flips) {
  flips <- sample(c('H', 'T'), n_flips, replace = TRUE, prob=c(0.6, 0.4))
  return(flips)
}

#
# Simulate a betting strategy
simulate_strategy <- function(n_flips, money, strategy_func) {
  # prepare the flips
  flips = get_loaded_flips(n_flips)
  
  # loop through the flips
  current_money = money
  count_bets = 0
  prevFlips = c()
  for (flip in flips) {
    count_bets = count_bets + 1
    if (current_money <= 0) {
      # no point to continue...the money is gone
      break
    }
    
    # call the strategy function to get bet_list which contains
    # the bet (bet_flip, either H or T) and the money on it (bet_money)
    bet_list = strategy_func(current_money, prevFlips)
    if (flip == bet_list$bet_flip) {
      # winning bet
      current_money = current_money + bet_list$bet_money
    } else {
      # loosing bet
      current_money = current_money - bet_list$bet_money
    }
    
    prevFlips = c(prevFlips, flip)
  }
  
  return(list(n_bets=count_bets, money=round(current_money, digits=1)))
}

#
# Always Head with all the money
strategy1 <- function(money, prevFlips) {
  bet_list <- list(bet_flip = 'H', bet_money = money)
  return(bet_list)
}

#
# Always Head with half of the money
strategy2 <- function(money, prevFlips) {
  bet_list <- list(bet_flip = 'H', bet_money = money / 2)
  return(bet_list)
}

#
# Always Head with twenty percent of the money
strategy3 <- function(money, prevFlips) {
  bet_list <- list(bet_flip = 'H', bet_money = money * 0.2)
  return(bet_list)
}

#
# Tail after 5 heads with twenty percent of the money
strategy4 <- function(money, prevFlips) {
  num = 5
  last5 = tail(prevFlips, num)
  
  bet_flip = 'H'
  if (sum(last5 == "H") == num) {
    #print('Tail rule') # prodebug
    bet_flip = 'T'
  }
  
  bet_list <- list(bet_flip = bet_flip, bet_money = money * 0.2)
  return(bet_list)
}

#
# Main
#

n_flips = 100
available_money = 25

# test strategies one by one
simulate_strategy(n_flips, available_money, strategy1)
simulate_strategy(n_flips, available_money, strategy2)
simulate_strategy(n_flips, available_money, strategy3)
simulate_strategy(n_flips, available_money, strategy4)

# replicate the strategies many times
rep_time = 10000

rep1 = replicate(rep_time, simulate_strategy(n_flips, available_money, strategy1))
rep2 = replicate(rep_time, simulate_strategy(n_flips, available_money, strategy2))
rep3 = replicate(rep_time, simulate_strategy(n_flips, available_money, strategy3))
rep4 = replicate(rep_time, simulate_strategy(n_flips, available_money, strategy4))

summary(unlist(rep1["money",]))
summary(unlist(rep1["n_bets",]))
summary(unlist(rep2["money",]))
summary(unlist(rep2["n_bets",]))
summary(unlist(rep3["money",]))
summary(unlist(rep4["money",]))

data <- data.frame(money=unlist(rep1["money",]), n_bets=unlist(rep1["n_bets",]), strategy=rep("Strategy 1", rep_time))
data <- rbind(data, data.frame(money=unlist(rep2["money",]), n_bets=unlist(rep2["n_bets",]), strategy=rep("Strategy 2", rep_time)))
data <- rbind(data, data.frame(money=unlist(rep3["money",]), n_bets=unlist(rep3["n_bets",]), strategy=rep("Strategy 3", rep_time)))
data <- rbind(data, data.frame(money=unlist(rep4["money",]), n_bets=unlist(rep4["n_bets",]), strategy=rep("Strategy 4", rep_time)))

# Boxplot of the Money grouped by Strategy
boxplot(money~strategy, data, main="Strategies - Money")
abline(h=available_money, col="red")

# Boxplot of the Money grouped by Strategy without outliers
data_filtered = data[data$money < 2000,]
boxplot(money~strategy, data_filtered, main="Strategies - Money without outliers")
abline(h=available_money, col="red")

# Boxplot of the number of bets performed grouped by Strategy without outliers
boxplot(n_bets~strategy, data_filtered, main="Strategies - # of bets without outliers")

# Barplot of the maximum win grouped by Strategy
data_bp = aggregate(data$money, by = list(data$strategy), max)
colnames(data_bp) <- c('max_money', 'strategy')
barplot(strategy~max_money, data_bp)
