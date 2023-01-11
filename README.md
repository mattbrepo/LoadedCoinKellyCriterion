# LoadedCoinKellyCriterion
Simulation with a loaded coin.

**Language: R**

**Start: 2023**

## Why
"What would you do if you were invited to play a game where you were given $25 and allowed to place bets for 30 minutes on a coin that you were told was biased to come up heads 60% of the time?". This is the intriguing premise of [this paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2856963) from 2016. Once I heard about it, I felt the need to write a simple simulation function to test different strategies:

```r
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
    
    # call the strategy function to get:
    #   - the bet choice (bet_flip, either H or T)
    #   - the bet size (i.e, the money on the bet, bet_money)
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
```

The money is set to $25 as in the original game, but instead of playing for "30 minutes", I set the game to be limited by a maximum of 100 bets (n_flips). Also, I defined 4 different strategies (strategy_func):

| Strategy | Bet choice                          | Bet size                              |
|----------|-------------------------------------|---------------------------------------|
| 1        | Always head                         | All-in                                |
| 2        | Always head                         | Half of the money available           |
| 3        | Always head                         | Twenty percent of the money available |
| 4        | Head unless 5 repetitive heads seen | Twenty percent of the money available |

_Strategy 4_ is a kind of implementation of the [Gambler's fallacy](https://en.wikipedia.org/wiki/Gambler%27s_fallacy) which is the incorrect belief that, if a particular event occurs more frequently than normal during the past, it is less likely to happen in the future (or vice versa).

I tested all strategies 10,000 times:

![Strategy Money](/images/strategy_money.jpg)

The red line is set to the initial $25. Both _Strategy 1_ and _Strategy 2_ are complete failures since the final amount is more often than not less the initial $25. However, _Strategy 2_ has by far the highest win recorded:

```r
> data_bp
   max_money    strategy
1 Strategy 1         0.0
2 Strategy 2 160499940.2
3 Strategy 3    276730.4
4 Strategy 4    184487.0
```

## Optimal Strategy - Kelly criterion
The [Kelly criterion](https://en.wikipedia.org/wiki/Kelly_criterion) is a formula that determines the optimal theoretical size for a bet which makes it perfect to determine the optimal strategy for this game:

$$ f = p - \frac{q}{b} $$

where _f_ is the proportion of the current money to wager, _p_ is the probability of a win (60%), _q_ is the probability of a loss (q = 1 − p = 40%) and _b_ is the proportion of the bet gained with a win (100%). Therefore, the formula for this game can be rewritten as:

$$ f = p - \frac{1 - p}{1} = 2p - 1 $$

The optimal bet strategy is 20% of the current money available (_Strategy 3_).


