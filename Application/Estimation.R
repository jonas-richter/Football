# get realistic quotes for win/draw/loss
# source = https://www.kaggle.com/datasets/austro/beat-the-bookie-worldwide-football-dataset/code?resource=download
quotes = read.csv(file = "./Application/closing_odds.csv")
win = mean(quotes$avg_odds_home_win) - 2
draw = mean(quotes$avg_odds_draw) - 2
loss = mean(quotes$avg_odds_away_win) - 2

## get predictions/truth
results_k = read.csv(file = "./Output/K_fold_testing/results_k.csv")

# bet on everything
a = ifelse(results_k$results_RF == "win" & results_k$Outcome == "win", win,
       ifelse(results_k$results_RF == "draw" & results_k$Outcome == "draw", draw,
              ifelse(results_k$results_RF == "loss" & results_k$Outcome == "loss", loss, -1)))
print(sum(a))
# bet on wins and losses only
b = ifelse(results_k$results_RF == "win" & results_k$Outcome == "win", win,
           ifelse(results_k$results_RF == "win" & results_k$Outcome == "loss", -1,
              ifelse(results_k$results_RF == "win" & results_k$Outcome == "draw", -1,
                ifelse(results_k$results_RF == "loss" & results_k$Outcome == "win", -1,
                  ifelse(results_k$results_RF == "loss" & results_k$Outcome == "draw", -1,
                     ifelse(results_k$results_RF == "loss" & results_k$Outcome == "loss", loss, 0))))))
print(sum(b))
# bet on wins only
c = ifelse(results_k$results_RF == "win" & results_k$Outcome == "win", win,
           ifelse(results_k$results_RF == "win" & results_k$Outcome == "loss", -1,
                  ifelse(results_k$results_RF == "win" & results_k$Outcome == "draw", -1, 0)))
print(sum(c))
# bet on loss only
d = ifelse(results_k$results_RF == "loss" & results_k$Outcome == "loss", loss,
           ifelse(results_k$results_RF == "loss" & results_k$Outcome == "win", -1,
                  ifelse(results_k$results_RF == "loss" & results_k$Outcome == "draw", -1, 0)))
print(sum(d))

## ENSEMBLE
# bet on everything
e = ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "win", win,
           ifelse(results_k$results_RF == "draw" & results_k$results_naivebayes == "draw" & results_k$Outcome == "draw", draw,
                  ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "loss", loss, -1)))
print(sum(e))
# bet on wins and losses only
f = ifelse(results_k$results_RF == "win" & results_k$Outcome == "win" & results_k$results_naivebayes == "win", win,
           ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "loss", -1,
               ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "draw", -1,
                  ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "win", -1,
                     ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "draw", -1,
                         ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "loss", loss, 0))))))
print(sum(f))
# bet on wins only
g = ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "win", win,
           ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "loss", -1,
                  ifelse(results_k$results_RF == "win" & results_k$results_naivebayes == "win" & results_k$Outcome == "draw", -1, 0)))
print(sum(g))
# bet on loss only
h = ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "loss", loss,
           ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "win", -1,
                  ifelse(results_k$results_RF == "loss" & results_k$results_naivebayes == "loss" & results_k$Outcome == "draw", -1, 0)))
print(sum(h))




# accuracy wins and losses only
## apparently, RF has highest accuracy for loss and naivebayes for win
#s = ifelse(results_k$results_naivebayes == "win" & results_k$Outcome == "win", win,
#               ifelse(results_k$results_RF == "loss" & results_k$Outcome == "loss", loss, 8))
#s_clean = s[!s %in% 8]
#1 - as.numeric(table(s_clean)[1]/table(s_clean)[2])

filter = ifelse(results_k$results_naivebayes == "win" & results_k$results_RF != "loss" & results_k$Outcome == "win", win,
             ifelse(results_k$results_naivebayes != "win" & results_k$results_RF == "loss" & results_k$Outcome == "loss", loss,
                    ifelse(results_k$results_naivebayes == "win" & results_k$results_RF != "loss" & results_k$Outcome == "loss", -1,
                           ifelse(results_k$results_naivebayes == "win" & results_k$results_RF != "loss" & results_k$Outcome == "draw", -1,
                                  ifelse(results_k$results_naivebayes != "win" & results_k$results_RF == "loss" & results_k$Outcome == "win", -1,
                                         ifelse(results_k$results_naivebayes != "win" & results_k$results_RF == "loss" & results_k$Outcome == "draw", -1,
                                                0))))))
sum(filter)
