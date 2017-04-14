#source("C:/Users/Julian Bautista/Documents/Portfolio/Playoff Basketball/Data Scraping.R")
source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Playoff Basketball")
final <- read.csv("https://raw.githubusercontent.com/julbautista/Playoff-Basketball/master/Playoff%20Data.csv")

#scatterplot
a <- ggplot(final, aes(season, playoffwins)) + geom_point() + geom_smooth() + ggtitle("Regular Season Win % on Raw Playoff Wins")
b <- ggplot(final, aes(allstar, playoffwins)) + geom_point() + geom_smooth() + ggtitle("Post-All Star Break Win % on Raw Playoff Wins")

c <- ggplot(final, aes(season, playoffpct)) + geom_point() + geom_smooth() + ggtitle("Regular Season Win % on Playoff Win %")
d <- ggplot(final, aes(allstar, playoffpct)) + geom_point() + geom_smooth(method ="lm") + ggtitle("Post-All Star Break Win % on Playoff Win %")

multiplot(a,b,c,d, cols = 2)

#running stan model
regwins <- final$season
playoffwins <- final$playoffwins
latewins <- final$allstar
N <- length(regwins)
seed1 <- final$seed


stanc("simple model.stan")$status
fit1 <- stan("simple model.stan",
             data = list("N","seed1", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()
fit_sum1 <- rstan::extract(fit1)


#
#Plots
par(mfcol = c(1,2),
    mar = c(4,3.9,1.5,3.5))
plot(colMeans(fit_sum$y_pred_reg),
     playoffwins,
     pch = 16,
     cex = 0.7,
     xlab = "Predicted",
     ylab = "Regular wins",
     cex.lab = 0.8,
     xlim = c(0,16),
     ylim = c(0,16))
abline(0,1, lty = 2, col = "gray")
plot(colMeans(fit_sum$y_pred_late),
     playoffwins,
     pch = 16,
     cex = 0.7,
     xlab = "Predicted",
     ylab = "Late wins",
     cex.lab = 0.8,
     xlim = c(0,16),
     ylim = c(0,16))
abline(0,1, lty = 2, col = "gray")

par(mfcol = c(1,2),
    mar = c(4,3.9,1.5,3.5))
plot(regwins,
     playoffwins,
     pch = 16,
     cex = 0.7,
     ylab = "Playoff Wins",
     xlab = "Regular win %",
     cex.lab = 0.8,
     xlim = c(0,1),
     ylim = c(0,16))
#abline(0,1, lty = 2, col = "gray")
points(regwins,
       colMeans(fit_sum$y_pred_reg), pch = 16, col = "grey")
plot(latewins,
     playoffwins,
     pch = 16,
     cex = 0.7,
     ylab = "Playoff Wins",
     xlab = "Late Win %",
     cex.lab = 0.8,
     xlim = c(0,1),
     ylim = c(0,16))
points(latewins,
       colMeans(fit_sum$y_pred_late), pch = 16, col = "grey")
#abline(0,1, lty = 2, col = "gray")


#write.csv(final, "playoff data.csv", row.names = F)
