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

stanc("binomial.stan")$status
fit2 <- stan("binomial.stan",
             data = list("N","seed1", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()
fit_sum2 <- rstan::extract(fit2)

print(fit2, digits = 3)
ggplot(NULL, aes(colMeans(fit_sum2$y_pred_reg), playoffwins)) + geom_point() + geom_abline(slope = 1, intercept = 0) + lims(x = c(0,16), y = c(0,16))

#fit2
par(mfcol = c(1,2),
    mar = c(4,3.9,1.5,3.5))
plot(colMeans(fit_sum2$y_pred_reg),
     playoffwins,
     pch = 16,
     cex = 0.7,
     xlab = "Predicted",
     ylab = "Regular wins",
     cex.lab = 0.8,
     xlim = c(0,16),
     ylim = c(0,16))
abline(0,1, lty = 2, col = "gray")
plot(colMeans(fit_sum2$y_pred_late),
     playoffwins,
     pch = 16,
     cex = 0.7,
     xlab = "Predicted Seed",
     ylab = "Late wins Seed",
     cex.lab = 0.8,
     xlim = c(0,16),
     ylim = c(0,16))
abline(0,1, lty = 2, col = "gray")



par(mfrow = c(4,2), #mar = c(,1,1,1))
    mar = c(4,4,1,1))
for(i in 1:8){
ranklevel <- final$seed == i
plot(regwins[ranklevel],
     playoffwins[ranklevel],
     pch = 16,
     cex = 0.7,
     ylab = "Playoff Wins Seed " %+% i,
     xlab = "Regular win % Seed " %+% i,
     cex.lab = 0.8,
     xlim = c(0,1),
     ylim = c(0,16))
#abline(0,1, lty = 2, col = "gray")
points(regwins[ranklevel],
       colMeans(fit_sum2$y_pred_reg)[ranklevel], pch = 16, col = "grey")
plot(latewins[ranklevel],
     playoffwins[ranklevel],
     pch = 16,
     cex = 0.7,
     ylab = "Playoff Wins Seed " %+% i,
     xlab = "Late Win % Seed " %+% i,
     cex.lab = 0.8,
     xlim = c(0,1),
     ylim = c(0,16))
points(latewins[ranklevel],
       colMeans(fit_sum2$y_pred_late)[ranklevel], pch = 16, col = "grey")
#abline(0,1, lty = 2, col = "gray")
}

stanc("twoparam.stan")$status
fit3 <- stan("twoparam.stan",
             data = list("N","seed1", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()
fit_sum3 <- rstan::extract(fit3)

print(fit3, digits = 3)
ggplot()
#fit2
par(mfcol = c(1,2),
    mar = c(4,3.9,1.5,3.5))
plot(colMeans(fit_sum3$y_pred_reg),
     playoffwins,
     pch = 16,
     cex = 0.7,
     xlab = "Predicted",
     ylab = "Regular wins",
     cex.lab = 0.8,
     xlim = c(0,16),
     ylim = c(0,16))
abline(0,1, lty = 2, col = "gray")
plot(colMeans(fit_sum3$y_pred_late),
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
       colMeans(fit_sum3$y_pred_reg), pch = 16, col = "grey")
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
       colMeans(fit_sum3$y_pred_late), pch = 16, col = "grey")
#abline(0,1, lty = 2, col = "gray")

#write.csv(final, "playoff data.csv", row.names = F)
