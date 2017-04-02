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
playoffwins <- final$playoffpct
latewins <- final$allstar
seed <- final$seed
N <- length(regwins)

stanc("simple model.stan")$status
fit1 <- stan("simple model.stan",
             data = c("N","seed", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()

rstan::extract(fit1)
#write.csv(final, "playoff data.csv", row.names = F)
