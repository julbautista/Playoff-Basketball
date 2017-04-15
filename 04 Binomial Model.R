#setup file and load data------------------
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Playoff Basketball")
#source("C:/Users/Julian Bautista/Documents/Portfolio/Playoff Basketball/Data Scraping.R")
source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
final <- read.csv("https://raw.githubusercontent.com/julbautista/Playoff-Basketball/master/Playoff%20Data.csv")

#run stan model------------------
regwins <- final$season
playoffwins <- final$playoffwins
latewins <- final$allstar
N <- length(regwins)
seed1 <- final$seed

stanc("binomial.stan")$status
fit2 <- stan("binomial.stan",
             data = list("N","seed1", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()
fit_sum2 <- rstan::extract(fit2)

#create dataframes----------------
betas_bin <- data_frame(Seed = c(1:8), Reg = colMeans(fit_sum2$beta_reg), Late = colMeans(fit_sum2$beta_late))
df_pred_bin <- cbind(final, WinsPredReg = colMeans(fit_sum2$y_pred_reg), 
                 LowBoundReg = apply(fit_sum2$y_pred_reg, 2, quantile, prob = 0.25),
                 UpBoundReg = apply(fit_sum2$y_pred_reg, 2, quantile, prob = 0.75),
                 WinsPredLate = colMeans(fit_sum2$y_pred_late), 
                 LowBoundLate = apply(fit_sum2$y_pred_late, 2, quantile, prob = 0.025),
                 UpBoundLate = apply(fit_sum2$y_pred_late, 2, quantile, prob = 0.975))


#plot betas--------------------
ggplot(betas_bin, aes(Reg, Late, label = Seed)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_text(colour = jbpal$brown, alpha = 1) + geom_label() + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Beta Values of a Binomial Model") + 
  ylab("Late Season Wins") + xlab("Full Regular Season Wins")

#plot prediction accuracy----------------------
ggplot(df_pred_bin, aes(year, WinsPredReg - playoffwins, 
                        ymin = LowBoundReg - playoffwins, 
                        ymax = UpBoundReg - playoffwins )) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Full Regular Season Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_errorbar() + 
  facet_grid(conference~seed)

ggplot(df_pred_bin, aes(year, WinsPredLate - playoffwins, ymin = LowBoundLate - playoffwins, ymax = UpBoundLate - playoffwins)) + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Late Season Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  geom_errorbar() + 
  facet_grid(conference~seed)

ggplot(df_pred_bin, aes(year, WinsPredReg, ymin = LowBoundReg, ymax = UpBoundReg)) + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Full Regular Season Prediction Accuracy Plots, per Seed", subtitle = "Green: Actual\nBlue: Predicted") + 
  ylab("Playoff Wins") + xlab("Years") + 
  geom_point(aes(year, playoffwins)) +
  geom_errorbar() + 
  facet_grid(conference~seed)

ggplot(df_pred_bin, aes(year, WinsPredLate, ymin = LowBoundLate, ymax = UpBoundLate)) + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Late Season Prediction Accuracy Plots, per Seed", subtitle = "Green: Actual\nBlue: Predicted") + 
  ylab("Playoff Wins") + xlab("Years") + 
  geom_point(aes(year, playoffwins)) +
  geom_errorbar() + 
  facet_grid(conference~seed)



#Finding Outliers Full Season------------------
ggplot(df_pred_bin, aes(year, WinsPredReg - playoffwins, 
                        ymin = LowBoundReg - playoffwins, 
                        ymax = UpBoundReg - playoffwins )) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Full Regular Season Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_errorbar() + 
  geom_label_repel(data = subset(df_pred_bin, WinsPredReg - playoffwins > 5| WinsPredReg - playoffwins < -5), aes(label = year %+% " " %+% nickname), size = 4, alpha = 1, box.padding = unit(0.85, "lines"), label.padding = unit(0.10, "lines"), force = 40) +
  geom_point(data = subset(df_pred_bin, WinsPredReg - playoffwins > 5| WinsPredReg - playoffwins < -5)) +
  facet_grid(conference~seed)


#Finding Outliers Late Season------------------
ggplot(df_pred_bin, aes(year, WinsPredLate - playoffwins, ymin = LowBoundLate - playoffwins, ymax = UpBoundLate - playoffwins)) + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Late Season Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) + 
  geom_errorbar() + 
  geom_label_repel(data = subset(df_pred_bin, WinsPredLate - playoffwins > 5| WinsPredLate - playoffwins < -5), aes(label = year %+% " " %+% nickname), size = 4, alpha = 1, box.padding = unit(0.65, "lines"), label.padding = unit(0.10, "lines"), force = 6) +
  geom_point(data = subset(df_pred_bin, WinsPredLate - playoffwins > 5| WinsPredLate - playoffwins < -5)) +
  facet_grid(conference~seed)
