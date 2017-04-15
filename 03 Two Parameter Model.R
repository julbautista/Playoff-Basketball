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

stanc("twoparameter.stan")$status
fit3 <- stan("twoparameter.stan",
             data = list("N","seed1", "regwins","playoffwins","latewins"),
             iter = 1000, chains = 3)
beep()
fit_sum3 <- rstan::extract(fit3)
print(fit3, digits = 3)

#create dataframes----------------
betas_two <- data_frame(Seed = c(1:8), Reg = colMeans(fit_sum3$beta_reg), Late = colMeans(fit_sum3$beta_late))
df_pred_two <- cbind(final, WinsPred = colMeans(fit_sum3$y_pred), 
                      LowBound = apply(fit_sum3$y_pred, 2, quantile, prob = 0.25),
                      UpBound = apply(fit_sum3$y_pred, 2, quantile, prob = 0.75))

#plot betas--------------------
ggplot(betas_two, aes(Reg, Late, label = Seed)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_text(colour = jbpal$brown, alpha = 1) + geom_label() + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Beta Values of a Two Parameter Normal Model") + 
  ylab("Late Season Wins") + xlab("Full Regular Season Wins")

#plot prediction accuracy----------------------
ggplot(df_pred_two, aes(year, WinsPred - playoffwins, ymin = LowBound - playoffwins, ymax = UpBound - playoffwins)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Two Parameter Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_errorbar() + 
  facet_grid(conference~seed) 

ggplot(df_pred_two, aes(year, WinsPred, ymin = LowBound, ymax = UpBound)) + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Two Parameter Accuracy Plots, per Seed", subtitle = "Green: Actual\nBlue: Predicted") + 
  ylab("Playoff Wins") + xlab("Years") + 
  geom_point(aes(year, playoffwins)) +
  geom_errorbar() + 
  facet_grid(conference~seed)

#find outliers----------------
ggplot(df_pred_two, aes(year, WinsPred - playoffwins, ymin = LowBound - playoffwins, ymax = UpBound - playoffwins)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Two Parameter Prediction Balance Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_errorbar() + 
  geom_label_repel(data = subset(df_pred_two, WinsPred - playoffwins > 5| WinsPred - playoffwins < -5), aes(label = year %+% " " %+% nickname), size = 4, alpha = 1, box.padding = unit(0.85, "lines"), label.padding = unit(0.10, "lines"), force = 40) +
  geom_point(data = subset(df_pred_two, WinsPred - playoffwins > 5| WinsPred - playoffwins < -5)) +
  facet_grid(conference~seed)


ggplot(df_pred_two, aes(year, WinsPred - playoffwins )) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Two Parameter Prediction Accuracy Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  geom_label_repel(data = subset(df_pred_two, WinsPred - playoffwins > 5| WinsPred - playoffwins < -5), aes(label = year %+% " " %+% nickname), size = 4, alpha = 1, box.padding = unit(0.65, "lines"), label.padding = unit(0.10, "lines"), force = 6) +
  geom_point(data = subset(df_pred_two, WinsPred - playoffwins > 5| WinsPred - playoffwins < -5)) +
  #geom_errorbar(ymin = df_pred_bin$LowBoundLate - df_pred_bin$playoffwins, ymax = df_pred_bin$UpBoundLate - df_pred_bin$playoffwins) + 
  facet_grid(.~seed)
