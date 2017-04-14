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
                 LowBoundLate = apply(fit_sum2$y_pred_late, 2, quantile, prob = 0.25),
                 UpBoundLate = apply(fit_sum2$y_pred_late, 2, quantile, prob = 0.75))


#plot betas--------------------
ggplot(betas_bin, aes(Reg, Late, label = Seed)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_text(colour = jbpal$brown, alpha = 1) + geom_label() + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Beta Values of a Binomial Model") + 
  ylab("Late Season Wins") + xlab("Full Regular Season Wins")

#plot prediction accuracy----------------------
ggplot(df_pred_bin, aes(year, WinsPredReg - playoffwins )) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Full Regular Season Prediction Accuracy Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  #geom_errorbar(ymin = df_pred_bin$LowBoundReg - df_pred_bin$playoffwins, ymax = df_pred_bin$UpBoundReg - df_pred_bin$playoffwins) + 
  facet_grid(.~seed)

ggplot(df_pred_bin, aes(year, WinsPredReg - playoffwins )) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(colour = jbpal$blue)  + 
  theme(panel.grid.major = element_blank()) + 
  ggtitle("Late Season Prediction Accuracy Plots, per Seed") + 
  ylab("Difference Between Prediction and Reality") + xlab("Years") + 
  #geom_errorbar(ymin = df_pred_bin$LowBoundLate - df_pred_bin$playoffwins, ymax = df_pred_bin$UpBoundLate - df_pred_bin$playoffwins) + 
  facet_grid(.~seed)
