source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Playoff Basketball")
library(rvest)
#throughout this project i use the special `%+%` function created from my custom startup file, the function is below 
#`%+%` <- function(a, b) paste0(a, b)

#scrape all-star game dates from 2003 because that's 7 game first round... later project from 1985 because it is start of 4 playoff rounds
allstar <- read_html("http://www.basketball-reference.com/allstar/")

dates <- allstar %>% 
  html_nodes('#all_star_games_nba td:nth-child(3) a') %>%
  html_text()

dates <- dates[2:15]
rm(allstar)

#scrape win-loss end of season for playoff teams 2016
#two for loops due to different forms of tables on website
win_loss <- data_frame()
year <- c()

for(i in c(2010,2015,2016)){
  endseason <- read_html("http://www.basketball-reference.com/leagues/NBA_" %+% i %+% ".html")
  
  #extract east relevant statistics as strings then classify into relevant vectors
  
  east <- endseason %>%
    html_nodes('#divs_standings_E .right , #divs_standings_E .left') %>%
    html_text()
  
  eteams <- east[seq(8,120,8)]
  ewins <- east[seq(9,121,8)]
  elosses <- east[seq(10,122,8)]
  ewinpct <- east[seq(11,123,8)]
  
  #extract west relevant statistics then classify into relevant vectors
  
  west <- endseason %>%
    html_nodes('#divs_standings_W .right , #divs_standings_W .left') %>%
    html_text()
  
  wteams <- west[seq(8,120,8)]
  wwins <- west[seq(9,121,8)]
  wlosses <- west[seq(10,122,8)]
  wwinpct <- west[seq(11,123,8)]
  
  #create tibble of first iteration set to be bound with other iterations
  
  win_loss <- rbind(win_loss, 
                    data_frame(teams = wteams, wins = as.numeric(wwins), losses = as.numeric(wlosses), winpct = as.numeric(wwinpct)))
  win_loss <- rbind(win_loss, 
                    data_frame(teams = eteams, wins = as.numeric(ewins), losses = as.numeric(elosses), winpct = as.numeric(ewinpct)))
  
  year <- c(year, rep(i,30))
}


for(i in c(2003:2009, 2011:2014)){
endseason <- read_html("http://www.basketball-reference.com/leagues/NBA_" %+% i %+% ".html")

#extract east relevant statistics then classify into relevant vectors

east <- endseason %>%
  html_nodes('#divs_standings_E .right , #divs_standings_E .left') %>%
  html_text()
east <- c("Teams", east)

eteams <- east[seq(9,128,8)]
ewins <- east[seq(10,128,8)]
elosses <- east[seq(11,128,8)]
ewinpct <- east[seq(12,128,8)]

#extract west relevant statistics then classify into relevant vectors

west <- endseason %>%
  html_nodes('#divs_standings_W .right , #divs_standings_W .left') %>%
  html_text()
west <- c("Teams", west)

wteams <- west[seq(9,128,8)]
wwins <- west[seq(10,128,8)]
wlosses <- west[seq(11,128,8)]
wwinpct <- west[seq(12,128,8)]

#create tibble of second iteration set to be bound with other iterations

win_loss <- rbind(win_loss, 
                  data_frame(teams = wteams, wins = as.numeric(wwins), losses = as.numeric(wlosses), winpct = as.numeric(wwinpct)))
win_loss <- rbind(win_loss, 
                  data_frame(teams = eteams, wins = as.numeric(ewins), losses = as.numeric(elosses), winpct = as.numeric(ewinpct)))

year <- c(year, rep(i,30))
}

win_loss <- add_column(win_loss, year)
rm(year,wteams,wwins,wlosses,wwinpct,eteams,ewins,elosses,ewinpct,east,west,endseason)
win_loss <- arrange(win_loss, year, as.character(teams))

#scrape win-loss during all star break http://www.basketball-reference.com/friv/standings.fcgi

#take dates needed to set all-star break into day and year vectors
day <- substring(dates,5, regexpr(",", dates) -1)[1:14]
season <- substring(dates,regexpr(",", dates) + 2, 14)[1:14]

year <- c()
mid_season <- data_frame()

for(i in 1:14){
  prestar <- read_html("http://www.basketball-reference.com/friv/standings.fcgi?month=2" %+%
                         "&day=" %+% day[i] %+% 
                         "&year=" %+% season[i] %+% "&lg_id=NBA")
  
  #extract east relevant statistics then classify into relevant vectors
  
  east <- prestar %>% 
    html_nodes('#standings_e .left , #standings_e .right') %>%
    html_text()
  
  eteams <- east[seq(10,144,9)]
  ewins <- east[seq(11,144,9)]
  elosses <- east[seq(12,144,9)]
  ewinpct <- east[seq(13,144,9)]
  
  #extract west relevant statistics then classify into relevant vectors
  
  west <- prestar %>% 
    html_nodes('#standings_w .left , #standings_w .right') %>%
    html_text()
  
  wteams <- west[seq(10,144,9)]
  wwins <- west[seq(11,144,9)]
  wlosses <- west[seq(12,144,9)]
  wwinpct <- west[seq(13,144,9)]
  
  #create tibble of first iteration set to be bound with other iterations
  
  mid_season <- rbind(mid_season, 
                    data_frame(teams = wteams, wins = as.numeric(wwins), losses = as.numeric(wlosses), winpct = as.numeric(wwinpct)))
  mid_season <- rbind(mid_season, 
                      data_frame(teams = eteams, wins = as.numeric(ewins), losses = as.numeric(elosses), winpct = as.numeric(ewinpct)))
  
  year <- c(year, rep(season[i], 30))
}

year <- as.numeric(year)

mid_season <- add_column(mid_season, year)
rm(year,wteams,wwins,wlosses,wwinpct,eteams,ewins,elosses,ewinpct,east,west, prestar)
mid_season <- arrange(mid_season, year, teams)

#subtract to find post-break record, convert to percentage
teams <- win_loss$teams
wins <- win_loss$wins - mid_season$wins
losses <- win_loss$losses - mid_season$losses
year <- win_loss$year
winpct <- wins/(wins+losses)

playoffs <- grepl("\\*", teams)

post_break <- data_frame(teams, wins, losses, winpct, year)
post_break <- post_break[playoffs,]
win_loss <- win_loss[playoffs,]

rm(teams,wins, losses, year, winpct, playoffs, mid_season, dates, day, season)

#scrape playoff number of wins
year <- c()
playoffs <- c()

#pulls out all series and series results
for(i in 2003:2016){
  postseason <- read_html("http://www.basketball-reference.com/playoffs/NBA_" %+% i %+% ".html")
  
  series <- postseason %>% 
    html_nodes('#all_playoffs td:nth-child(2)') %>%
    html_text()
  
  playoffs <- c(playoffs, series)
  year <- c(year, rep(i, length(series)))
}

playoffs <- data_frame(series = playoffs[nchar(playoffs) > 13], year = year[nchar(playoffs) > 13])

#converts series results into game win totals

winner <- substring(playoffs$series, 
                     1,
                     regexpr(" over ", playoffs$series)-1)

loser <- substring(playoffs$series, 
                        regexpr(" over ", playoffs$series) + 6, 
                        regexpr("\\(", playoffs$series) - 2)

wins <- c(substring(playoffs$series, 
                    regexpr("\\(", playoffs$series) +1, 
                    regexpr("\\(", playoffs$series) +1),
          substring(playoffs$series,
                    regexpr("\\)", playoffs$series) -1,
                    regexpr("\\)", playoffs$series) -1))

losses <- c(substring(playoffs$series,
                      regexpr("\\)", playoffs$series) -1,
                      regexpr("\\)", playoffs$series) -1),
            substring(playoffs$series, 
                      regexpr("\\(", playoffs$series) +1, 
                      regexpr("\\(", playoffs$series) +1))

wins <- as.numeric(wins)
losses <- as.numeric(losses)

results <- data_frame(teams = c(winner,loser), wins, losses, year = c(playoffs$year,playoffs$year))

playoffs <- results %>% group_by(teams, year) %>% summarise(wintotal = sum(wins), losstotal = sum(losses))

playoffs <- arrange(playoffs, year, teams)

rm(results, winner, wins, losses, loser, series, postseason, year)

#combine to form final dataset
playoffpct <- playoffs$wintotal/(playoffs$wintotal + playoffs$losstotal)
seed <- as.numeric(substring(win_loss$teams, regexpr("([0-9])", win_loss$teams), regexpr("([0-9])", win_loss$teams)))
final <- data_frame(teams = playoffs$teams, seed, year = playoffs$year, playoffwins = playoffs$wintotal, season = win_loss$winpct, allstar = post_break$winpct, playoffpct)
rm(playoffs,post_break,win_loss,i, playoffpct)
