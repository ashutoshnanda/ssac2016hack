setwd("~/Developer/ssac2016hack")

library(zoo)
library(dplyr)

game.data <- read.csv("hackathon data/UnderArmor2016.csv", stringsAsFactors = F)
game.data$gameclock.minute <- sapply(game.data$gameclock, function(x) {
  pos.of.colon <- gregexpr(pattern =':', x)[[1]][1]
  value <- as.integer(substr(x, 1, pos.of.colon - 1))
  ifelse(value == -1, NA, value)
})
game.data$gameclock.second <- sapply(game.data$gameclock, function(x) {
  pos.of.colon <- gregexpr(pattern =':', x)[[1]][1]
  value <- as.integer(substr(x, pos.of.colon + 1, nchar(x)))
  ifelse(value == -1, NA, value)
})
game.data$gameclock.minute <- na.locf(game.data$gameclock.minute)
game.data$gameclock.second <- na.locf(game.data$gameclock.second)
game.data$gameclock.total <- game.data$gameclock.minute * 60 + game.data$gameclock.second
game.data$z <- game.data$z / 3
game.data$x <- game.data$x / 3
full.play.data <- game.data %>% subset(!(playtype %in% c(2, 5, 8)))
time.of.play <- game.data %>% 
                group_by(play) %>% 
                summarize(start = max(gameclock.total[gameclock.total >= 0], na.rm = T),
                          end = min(gameclock.total[gameclock.total >= 0], na.rm = T))
frame.data <- game.data %>% group_by(play) %>% summarize(num.frames = length(unique(frame)))
play.types <- game.data %>% group_by(play) %>% summarize(type = nth(playtype, n = 1))

