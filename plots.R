library(ggplot2)
library(dplyr)
library(animation)

setwd("~/Developer/ssac2016hack")

y.labels <- c("BEZ", "10", "30", "50", "30", "10", "BEZ")
x.breaks <- c(0, 160/6 - 18.5 / 3, 160/6 +  18.5 / 3, 160 / 3)

extract.data <- function(play.number, frame.number, data = game.data) {
  data %>% subset(play == play.number & frame == frame.number)
}

find_hull <- function(df){
  df[chull(df$z, df$x), ]
}

plot.frame <- function(p, f, sub = NULL) {
  df <- if(is.null(sub)) {extract.data(p, f)} else {sub}
  title <- paste(c("Play:", "Frame:", "Game Clock:"), 
                 c(unique(df$play), unique(df$frame), unique(df$gameclock)), 
                 collapse = "; ")
  pos.of.ball <- (df %>% subset(hasball == 1))
  ball.x <- pos.of.ball$z
  ball.y <- pos.of.ball$x
  average.number <- df %>% group_by(team) %>% summarize(a = mean(player))
  defense <- if(average.number$a[1] > average.number$a[2]) {
    average.number$team[1] 
  } else {
    average.number$team[2] 
  }
  defense.data <- df %>% subset(team == defense)
  ggplot() + geom_point(aes(x = df$z, y = df$x, color = factor(df$team))) + 
    geom_polygon(data = find_hull(defense.data), aes(x = z, y = x), alpha = 0.2, fill = "green") +
    annotate("text", x = ball.x, y = ball.y, label = "*", color = "brown", size = 5) +
    geom_hline(aes(yintercept = unique(df$los)), color = "blue") + 
    geom_hline(aes(yintercept = unique(df$fdl)), color = "yellow") + 
    theme(panel.background = element_rect(fill = "#395D33"),
          panel.grid.major.x = element_line(linetype = "dotted"),
          legend.position = "none") + 
    coord_cartesian(xlim = c(0, 160 / 3), ylim = c(-10, 110)) + 
    scale_y_continuous(breaks = seq(-10, 110, 20), 
                       labels = y.labels) +
    scale_x_continuous(minor_breaks = NULL, breaks = x.breaks, labels = NULL) +
    ggtitle(title) + ylab("Yard Line") + xlab(NULL)
}

#plot.play <- function(p, file.name, data = game.data, frame.info = frame.data) {
#  num.frames <- (frame.info %>% subset(play == p))$num.frames
#  num.frames <- 120
#  saveGIF({
#    ani.options('interval', 0.1)
#    for(i in 1:num.frames) {
#      print(i)
#      print(plot.frame(p, i))
#      ani.pause()
#    }
#  }, movie.name = file.name)
#}

plot.play <- function(p, data = game.data, frame.info = frame.data, clean = T) {
  file.name <- paste0("play", p, ".gif")
  num.frames <- (frame.info %>% subset(play == p))$num.frames
  picks <- rev(seq(num.frames, 1, -30))
  print("===================")
  for(i in picks) {
    print(i)
    suppressWarnings(ggsave(paste0("frame-", p, "-", i, ".png"), plot.frame(p, i), 
                            width = 4, height = 4))
  }
  print("===================")
  files.parts <- paste(sapply(picks, function(i){paste0("frame-", p, "-", i, ".png")}), collapse = " ")
  system(paste("convert -delay 50", files.parts, file.name))
  if(clean) {
    unlink("frame[0-9]*-[0-9]*.png")
  }
}

make.all.plots <- function() {
  plays <- unique(full.play.data$play)
  for(i in plays) {
    print(i)
    plot.play(i)
  }
}

