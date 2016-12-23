big_data <- readRDS("big_data.rds")

library(dplyr)

#eliminate players that where not rookies in 1980-1981

##Get a data frame with only the 79-80 season
NBA_80 <- filter(big_data, Season == "1979-80")

##Get the players that played in the 80s
players80<- unique(NBA_80$Player)

##Eliminate rows with players that played in season 79-80, with that we have players with all their seasons (Rookies from 80-81)

new_NBA <- big_data

`%not%` <- Negate(`%in%`)

new_NBA <- filter(new_NBA, Player %not% players80)

##Eliminate players that have not retired up to season 2015-2016

NBA_16 <- filter(big_data, Season == "2015-16")
players16<- unique(NBA_16$Player)
new_NBA <- filter(new_NBA, Player %not% players16)

#Get number of seasons of players, mean and max PER, and rookie age

NBA_number_Seasons <- group_by(new_NBA, Player)

NBA_number_Seasons <- summarise(NBA_number_Seasons, N = n(), MEAN_PER = mean(PER), MAX_PER = max(PER), Rookie_age = min(Age))

NBA_number_Seasons <- filter(NBA_number_Seasons, N < 50)

#Histograms of number of seasons and age as a rookie

hist(NBA_number_Seasons$N)
hist(NBA_number_Seasons$Rookie_age)
hist(NBA_number_Seasons$MEAN_PER)


#look for relationships

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


pairs(NBA_number_Seasons[,-1], panel = panel.smooth, upper.panel = panel.cor)
pairs(filter(NBA_number_Seasons, Rookie_age < 25)[,-1], panel = panel.smooth, upper.panel = panel.cor)
