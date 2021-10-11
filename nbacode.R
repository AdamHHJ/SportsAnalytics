library(googleVis)
library("dplyr")
library(SportsAnalytics)
library(rvest)
library(tidyverse)
library(gdata)

nba07_08 <- fetch_NBAPlayerStatistics("07-08")

save(nba07_08, file="nba.RData")

load(file="nba.RData")

names(nba07_08)

#team Philadelphia 76ers
sixers<-subset(nba07_08, Team == 'PHI')

#1. Which player has the best three point percentage? 
sixers$Threespercentage<-sixers$ThreesMade/sixers$ThreesAttempted
sixers$Name[which.max(sixers$Threespercentage)]
sixers[is.na(sixers)] <- 0

c1 <-gvisLineChart(
  sixers,
  "Name",
  "Threespercentage",
)

plot(c1)
print(c1, file = "threespercentage.html")

#2. Which player has played the largest number of minutes?
sixers$Name[which.max(sixers$TotalMinutesPlayed)]
c2 <-gvisBarChart(
  sixers,
  xvar = "Name",
  yvar = c("TotalMinutesPlayed"),
  options=list(
    legend="top")
)
plot(c2)
print(c2, file = "TotalMinutesPlayed.html")

#3.Which player has the most "Steals"?
sixers$Name[which.max(sixers$Steals)]
c3 <-gvisColumnChart(
  sixers,
  xvar = "Name",
  yvar = "Steals"
  
)
plot(c3)
print(c3, file = "steals.html")

#4. who has most Points/Free points Per Game?
sixers$PTG<-sixers$TotalPoints/sixers$GamesPlayed
sixers$FPG<-sixers$FreeThrowsMade/sixers$GamesPlayed
sixers$Name[which.max(sixers$PTG)]
sixers$Name[which.max(sixers$FPG)]
c4 <-gvisAreaChart(
  sixers,
  xvar = "Name",
  yvar = c("PTG","FPG")
  
)
plot(c4)

#5 Rebounds and assists
a<-data.frame(name=sixers$Name,rebounds=sixers$TotalRebounds)
b<-data.frame(name=sixers$Name,rebounds=sixers$Assists)
c5 <-gvisPieChart(
  a)
plot(c5)

c_5 <-gvisPieChart(
  b)
plot(c_5)
#6 five teams for the 2007-2008 season that have the most wins in descending order
html <- read_html("http://dougstats.com/07-08.html")
pre <- html_node(html, "pre")
lines <- unlist(str_split(html_text(pre), "\n"))
lines <- lines[5:length(lines)]
lines <- lines[lines!=""]
teams <- c()
W <- c()
L <- c()
for(i in 1:length(lines)){
  arr <- unlist(strsplit(lines[i], " "))
  arr <- arr[arr!=""]
  teams <- c(teams, arr[2], arr[9])
  W <- c(W, arr[3], arr[10])
  L <- c(L, arr[4], arr[11])
}
dat <- data.frame(team=teams, W=W, L=L)
data<-dat[order(dat$W, decreasing= T),][1:5,]
data$Conference<-c("Eastern","Eastern","Western","Western","Western")
data
c6<-gvisTable(data)
plot(c6)
print(c6, file = "wintop5.html")

#7 World Champion Countries 
html2<-read_html("https://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
table<-html_node(html2, "table")
df <- html_table(table)
df <- df[, -2]
colnames(df) <- c("WorldCup", "Gold", "Silver", "Bronze")
df[df==""]<- NA
df <- na.omit(df)
df <- df[-1, ]
s<-as.data.frame(table(df$Gold))
names(s)<-c("Country", "Times")

s$Country<-c("Argentina","Brazil","Serbia","Russia","Spain","United States","Croatia")
s
c7<-gvisGeoChart(s,
                 locationvar = "Country",
                 colorvar = "Times")
plot(c7)

