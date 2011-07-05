## Ryan Elmore
## Date:  2 July 2011
## Description:  Trying to find the minimum number of pitches per inning

project.path <- "~/Side_Projects/Sports/Pitch_Count/"

## Dependencies:
source(paste(project.path, "src/load.R", sep=""))

test.html <- paste(project.path, "data/test.html", sep="")
base.url <- "http://www.baseball-reference.com/boxes"
team <- "COL"
date.url <- paste(team, "20100411", sep="")
full.url <- paste(paste(base.url, team, date.url, sep="/"), "0.shtml", sep="")

table.stats <- readHTMLTable(full.url)
pbyp.stats <- table.stats[["play_by_play"]]

stats.df <- pbyp.stats[!is.na(as.numeric(pbyp.stats$Out)), 
                       c("Inn", "Out", "Pit(cnt)Sequence", "Pitcher")]
                                              
stats.df <- table.stats[["play_by_play"]][, c("Inn", 
                                              "Out",
                                              "Pit(cnt)Sequence",
                                              "Pitcher")]

stats.df$Pitches <- unlist(lapply(strsplit(stats.df$"Pit(cnt)Sequence", ","), 
                           function(x) as.numeric(x[1])))

pc.run.length <- rle(stats.df$Pitches)

teams <- c("COL")
year <- c("2010")
month <- seq(04, 10, by=1)
# [1] "Inn"              "Score"            "Out"              "RoB"             
# [5] "Pit(cnt)Sequence" "R/O"              "@Bat"             "Batter"          
# [9] "Pitcher"          "wWPA"             "wWE"              "Play Description"

url.tmp <- paste("http://www.nba.com/", team, sep="")
nba.url <- paste(url.tmp, "/stats/", sep="")
if(team %in% c("bulls", "raptors")) {
  table.stats <- readHTMLTable(nba.url)
  stats.one <- table.stats[1]$`NULL`[-1, ]
  names(stats.one) <- table.stats[1]$`NULL`[1, ]
  stats.one$Player[stats.one$Player == "Team Averages"] <- "Team Statistics"
  stats.two <- table.stats[2]$`NULL`[-1, ]
  names(stats.two) <- table.stats[2]$`NULL`[1, ]
  stats.two$Player[stats.two$Player == "Team Totals"] <- "Team Statistics"
  df.team <- merge(stats.one, stats.two[, -(2:3)], by.x="Player", 
                   by.y="Player")
  df.team$team <- team
}
else {
  doc <- htmlTreeParse(nba.url, useInternalNodes=T)
  nset.stats <- getNodeSet(doc, "//table[@class=' gSGTable']")    
  table.stats <- lapply(nset.stats, readHTMLTable, header=F)
  stats.one <- table.stats[[1]][-(1:3), ]
  names(stats.one) <- table.stats[[1]][3, ]
  stats.one$Player[stats.one$Player == "Team Averages"] <- "Team Statistics"
  nRecords <- dim(stats.one)[1]
  stats.two <- table.stats[[2]][-(1:3), -(2:3)]
  names(stats.two) <- table.stats[[2]][3, -(2:3)]
  stats.two$Player[stats.two$Player == "Team Totals"] <- "Team Statistics"
  df.team <- merge(stats.one[-nRecords, ], stats.two[-nRecords, ], 
                   by.x="Player", by.y="Player")
  df.team$team <- team
}
