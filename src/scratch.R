## Ryan Elmore
## Date:
## Scratch pad 

## Testing
full.url <- "http://www.baseball-reference.com/boxes/COL/COL201007310.shtml"

table.stats <- readHTMLTable(full.url)
pbyp.stats <- table.stats[["play_by_play"]]
stats.df <- pbyp.stats[!is.na(as.numeric(pbyp.stats$Out)), 
                       c("Inn", "Out", "Pit(cnt)Sequence", 
                         "Pitcher")]
stats.df$Pitches <- unlist(lapply(strsplit(
                           stats.df$"Pit(cnt)Sequence", ","), 
                           function(x) as.numeric(x[1])))
stats.df$Inning <- unlist(lapply(strsplit(stats.df$Inn, "[a-z]"), 
                          function(x) as.numeric(x[2])))
stats.agg <- aggregate(stats.df[, "Pitches"], 
                       by=list(stats.df$Inning), FUN="sum")

stats.df <- pbyp.stats[!is.na(as.numeric(pbyp.stats$Out)), 
                       c("Inn", "Out", "Pit(cnt)Sequence", "Pitcher")]
                                              

stats.df$Pitches <- unlist(lapply(strsplit(stats.df$"Pit(cnt)Sequence", ","), 
                           function(x) as.numeric(x[1])))
stats.df$Inning <- unlist(lapply(strsplit(stats.df$Inn, "[a-z]"), 
                          function(x) as.numeric(x[2])))

stats.agg <- aggregate(stats.df[, "Pitches"], by=list(stats.df$Inning), 
                       FUN="sum")

if(sum(stats.agg$x==6) != 0){
  stats.agg.2 <- aggregate(stats.df[, "Pitches"], by=list(stats.df$Inning), 
                           FUN="length")
  if(stats.agg.2$x[stats.agg$x==6]==6) 
  print (full.url)}

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
