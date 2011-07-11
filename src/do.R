## Ryan Elmore
## Date:  2 July 2011
## Description:  Trying to find the minimum number of pitches per inning

project.path <- "~/Side_Projects/Sports/Pitch_Count/"

## Dependencies:
source(paste(project.path, "src/load.R", sep=""))

test.html <- paste(project.path, "data/test.html", sep="")
base.url <- "http://www.baseball-reference.com/boxes"
teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
           "FLA", "HOU", "KCR", "ANA", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
           "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBD", "TEX", "TOR", "WSN")
years <- 2005:2009
months <- 4:10
days <- 1:31
urls <- c()
for (team in teams){
  for (year in years){
    out.string <- paste(Sys.time(), "--", team, year, sep = " ")
    print(out.string)
    for (month in months){
      for (day in days){
        ifelse(length(as.character(month) == 1), 
          date.url <- paste(team, year, 0, month, day, sep=""),
          date.url <- paste(team, year, month, day, sep="")
        )
        for (i in 0:1){
          full.url <- paste(paste(base.url, team, date.url, sep="/"), i, 
                            ".shtml", sep="")
          try({
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
              if(sum(stats.agg$x==6) != 0){
                stats.agg.2 <- aggregate(stats.df[, "Pitches"], 
                                         by=list(stats.df$Inning), FUN="length")
                if(stats.agg.2$x[stats.agg$x==6]==6){
                  urls <- c(urls, c(full.url))
                  }                            
                }
                
            }, TRUE)
        }
      }
    }
  }  
}

write.table(urls, "../data/outfile.csv", row.names=F, col.names=F)
final.string <- paste(Sys.time(), " -- Finished :)", sep = "")
print(final.string)

# Started
#[1] "2011-07-10 17:34:38 -- ARI 2005"

#[1] "2011-07-10 20:50:19 -- Finished :)"
