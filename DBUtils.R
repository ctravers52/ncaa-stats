# Utility functions for app to operate
library(rvest)
library(dplyr)
library(reactable)
# this function returns the scraped college football data
get_offensive_data <- function() {
  
  url <- "http://www.cfbstats.com/2019/leader/national/team/offense/split01/category09/sort01.html"
  
  # read the html
  html <- read_html(url)
  
  # table to import - tells rvest to find the table node in the html code - for webpages with multiple
  # tables on, this will be a list. Use table[i] to find the i'th table in this case
  tab <- html_nodes(html, "table")
  
  # reconstruct table out of the found nodes
  htmltable <- html_table(tab, header = TRUE)
  
  # lets R make a data frame from the html table
  dat <- data.frame(htmltable)
  
  # make a new column for TD & FG
  dat[, 'TDFG'] <- dat[, 'TD'] + dat[, 'FG']
  
  # make a new column for TDratio
  dat[, 'TDratio'] <- round((dat[, 'TD'] / dat[,'TDFG']),digits = 2) 
  
  # remove full stops in column names
  names(dat) <- gsub(x = names(dat), pattern = "\\.", replacement = "")
  
  return(dat)
  
}
get_data <- function(url) {
  
  # read the html
  html <- read_html(url)
  
  # table to import - tells rvest to find the table node in the html code - for webpages with multiple
  # tables on, this will be a list. Use table[i] to find the i'th table in this case
  tab <- html_nodes(html, "table")
  
  # reconstruct table out of the found nodes
  htmltable <- html_table(tab, header = TRUE)
  
  # lets R make a data frame from the html table
  dat <- data.frame(htmltable)
  
  return(dat)
}
# filter the data based on passing/rushing efficiency
efficiency_filter <- function(data, option, threshold = NULL) {
  
  # filter the data
  if (option %in% names(data)) {
    filtered_data <- data[which(data[, option] >= threshold), ]
  }
  else {
    filtered_data <- data
  }
  
  return(filtered_data)
  
}
# this function takes an R data frame and creates a smartlooking table in the app
create_offense_table <- function(data) {
  
  # pnly include some columns
  data <- data[,c("Name", "G", "TD", "FG", "TDFG", "TDratio")]
  
  # create reactable table
  reactable(data, bordered = TRUE, striped = TRUE, defaultPageSize = 150, defaultColDef = colDef(align = "center"),
            columns = list(G = colDef(header = "Games"),
                           Name = colDef(header = "Team Name", style = JS("function(rowInfo) {
                                                                              var value = rowInfo.rownames
                                                                              return {fontWeight: 'bold', color: '#4169E1'}
                                                                            }"), align = "left"),
                           TD = colDef(header = "TD"),
                           FG = colDef(header = "FG"),
                           TDFG = colDef(header = "TDFG"),
                           TDratio = colDef(header = "TDratio")
            ))
  
}
get_defensive_data <- function() {
  
  url <- "http://www.cfbstats.com/2019/leader/national/team/offense/split01/category12/sort01.html"
  
  
  # read the html
  html <- read_html(url)
  
  # table to import - tells rvest to find the table node in the html code - for webpages with multiple
  # tables on, this will be a list. Use table[i] to find the i'th table in this case
  tab <- html_nodes(html, "table")
  
  # reconstruct table out of the found nodes
  htmltable <- html_table(tab, header = TRUE)
  
  # lets R make a data frame from the html table
  dat <- data.frame(htmltable)
  
  # make a new column for Turnovers Gained per game
  dat[, 'ToGPG'] <- round((dat[, 'Total.Gain'] / (dat[, 'G'])),digits = 2)
  
  # make a new column for TD & FG
  dat[, 'ToLPG'] <- round((dat[, 'Total.Lost'] / dat[, 'G']),digits =2)
  
  # remove full stops in column names
  names(dat) <- gsub(x = names(dat), pattern = "\\.", replacement = "")
  
  return(dat)
  
}
get_data <- function(url) {
  
  # read the html
  html <- read_html(url)
  
  # table to import - tells rvest to find the table node in the html code - for webpages with multiple
  # tables on, this will be a list. Use table[i] to find the i'th table in this case
  tab <- html_nodes(html, "table")
  
  # reconstruct table out of the found nodes
  htmltable <- html_table(tab, header = TRUE)
  
  # lets R make a data frame from the html table
  dat <- data.frame(htmltable)
  
  return(dat)
}

# this function takes an R data frame and creates a smartlooking table in the app
create_defense_table <- function(data) {
  
  # pnly include some columns
  data <- data[,c("Name", "G", "TotalGain", "ToGPG", "TotalLost", "ToLPG")]
  
  # create reactable table
  reactable(data, bordered = TRUE, striped = TRUE, defaultPageSize = 150, defaultColDef = colDef(align = "center"),
            columns = list(G = colDef(header = "Games"),
                           Name = colDef(header = "Team Name", style = JS("function(rowInfo) {
                                                                              var value = rowInfo.rownames
                                                                              return {fontWeight: 'bold', color: '#4169E1'}
                                                                            }"), align = "left"),
                           TotalGain = colDef(header = "TO Forced"),
                           ToGPG = colDef(header = "Forced per game"),
                           TotalLost = colDef(header = "TO Lost"),
                           ToLPG = colDef(header = "Lost per game")
            ))
  
}

get_quick_stats_data <- function() {
  
  # get attack stats of championship teams
  offense_dat <- get_data("http://www.cfbstats.com/2019/leader/national/team/offense/split01/category09/sort01.html")
  
  # get defense stats of championship teams
  defense_data <- get_data("http://www.cfbstats.com/2019/leader/national/team/offense/split01/category12/sort01.html")
  
  # get penalty data
  penalty_data <- get_data("http://cfbstats.com/2019/leader/national/team/offense/split01/category14/sort01.html")
  
  # get sack data
  sack_D_data <- get_data("http://cfbstats.com/2019/leader/national/team/offense/split01/category20/sort01.html")
  
  # get sacks allowed data
  sack_lost_data <- get_data("http://cfbstats.com/2019/leader/national/team/defense/split01/category20/sort01.html")
  
  # make a new column for turnovers gained per game
  defense_data[, 'ToGPG']<- round((defense_data[,'Total.Gain']/defense_data[,'G']),digits = 2)
  
  # make a new column for turnovers lost per game
  defense_data[, 'ToLPG']<- round((defense_data[,'Total.Lost']/defense_data[,'G']),digits =2)
  
  # make a new column for pass efficiency
  offense_dat[, 'TDFG'] <- offense_dat[, 'TD'] + offense_dat[, 'FG']
  
  # make a new column for rush efficiency
  offense_dat[, 'TDratio'] <- round((offense_dat[, 'TD'] / offense_dat[, 'TDFG']),digits = 2)
  
  # distinguish between sacks lost and defensive sacks
  names(sack_lost_data)[which(names(sack_lost_data) == "Sacks.G")] <- "SacksLost.G"
  
  # get data from all five tables onto one table
  offense_dat_new <- offense_dat[order(offense_dat[,'Name']),]
  defense_data_new <- defense_data[order(defense_data[,'Name']),]
  penalty_data_new <- penalty_data[order(penalty_data [,'Name']),]
  sack_D_data_new <- sack_D_data[order(sack_D_data [,'Name']),]
  sack_lost_data_new <- sack_lost_data[order(sack_lost_data [,'Name']),]
  
  full_data <- cbind(offense_dat_new, defense_data_new, penalty_data_new, sack_D_data_new, sack_lost_data_new)
  
  # Only extract columns we want
  new_full_data <- full_data[, c('Name', 'G', 'ToGPG', 'ToLPG', 'TDratio', 'Yards.G', 'Sacks.G', 'SacksLost.G')]
  
  # remove full stops in column names
  names(new_full_data) <- gsub(x = names(new_full_data), pattern = "\\.", replacement = "")
  
  return(new_full_data)
  
}
add_team_columns <- function(team_table, opposition_table) {
  
  # here team table is the table of the team in question, and opposition_table is the table of the other team
  team_table[,'expSacks' ] <- round((((team_table[,'SacksG']/2.2)*opposition_table[,'SacksLostG'])),digits = 2)
  
  #same as line above, but with turnovers
  team_table[,'expTO'] <- round((((team_table[,'ToLPG']/1.5*opposition_table[,'ToGPG']))),digits = 2)
  
  # which calculates the two new columns we want
  return(team_table)  
}

# this function takes an R data frame and creates a smartlooking table in the app
create_quick_stats_table <- function(data) {
  
  # pnly include some columns
  data <- data[,c("Name", "G", "ToGPG", "ToLPG", "TDratio","YardsG", 'SacksG', 'SacksLostG', 'expSacks', 'expTO')]
  
  # create reactable table
  reactable(data, bordered = TRUE, striped = TRUE, defaultPageSize = 150, defaultColDef = colDef(align = "center"),
            columns = list(G = colDef(header = "Games"),
                           Name = colDef(header = "Team Name", style = JS("function(rowInfo) {
                                                                              var value = rowInfo.rownames
                                                                              return {fontWeight: 'bold', color: '#4169E1'}
                                                                            }"), align = "left"),
                           ToGPG = colDef(header = "TO Gained per G"),
                           ToLPG = colDef(header = "TO Lost per G"),
                           TDratio = colDef(header = "TDratio"),
                           YardsG = colDef(header = "PenYPG"),
                           SacksG = colDef(header = "SacksD"),
                           SacksLostG = colDef(header = "SacksLost"),
                           expSacks = colDef(header = "expected Sacks"),
                           expTO =colDef(header = "expected Turnovers")
            ))
  
}

get_player_data <- function(Name) {
  year <- "2019"
  if (Name == "Alabama") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/8/scoring/index.html", sep = "")))
  }
  # repeat for rest of teams
  if (Name == "LSU") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/365/scoring/index.html", sep = "")))
  }
  if (Name == "Georgia") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/257/scoring/index.html", sep = "")))
  }
  if (Name == "Auburn") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/37/scoring/index.html", sep = "")))
  }
  if (Name == "Mississippi") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/433/scoring/index.html", sep = "")))
  }
  if (Name == "Mississippi State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/430/scoring/index.html", sep = "")))
  }
  if (Name == "Texas A&M") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/697/scoring/index.html", sep = "")))
  }
  if (Name == "Tennessee") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/694/scoring/index.html", sep = "")))
  }
  if (Name == "Missouri") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/434/scoring/index.html", sep = "")))
  }
  if (Name == "Florida") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/235/scoring/index.html", sep = "")))
  }
  if (Name == "South Carolina") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/648/scoring/index.html", sep = "")))
  }
  if (Name == "Kentucky") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/334/scoring/index.html", sep = "")))
  }
  if (Name == "Arkansas") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/31/scoring/index.html", sep = "")))
  }
  if (Name == "Vanderbilt") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/736/scoring/index.html", sep = "")))
  }
  if (Name == "Ohio State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/518/scoring/index.html", sep = "")))
  }
  if (Name == "Penn State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/539/scoring/index.html", sep = "")))
  }
  if (Name == "Maryland") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/392/scoring/index.html", sep = "")))
  }
  if (Name == "Michigan") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/418/scoring/index.html", sep = "")))
  }
  if (Name == "Iowa") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/312/scoring/index.html", sep = "")))
  }
  if (Name == "Nebraska") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/463/scoring/index.html", sep = "")))
  }
  if (Name == "Indiana") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/306/scoring/index.html", sep = "")))
  }
  if (Name == "Wisconsin") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/796/scoring/index.html", sep = "")))
  }
  if (Name == "Northwestern") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/509/scoring/index.html", sep = "")))
  }
  if (Name == "Rutgers") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/587/scoring/index.html", sep = "")))
  }
  if (Name == "Michigan State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/416/scoring/index.html", sep = "")))
  }
  if (Name == "Minnesota") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/428/scoring/index.html", sep = "")))
  }
  if (Name == "Purdue") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/559/scoring/index.html", sep = "")))
  }
  if (Name == "Illinois") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/301/scoring/index.html", sep = "")))
  }
  if (Name == "TCU") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/698/scoring/index.html", sep = "")))
  }
  if (Name == "Oklahoma") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/522/scoring/index.html", sep = "")))
  }
  if (Name == "Texas Tech") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/700/scoring/index.html", sep = "")))
  }
  if (Name == "Oklahoma State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/521/scoring/index.html", sep = "")))
  }
  if (Name == "Baylor") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/51/scoring/index.html", sep = "")))
  }
  if (Name == "West Virginia") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/768/scoring/index.html", sep = "")))
  }
  if (Name == "Iowa State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/311/scoring/index.html", sep = "")))
  }
  if (Name == "Texas") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/703/scoring/index.html", sep = "")))
  }
  if (Name == "Kansas State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/327/scoring/index.html", sep = "")))
  }
  if (Name == "Kansas") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/328/scoring/index.html", sep = "")))
  }
  if (Name == "Oregon") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/529/scoring/index.html", sep = "")))
  }
  if (Name == "Arizona") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/29/scoring/index.html", sep = "")))
  }
  if (Name == "UCLA") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/110/scoring/index.html", sep = "")))
  }
  if (Name == "Stanford") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/674/scoring/index.html", sep = "")))
  }
  if (Name == "California") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/107/scoring/index.html", sep = "")))
  }
  if (Name == "Arizona State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/28/scoring/index.html", sep = "")))
  }
  if (Name == "Washington") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/756/scoring/index.html", sep = "")))
  }
  if (Name == "Oregon State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/528/scoring/index.html", sep = "")))
  }
  if (Name == "Colorado") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/157/scoring/index.html", sep = "")))
  }
  if (Name == "Washington State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/754/scoring/index.html", sep = "")))
  }
  if (Name == "Utah") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/732/scoring/index.html", sep = "")))
  }
  if (Name == "USC") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/657/scoring/index.html", sep = "")))
  }
  if (Name == "Wake Forest") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/749/scoring/index.html", sep = "")))
  }
  if (Name == "North Carolina State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/490/scoring/index.html", sep = "")))
  }
  if (Name == "Georgia Tech") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/255/scoring/index.html", sep = "")))
  }
  if (Name == "Vanderbilt") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/736/scoring/index.html", sep = "")))
  }
  if (Name == "North Carolina") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/457/scoring/index.html", sep = "")))
  }
  if (Name == "Miami (Florida)") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/415/scoring/index.html", sep = "")))
  }
  if (Name == "Boston College") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/67/scoring/index.html", sep = "")))
  }
  if (Name == "Syracuse") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/688/scoring/index.html", sep = "")))
  }
  if (Name == "Florida State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/234/scoring/index.html", sep = "")))
  }
  if (Name == "Virginia") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/746/scoring/index.html", sep = "")))
  }
  if (Name == "Virginia Tech") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/742/scoring/index.html", sep = "")))
  }
  if (Name == "Vanderbilt") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/736/scoring/index.html", sep = "")))
  }
  if (Name == "Pittsburgh") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/545/scoring/index.html", sep = "")))
  }
  if (Name == "Clemson") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/147/scoring/index.html", sep = "")))
  }
  if (Name == "Duke") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/193/scoring/index.html", sep = "")))
  }
  if (Name == "Louisville") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/367/scoring/index.html", sep = "")))
  }
  if (Name == "Tulane") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/718/scoring/index.html", sep = "")))
  }
  if (Name == "Tulsa") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/719/scoring/index.html", sep = "")))
  }
  if (Name == "Houston") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/288/scoring/index.html", sep = "")))
  }
  if (Name == "SMU") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/663/scoring/index.html", sep = "")))
  }
  if (Name == "Cincinnati") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/140/scoring/index.html", sep = "")))
  }
  if (Name == "UCF") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/128/scoring/index.html", sep = "")))
  }
  if (Name == "Memphis") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/404/scoring/index.html", sep = "")))
  }
  if (Name == "South Florida") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/651/scoring/index.html", sep = "")))
  }
  if (Name == "Navy") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/726/scoring/index.html", sep = "")))
  }
  if (Name == "Temple") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/690/scoring/index.html", sep = "")))
  }
  if (Name == "East Carolina") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/196/scoring/index.html", sep = "")))
  }
  if (Name == "Connecticut") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/164/scoring/index.html", sep = "")))
  }
  if (Name == "Hawai'i") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/277/scoring/index.html", sep = "")))
  }
  if (Name == "Air Force") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/721/scoring/index.html", sep = "")))
  }
  if (Name == "Boise State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/66/scoring/index.html", sep = "")))
  }
  if (Name == "Colorado State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/156/scoring/index.html", sep = "")))
  }
  if (Name == "Fresno State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/96/scoring/index.html", sep = "")))
  }
  if (Name == "Wyoming") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/811/scoring/index.html", sep = "")))
  }
  if (Name == "Nevada") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/466/scoring/index.html", sep = "")))
  }
  if (Name == "New Mexico") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/473/scoring/index.html", sep = "")))
  }
  if (Name == "San Diego State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/626/scoring/index.html", sep = "")))
  }
  if (Name == "San Jose State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/630/scoring/index.html", sep = "")))
  }
  if (Name == "UNLV") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/465/scoring/index.html", sep = "")))
  }
  if (Name == "Utah State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/731/scoring/index.html", sep = "")))
  }
  if (Name == "Akron") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/5/scoring/index.html", sep = "")))
  }
  if (Name == "Ball State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/47/scoring/index.html", sep = "")))
  }
  if (Name == "Bowling Green") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/71/scoring/index.html", sep = "")))
  }
  if (Name == "Buffalo") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/86/scoring/index.html", sep = "")))
  }
  if (Name == "Central Michigan") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/129/scoring/index.html", sep = "")))
  }
  if (Name == "Eastern Michigan") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/204/scoring/index.html", sep = "")))
  }
  if (Name == "Kent State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/331/scoring/index.html", sep = "")))
  }
  if (Name == "Miami (Ohio)") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/414/scoring/index.html", sep = "")))
  }
  if (Name == "Northern Illinois") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/503/scoring/index.html", sep = "")))
  }
  if (Name == "Ohio") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/519/scoring/index.html", sep = "")))
  }
  if (Name == "Toledo") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/709/scoring/index.html", sep = "")))
  }
  if (Name == "Western Michigan") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/774/scoring/index.html", sep = "")))
  }
  if (Name == "Florida Atlantic") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/229/scoring/index.html", sep = "")))
  }
  if (Name == "Charlotte") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/458/scoring/index.html", sep = "")))
  }
  if (Name == "Florida International") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/231/scoring/index.html", sep = "")))
  }
  if (Name == "Louisiana Tech") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/366/scoring/index.html", sep = "")))
  }
  if (Name == "Marshall") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/388/scoring/index.html", sep = "")))
  }
  if (Name == "Middle Tennessee") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/419/scoring/index.html", sep = "")))
  }
  if (Name == "North Texas") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/497/scoring/index.html", sep = "")))
  }
  if (Name == "Old Dominion") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/523/scoring/index.html", sep = "")))
  }
  if (Name == "Rice") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/574/scoring/index.html", sep = "")))
  }
  if (Name == "Southern Mississippi") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/664/scoring/index.html", sep = "")))
  }
  if (Name == "UAB") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/9/scoring/index.html", sep = "")))
  }
  if (Name == "UTEP") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/704/scoring/index.html", sep = "")))
  }
  if (Name == "UTSA") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/706/scoring/index.html", sep = "")))
  }
  if (Name == "Western Kentucky") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/772/scoring/index.html", sep = "")))
  }
  if (Name == "Appalachian State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/27/scoring/index.html", sep = "")))
  }
  if (Name == "Arkansas State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/30/scoring/index.html", sep = "")))
  }
  if (Name == "Coastal Carolina") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/149/scoring/index.html", sep = "")))
  }
  if (Name == "Georgia Southern") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/253/scoring/index.html", sep = "")))
  }
  if (Name == "Georgia State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/254/scoring/index.html", sep = "")))
  }
  if (Name == "Louisiana-Lafayette") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/671/scoring/index.html", sep = "")))
  }
  if (Name == "Louisiana-Monroe") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/498/scoring/index.html", sep = "")))
  }
  if (Name == "South Alabama") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/646/scoring/index.html", sep = "")))
  }
  if (Name == "Texas State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/670/scoring/index.html", sep = "")))
  }
  if (Name == "Troy") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/716/scoring/index.html", sep = "")))
  }
  if (Name == "Army") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/725/scoring/index.html", sep = "")))
  }
  if (Name == "BYU") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/77/scoring/index.html", sep = "")))
  }
  if (Name == "Liberty") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/355/scoring/index.html", sep = "")))
  }
  if (Name == "Massachusetts") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/400/scoring/index.html", sep = "")))
  }
  if (Name == "New Mexico State") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/472/scoring/index.html", sep = "")))
  }
  if (Name == "Notre Dame") {
    return(get_player_td_data(paste("http://www.cfbstats.com/",year,"/team/513/scoring/index.html", sep = "")))
  }
}

get_player_td_data <- function(url) {
  
  
  options(max.print = 100000000)
  
  dat <- get_data(url)
  
  dat <- dat[-nrow(dat),]
  
  #Create column for touchdowns per game
  dat["TDPG"] <- round((dat["TD"]/dat["G"]),digits = 2)
  
  #creates column for running average of TD Yards per game
  dat[, "TDpercent"] <- round((((as.numeric(dat[, "TD"])*2)/(sum(as.numeric(dat[, "TD"]))))),digits = 2)
  
  return(dat)

}

# this function takes an R data frame and creates a smartlooking table in the app
create_players_table <- function(data) {
  
  # pnly include some columns
  data <- data[,c("Name", "Pos", "G", "TD", "TDPG", "TDpercent")]
  
  # create reactable table
  reactable(data, bordered = TRUE, striped = TRUE, defaultPageSize = 150, defaultColDef = colDef(align = "center"),
            columns = list(G = colDef(header = "Games"),
                           Name = colDef(header = "Players Name", style = JS("function(rowInfo) {
                                                                              var value = rowInfo.rownames
                                                                              return {fontWeight: 'bold', color: '#4169E1'}
                                                                            }"), align = "left"),
                           Pos = colDef(header = "Position"),
                           TD = colDef(header = "TD"),
                           TDPG = colDef(header = "TD per Game"),
                           TDpercent =colDef(header = "TD share")
            ))
  
}

