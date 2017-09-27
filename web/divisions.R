library("XML")
library("plyr")

#Team Name and Abbreviation Lookup Table
teams <- data.frame(TeamCode =  c("FLUF",
                              "SHSR",
                              "OCTO",
                              "NIBB",
                              "TGIF",
                              "ATOM",
                              "ROBO",
                              "MEGA",
                              "AFB",
                              "DMPS",
                              "BUTT",
                              "PHSH"),
                    TeamName = c("The Fluffy Bunnies",
                                 "Shelbyville Shittysauruses",
                                 "Giant Octopus",
                                 "The Nibblonians",
                                 "Team GoInsanelyFast",
                                 "Springfield Atoms",
                                 "Robot Unicorn",
                                 "Mega Shark",
                                 "Nellis A.F.B. Boomers",
                                 "Dreadmetal Pumaskulls",
                                 "Butthole Surfers",
                                 "Philadelphia Shenanihads"))

#Scrape Division Page:
division.page <- htmlParse("http://games.espn.com/ffl/standings?leagueId=45342&seasonId=2017")
division.page <- readHTMLTable(division.page,stringAsFactors = FALSE)

#Gets Teams Divisions and Record
div.table <- as.data.frame(division.page[2]) 

divisions <- data.frame(TeamName = div.table[3:8,1], 
                        Division = div.table[1,1], 
                        Record = paste(as.character(div.table[3:8, 2]), "-", as.character(div.table[3:8, 3]), sep = ""))
divisions <- rbind(divisions, 
                   data.frame(TeamName = div.table[11:16,1],
                              Division = div.table[9,1],
                              Record = paste(as.character(div.table[11:16, 2]), "-", as.character(div.table[11:16, 3]), sep = "")))

#Add abbreviations to dataframe
divisions <- merge(teams, divisions)

#Cleans up the section of the page that has points for and points against
division1 <- division.page$xstandTbl_div0[2:7,1:3]
division2 <- division.page$xstandTbl_div1[2:7,1:3]

#Add division rank
division1$DivRank <- c(1:6)
division2$DivRank <- c(1:6)

#Combine division tables
division.combined <- rbind(division1, division2)

#Clean Team Names and add Abbr
division.combined[,1] <- gsub("\\s\\(.*", "", division.combined[,1])
colnames(division.combined) <- c("TeamName", "PointsFor", "PointsAgainst", "DivRank")
division.combined <- merge(teams, division.combined)

#TBD Section on how to to "CHANGE"
division.combined$Change <- 0


#Combine BOTH Dataframes and Rearrange for final export
combined.division.data <- merge(divisions, division.combined)

final.division.data <- combined.division.data[, c("DivRank",
                                                  "TeamCode",
                                                  "TeamName",
                                                  "Division",
                                                  "Change",
                                                  "Record",
                                                  "PointsFor",
                                                  "PointsAgainst")]

final.division.data <- final.division.data[with(final.division.data, order(DivRank)),]
final.division.data <- final.division.data[with(final.division.data, order(Division)),]

#Need to add command to send this to a csv
final.division.data 
