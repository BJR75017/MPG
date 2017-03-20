source("library.R")

# Chargement du fichier
mpg_notes <- loadWorkbook("data/Stats MPG-saison4MPG.xlsx")

# Sheet names
sheet_names <- getSheets(mpg_notes)
sheet_names <- sheet_names[-1] # Remove rules
names(sheet_names) <- sheet_names

# Read function
team_results <- function(team_name) {
  message(team_name)
  if(team_name %in% sheet_names) {
    #evaluate first row
    f_row <- readWorksheet(object=mpg_notes, team_name)
    first_row <- grep("Poste",f_row[,1])
    
    #load data
    df_team <- readWorksheet(object=mpg_notes, team_name, startRow = first_row+1, header = T)
    names(df_team) <- tolower(names(df_team))
    names(df_team)[1:6] <- c("poste","nom","tit","entree","buts","mean_l1")
    id_day <- grep("^j",names(df_team))
    names(df_team)[id_day] <- substr(names(df_team)[id_day],1,3)
    df_team$team <- team_name
  
    #exclude last row
    df_team <- df_team[which(!is.na(df_team$poste)),]
  } else {
    message("Unknown team")
    df_team <- NULL
  }
  return(df_team)
}

results_mpg <- data.frame(stringsAsFactors = F)
for(name in sheet_names){
  results_mpg <- rbind.fill(results_mpg, team_results(name))
}

for(i in grep("^j", names(results_mpg))){
  results_mpg[which(results_mpg[,i] == "<"),i] <- NA
  results_mpg[,i] <- as.numeric(results_mpg[,i])
}

derniere_journee <- max(as.numeric(gsub("^j","", names(results_mpg))), na.rm = T)
last7 <- paste("j",derniere_journee:(derniere_journee-7), sep = "")  
results_mpg$mean_last7 <- rowMeans(results_mpg[,last7], na.rm = T)


save(results_mpg, file = "data/results.RData")
