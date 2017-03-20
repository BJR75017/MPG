require(XLConnect)
require(plyr)
require(shinydashboard)
require(reshape2)
require(ggplot2)

mpg_color <- "#45c945"

filter_order <- function(results_mpg, filter_poste) {
  results <- subset(results_mpg, poste == filter_poste)
  last_day <- names(results)[length(results) -1]
  results <- results[,c("poste","team","nom","tit","mean_l1","mean_last7", last_day)]
  results <- results[order(results$tit, results$mean_last7, results$mean_l1, decreasing = T),]
  results <- results[which(!is.na(results$tit)),]
  return(results)
}


filter_order_name <- function(results_mpg, filter_poste) {
  return(filter_order(results_mpg, filter_poste)$nom)
}

stat_prep <- function(results_mpg, filter_poste) {
  days <- subset(results_mpg, poste == filter_poste)
  days <- days[which(!is.na(days$tit)),]
  id_days <- c(which(names(results_mpg) %in% c("nom")), grep("^j", names(days)))
  days <- days[,id_days]
  days <- melt(days, id.vars = "nom")
  days$variable <- as.numeric(gsub("j","",days$variable))
  return(days)
}

stat_mean <- function(results_mpg, filter_poste){
  mean_poste <- aggregate(value ~ variable, stat_prep(results_mpg, filter_poste), mean)
  return(mean_poste)
}

stat_player <- function(results_mpg, filter_poste, player){
  mean_player <- subset(stat_prep(results_mpg,filter_poste), nom == player)
  return(mean_player)
}

stat_graph <- function(results_mpg, filter_poste, player){
  mean_poste <- stat_mean(results_mpg, filter_poste)
  mean_poste$nom <- "Moyenne"
  mean_player <- stat_player(results_mpg, filter_poste, player)
  df.agg <- rbind(mean_poste, mean_player)
  ggplot(df.agg, aes(x = variable, y = value, group = nom, linetype = nom)) +
    geom_line(size = 1.05, color = mpg_color) +
    ylim(0,10) +
    scale_x_continuous(breaks = seq(1,max(df.agg$variable), 2)) +
    labs(title = paste(player,"- Stats"), x = "Journee", y = "Note")
}