## save interventions into csv file ############################################
interventions_csv <- function(interventions) {
  
  # interventions, maximum 8 categories
  inter_all <- data.frame(dates    = as.Date(interventions$Date),
                          measures = paste0("(", tolower(as.roman(as.numeric(interventions$Measure_L1))), ") ",
                                            as.character(interventions$Measure_L1)),
                          details  = paste0( as.character(interventions$Measure_L2),
                                             " (", as.character(interventions$Measure_L3), ")"),
                          stringsAsFactors = F)
  
  # sorting by dates and measures
  inter_all <- inter_all[order(inter_all$dates, inter_all$measures),]
  
  # remove repeated ones
  inter_all <- unique(inter_all)
  
  # unique interventions
  inter_uni <- inter_all[!duplicated(inter_all[, 1:2]), ]
  
  # repeated rows
  ( repeated <- which(duplicated(inter_all[, 1:2])) )
  for(r in repeated) {
    ( m <- which(inter_uni[, 1] == inter_all[, 1][r] & inter_uni[, 2] == inter_all[, 2][r]) )
    inter_uni$details[m] <- paste0(inter_uni$details[m], "; ", inter_all$details[r])
  }
  
  # save as csv file
  ( filename <- paste0("interventions",
                       "_", country$index,
                       "_", country$name_who, ".csv") )
  write.table(inter_uni, file = filename, sep = ",",
              quote = T, row.names = F, col.names = F)
  
  # save as text file
  # ( filename <- paste0("interventions",
  #                      "_", country$index,
  #                      "_", country$name_who, ".txt") )
  # write.table(inter_uni, file = filename, sep = " & ", eol = " \\\\ \n",
  #             quote = F, row.names = F, col.names = F)
  
}