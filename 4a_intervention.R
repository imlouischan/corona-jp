## interventions ###############################################################
# input: get data online or call downloaded data
# input: target country
# input: include only national interventions or all interventions
# output: implemented interventions and the corresponding dates
interventions <- function(online, country, national) {
  
  if(online) {
    
    # # call data from github, # http://covid19-interventions.com
    # interventions_github <- read.csv(text = RCurl::getURL(paste0(
    #   "https://raw.githubusercontent.com/",
    #   "amel-github/covid19-interventionmeasures/master/",
    #   "COVID19_non-pharmaceutical-interventions.csv")))
    
    # call data from github, # http://covid19-interventions.com
    interventions_github <- read.csv(text = RCurl::getURL(paste0(
      "https://raw.githubusercontent.com/",
      "amel-github/covid19-interventionmeasures/master/",
      "COVID19_non-pharmaceutical-interventions_version2_utf8.csv")))
    
    # save data
    today <- format(Sys.time(), "%Y-%m-%d-%H%M") # ?strptime()
    N <- nrow(interventions_github)
    ( filename <- paste0("data/interventions_github", "-", N, ".Rdata") )
    save(interventions_github, file = filename)
    
  } else {
    
    # load data
    N <- 5113
    ( filename <- paste0("data/interventions_github", "-", N, ".Rdata") )
    load(file = filename)
    
  }
  
  ## interventions #############################################################
  
  # interventions implemented by the country
  interventions <- subset(interventions_github, Country == country$name_github)
  
  # option A: take all regions
  region  <- as.character(interventions$Region)
  # option B: only national interventions
  if(national)
    interventions <- interventions[(country$name_github == region | region == "National"), ]
  
  # order 8 measure themes
  interventions$Measure_L1 <-
    factor(interventions$Measure_L1, levels = c(
      "Case identification, contact tracing and related measures", # (1) > (i)
      "Environmental measures",                                    # (2) > (ii)
      "Healthcare and public health capacity",                     # (3) > (iii)
      "Resource allocation",                                       # (4) > (iv)
      "Risk communication",                                        # (6) > (v)
      "Social distancing",                                         # (7) > (vi)
      "Travel restriction",                                        # (8) > (vii)
      "Returning to normal life"                                   # (5) > (viii)
    )
    )
  
  # # save as csv file
  # source("4a_intervention_csv.R")
  # interventions_csv(interventions)
  
  # interventions, maximum 8 categories
  inter <- data.frame(dates     = as.Date(interventions$Date),
                      measures1 = interventions$Measure_L1,
                      m1        = as.numeric(interventions$Measure_L1))
  
  # remove repeated ones
  inter <- unique(inter)
  
  # sorting by dates
  inter <- inter[order(inter$dates, inter$measures1),]
  
  # output
  return(inter)
  
}