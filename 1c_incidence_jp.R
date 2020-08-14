## (1) incidence from Japan ######################################################
# input: get data online or call downloaded data
# input: target output (incidence or detection time)
# output: daily incidence of confirmed cases
# output: time of symptomatic and confirmed cases
incidence <- function(online, detection) {
  
  if(online) {
    
    # call Japanese data, https://gis.jag-japan.com/covid19jp/
    case_jp <- read.csv(text = RCurl::getURL(paste0(
      "https://dl.dropboxusercontent.com/",
      "s/6mztoeb6xf78g5w/",
      "COVID-19.csv")))
    
    # save data
    today <- format(Sys.time(), "%Y-%m-%d-%H%M") # ?strptime()
    N <- max(case_jp$通し, na.rm = T)
    ( filename <- paste0("data/case_jp", "-", N, ".Rdata") )
    save(case_jp, file = filename)
    
  } else {
    
    # load data
    N <- 17131
    ( filename <- paste0("data/case_jp", "-", N, ".Rdata") )
    load(file = filename)
    
  }
  
  ## incidence #################################################################
  
  # number of cases
  case_jp <- case_jp[1:N, ]
  # index
  index <- case_jp$通し
  # date of confirmed from 15-Jan-2020
  confirmed <- as.Date(case_jp$確定日, format = "%m/%d/%Y")
  # date of symptomatic from 3-Jan-2020
  symptomatic <- as.Date(case_jp$発症日, format = "%m/%d/%Y")
  # local / imported
  overseas <- (is.na(case_jp$居住都道府県コード) & case_jp$Residential.Pref != "Unknown")
  airport <- (case_jp$Hospital.Pref == "Tokyo International Airport/NND"    |
                case_jp$Hospital.Pref == "Narita International Airport/NRT" |
                case_jp$Hospital.Pref == "Kansai International Airport/KIX" |
                case_jp$Hospital.Pref == "Chubu Centrair International Airport/NGO")
  imported <- (overseas | airport)
  local <- !imported
  
  # incidence of confirmed cases
  incid <- as.data.frame(table(
    factor(confirmed, levels = factor(seq(from = min(confirmed),
                                          to = max(confirmed),
                                          by = "days")))))
  colnames(incid) <- c("dates", "I")
  incid$dates <- as.Date(incid$dates)
  incid$I <- as.numeric(incid$I)
  # cumulative
  incid$C <- cumsum(incid$I)
  
  # output
  incid <- list(C = incid)
  
  ## time of symptomatic and confirmed cases ###################################
  
  detec <- data.frame(S = symptomatic, C = confirmed)
  
  ## output ####################################################################
  
  if(detection) {
    return(detec)
  } else {
    return(incid)
  }
  
}