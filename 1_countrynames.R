## country names ###############################################################
# input: no
# output: country names for data of incidence (WHO or Johns Hopkins) and interventions (github)
countrynames <- function() {
  
  # # 49 countries in incidence data (WHO)
  # country_who <- as.character(confirmed_who$Country)
  # # 52 countries in intervention data (github)
  # country_github <- as.character(unique(interventions_github$Country))
  #
  # # print names
  # cat(paste0('"', country_who,    '"'), sep = ", ", fill = F)
  # cat(paste0('"', country_github, '"'), sep = ", ", fill = F)
  # # matching
  # country_who[which(is.na(match(country_who, country_github)))]
  # country_github[which(is.na(match(country_github, country_who)))]
  
  # ISO2 code
  name_iso <-
    c("AL", "AT", "BE", "BA", "BR", "CA", "HR",
      "CZ", "DK", "EC", "SV", "EE", "FI", "FR",
      "DE", "GR", "HN", "HU", "IS", "IN", "ID",
      "IE", "IT", "JP", "KZ", "XK", "KW", "LT",
      "MY", "MU", "MX", "ME", "NL", "NZ", "MK",
      "NO", "PT", "KR", "RO", "RS", "SG", "SK",
      "SI", "ES", "SE", "CH", "SY", "TH", "GB", "US")
  
  # WHO
  name_who <-
    c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Brazil", "Canada", "Croatia",
      "Czechia", "Denmark", "Ecuador", "El Salvador", "Estonia", "Finland", "France",
      "Germany", "Greece", "Honduras", "Hungary", "Iceland", "India", "Indonesia",
      "Ireland", "Italy", "Japan", "Kazakhstan", "Kosovo", "Kuwait", "Lithuania",
      "Malaysia", "Mauritius", "Mexico", "Montenegro", "Netherlands", "New Zealand", "North Macedonia",
      "Norway", "Portugal", "Republic of Korea", "Romania", "Serbia", "Singapore", "Slovakia",
      "Slovenia", "Spain", "Sweden", "Switzerland", "Syrian Arab Republic", "Thailand", "The United Kingdom",
      "United States of America")
  
  # manually matching
  name_github <-
    c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Brazil", "Canada", "Croatia",
      "Czech Republic", "Denmark", "Ecuador", "El Salvador", "Estonia", "Finland", "France",
      "Germany", "Greece", "Honduras", "Hungary", "Iceland", "India", "Indonesia",
      "Republic of Ireland", "Italy", "Japan", "Kazakhstan", "Kosovo", "Kuwait", "Lithuania",
      "Malaysia", "Mauritius", "Mexico", "Montenegro", "Netherlands", "New Zealand", "North Macedonia",
      "Norway", "Portugal", "South Korea", "Romania", "Serbia", "Singapore", "Slovakia",
      "Slovenia", "Spain", "Sweden", "Switzerland", "Syria", "Thailand", "United Kingdom",
      "United States of America")
  
  # # original
  # name_github <-
  #   c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Brazil", "Canada", "China",
  #     "Croatia", "Czech Republic", "Denmark", "Diamond Princess", "Ecuador", "El Salvador", "Estonia",
  #     "Finland", "France", "Germany", "Greece", "Honduras", "Hungary", "Iceland",
  #     "India", "Indonesia", "Italy", "Japan", "Kazakhstan", "Kosovo", "Kuwait",
  #     "Liechtenstein", "Lithuania", "Malaysia", "Mauritius", "Mexico", "Montenegro", "Netherlands",
  #     "New Zealand", "North Macedonia", "Norway", "Portugal", "Republic of Ireland", "Romania", "Serbia",
  #     "Singapore", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden", "Switzerland",
  #     "Syria", "Taiwan", "Thailand", "United Kingdom", "United States of America")
  
  # manually matching
  name_hopkins <-
    c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Brazil", "Canada", "Croatia",
      "Czechia", "Denmark", "Ecuador", "El Salvador", "Estonia", "Finland", "France",
      "Germany", "Greece", "Honduras", "Hungary", "Iceland", "India", "Indonesia",
      "Ireland", "Italy", "Japan", "Kazakhstan", "Kosovo", "Kuwait", "Lithuania",
      "Malaysia", "Mauritius", "Mexico", "Montenegro", "Netherlands", "New Zealand", "North Macedonia",
      "Norway", "Portugal", "Korea, South", "Romania", "Serbia", "Singapore", "Slovakia",
      "Slovenia", "Spain", "Sweden", "Switzerland", "Syria", "Thailand", "United Kingdom",
      "US")
  
  # data source selection: Johns Hopkins / WHO / Japan
  # Johns Hopkins: 40 countries
  # WHO: 9 countries # c(2, 5, 10, 14, 17, 25, 26, 36, 40)
  # Japan: 1
  selected <- c(
    "hop", "who", "hop", "hop", "who", "hop", "hop", "hop", "hop", "who",
    "hop", "hop", "hop", "who", "hop", "hop", "who", "hop", "hop", "hop",
    "hop", "hop", "hop", "jap", "who", "who", "hop", "hop", "hop", "hop",
    "hop", "hop", "hop", "hop", "hop", "who", "hop", "hop", "hop", "who",
    "hop", "hop", "hop", "hop", "hop", "hop", "hop", "hop", "hop", "hop")
  
  # country names
  ( countries <- data.frame(index = 1:length(name_who),
                            selected    = selected,
                            name_iso    = name_iso,
                            name_who    = name_who,
                            name_github = name_github,
                            name_hopkins = name_hopkins,
                            stringsAsFactors = F) )
  
  # the target country name
  # print( country <- countries[i, ] )
  
  # output
  return(countries)
  
}