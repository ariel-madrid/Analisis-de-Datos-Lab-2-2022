library(dplyr)

read_time_series <- function(location) {
  return(
    read.csv(location, 
             header=FALSE,
             sep='\t') %>%
      select(-2) %>%
      rename_with(~ c("PAM", "VFSC"), all_of(c("V1", "V3")))
  )
}

data_normocapnia <- read_time_series("./Archivos/RP000.txt")
data_hipercapnia <- read_time_series("./Archivos/RP001.txt")

