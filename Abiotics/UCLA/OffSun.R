library(readxl)
setwd("~/Downloads/All Data/UCLA/")
getwd()

Off_Sun <- read_xlsx("UCLA_Off_Sun.xlsx")

library(dplyr)
library(lubridate)

Off_Sun$DateTime <- as_datetime(Off_Sun$DateTime)


Off_Sun <- Off_Sun %>%
  filter(minute(DateTime) %in% c(0, 30),
         second(DateTime) == 0)

if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}

write_xlsx(Off_Sun, "Filtered_Off_Sun.xlsx")
