library(readxl)
library(dplyr)
setwd("~/Downloads/All Data/Pepp/")
getwd()
Pepp_All <- read_xlsx("Pepperdine Compiled.xlsx")

if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}
if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)
}

summary(Pepp_All)
class(Pepp_All$DateTime)

Pepp_All$DateTime <- as_datetime(Pepp_All$DateTime)

Pepp_All <- Pepp_All %>% 
  mutate(Time = format(DateTime, "%H:%M:%S")) %>% 
  relocate(Time, .after = 1)

write_xlsx(Pepp_All, "Pepp_All.xlsx")
