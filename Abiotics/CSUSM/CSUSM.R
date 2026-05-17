if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}
if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}

setwd("~/Downloads/All Data/CSUSM/")
getwd()
CSUSM_All <- read_xlsx("CSUSM_Master.xlsx")

CSUSM_All <- CSUSM_All %>% 
  mutate(Time = format(DateTime, "%H:%M:%S")) %>% 
  relocate(Time, .after = 1)

CSUSM_All <- CSUSM_All %>% 
  mutate(Month = format(DateTime, "%b")) %>% 
  relocate(Month, .after = 2)

write_xlsx(CSUSM_All, "CSUSM_Statistics.xlsx")
