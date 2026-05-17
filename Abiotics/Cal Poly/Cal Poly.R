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

setwd("~/Downloads/All Data/Cal Poly/")
getwd()

CalPoly_All <- read_xlsx("Cal Poly.xlsx")

CalPoly_DateTime <- CalPoly_All %>% 
  mutate(Time = format(DateTime, "%H:%M:%S"),
         MonthDayYear = format(DateTime, "%D")) %>% 
  relocate(Time, .after = 1) %>% 
  relocate(MonthDayYear, .after = 2)
write_xlsx(CalPoly_DateTime, "CalPoly_DateTime.xlsx")


CalPoly_TimeMonth <- CalPoly_All %>% 
  mutate(Time = format(DateTime, "%H:%M:%S"),
         Month = format(DateTime, "%b")) %>% 
  relocate(Time, .after = 1) %>% 
  relocate(Month, .after = 2)

write_xlsx(CalPoly_TimeMonth, "CalPoly_TimeMonth.xlsx")


#Creating daily averages 
CalPoly_DailyAvgs <- CalPoly_All %>% 
  group_by(MonthDayYear, Location) %>% 
  summarize(across(.cols = Sun:RH, .fns = ~ mean(.x))) %>% 
  ungroup()
#save as xlsx
write_xlsx(CalPoly_DailyAvgs, "CalPoly_DailyAverages.xlsx")
#Creating daily range
CalPoly_Range <- CalPoly_All %>% 
  group_by(MonthDayYear, Location) %>%
  summarize(across(.cols = Sun:RH, .fns = ~ max(.x)-min(.x))) %>% 
  ungroup()
#save as xlsx
write_xlsx(CalPoly_Range, "CalPoly_Range.xlsx")
  
  