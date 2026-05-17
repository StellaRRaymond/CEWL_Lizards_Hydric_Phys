if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}
library(tidyverse)

if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}
if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}

#Set wd & check it
setwd("~/Downloads/All Data/")
getwd()
Prelim_Stat_Longest <- read_xlsx(
  path = "~/Downloads/All Data/Cal Poly/Cal Poly Master.xlsx","Temp",
  col_types = c(
    "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Cal Poly") %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CSUSM/CSUSM Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSUSM"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/Pepperdine/Pepperdine Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "Pepperdine"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/Scripps/Scripps Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "Scripps"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/Whittier/Whittier Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "Whittier"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/UCLA/UCLA Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "UCLA"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CSUSB/CSUSB Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))%>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSUSB"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CSULB/CSULB Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSULB"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CLU/CLU Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CLU"), by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/SDSU/SDSU Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "SDSU"), by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/Harvey Mudd/HMC Master.xlsx","Temp",
      col_types = c(
        "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))%>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "HMC"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  mutate(Location = dplyr::recode(Location, "Amb" = "Ambient"), #Cannot be done after one of the lmer packages (car) because it overrides recode.
         Time = format(DateTime, "%H:%M:%S"),
         #Time = hms(format(DateTime, "%H:%M:%S")),
         MonthDayYear = format(DateTime, "%D"),
         MonthDayYear = mdy(MonthDayYear))

#Removing all bad data
Prelim_Stat_Longest_Filtered <- Prelim_Stat_Longest %>% 
  mutate(Time = hms(Time),
         Time_sec = hour(Time) * 3600 + minute(Time) * 60 + second(Time)) %>% 
  filter(
  #Removal of all of CLU's Off_Sun because it was chewed through and unreliable
    !(Campus == "CLU" & Location == "Sun" & Site == "Off"),
  #Removal of all of CSUSM's Ambient because Ambient appeared to be completely sunned
    !(Campus == "CSUSM" & Location == "Ambient"),
  #Known Sunning of Whittier's On_Shade
  !(Campus == "Whittier" & Location == c("Shade","Ambient") & Site == "On" &
      DateTime >= ymd_hms("2025-06-01 00:00:00") &
      DateTime <= ymd_hms("2025-08-07 19:00:00")),    
  #More known sunning of shade logger only
  !(Campus == "Whittier" & Location == c("Shade","Ambient") & Site == "On" &
      DateTime >= ymd_hms("2025-09-11 12:30:00")),
    #All other data filtering
  #Little misc. dips in UCLA
    !(Campus == "UCLA" & Site == "Off" & Location == "Sun" & MonthDayYear == mdy("05-18-2025") & Time == hms("14:00:00")),
    !(Campus == "UCLA" & Site == "Off" & Location == "Sun" & MonthDayYear == mdy("05-18-2025") & Time == hms("15:00:00")),
    !(Campus == "UCLA" & Site == "Off" & Location == "Sun" & MonthDayYear == mdy("05-16-2025") & Time == hms("14:00:00")),
    !(Campus == "UCLA" & Site == "Off" & Location == "Sun" & MonthDayYear == mdy("05-16-2025") & Time == hms("15:00:00")),
    !(Campus == "UCLA" & Site == "On" & Location == "Sun" &
        DateTime >= ymd_hms("2025-05-16 12:30:00") &
        DateTime <= ymd_hms("2025-05-16 14:00:00")),
    !(Campus == "UCLA" & Site == "On" & Location == "Sun" &
        DateTime >= ymd_hms("2025-05-28 08:30:00") &
        DateTime <= ymd_hms("2025-05-28 17:30:00")),
    !(Campus == "UCLA" & Site == "On" & Location == "Sun" & between(MonthDayYear, mdy("05-07-2025"), mdy("07-14-2025")) & between(Time, hms("09:00:00"), hms("18:00:00"))),
    #CSULB Shaded sun loggers (Off showed no signs of cloudiness)
    !(Campus == "CSULB" & Site == "On" & Location == "Sun" & between(MonthDayYear, mdy("05-24-2025"), mdy("05-27-2025")) & between(Time, hms("08:00:00"), hms("20:00:00"))),
    !(Campus == "CSULB" & Site == "On" & Location == "Sun" & DateTime >= mdy_hms("05-23-2025 13:30:00") & DateTime <= mdy_hms("05-23-2025 16:30:00")),
    !(Campus == "CSULB" & Site == "Off" & Location == "Shade" & DateTime >= mdy_hms("03-05-2025 11:00:00") & DateTime <= mdy_hms("03-05-2025 18:00:00")),
    !(Campus == "CSULB" & Site == "Off" & Location == "Ambient" & DateTime >= mdy_hms("03-05-2025 12:30:00") & DateTime <= mdy_hms("03-05-2025 18:00:00")),
    !(Campus == "CSULB" & Site == "Off" & Location == "Shade" & DateTime >= mdy_hms("03-06-2025 13:00:00") & DateTime <= mdy_hms("03-06-2025 18:00:00")),
    !(Campus == "CSULB" & Site == "Off" & Location == "Ambient" & DateTime >= mdy_hms("03-06-2025 13:00:00") & DateTime <= mdy_hms("03-06-2025 18:00:00")),
    #Miscellaneous removals
    #Scripps shade sunning
  #Off_Shade nearly reaches sun temp, over 1/2 way between Amb & sun
    !(Campus == "Scripps" & Site == "Off" & Location == "Shade" &
        DateTime >= ymd_hms("2025-06-20 08:00:00") &
        DateTime <= ymd_hms("2025-07-08 20:00:00")),
    !(Campus == "Scripps" & Site == "Off" & Location == "Shade" &
        DateTime >= ymd_hms("2025-08-03 08:00:00") &
        DateTime <= ymd_hms("2025-08-23 08:00:00")),
    !(Campus == "Cal Poly" & Location == "Sun" &
        MonthDayYear == mdy("03/12/2025") & 
        hms("08:00:00") < Time & Time < hms("20:00:00")),
    !(Campus == "Cal Poly" & Site == "Off" &
        DateTime >= ymd_hms("2025-03-10 20:00:00") &
        DateTime <= ymd_hms("2025-03-11 07:00:00")),
    !(Campus == "Cal Poly" & Location == "Sun" & Site == "On" &
        MonthDayYear == mdy("07/01/2025") & 
        hms("08:00:00") < Time & Time < hms("20:00:00")),
  #Should already be filtered above, not sure why I re-filtered
    !(Campus == "Whittier" & Site == "On" & Location == "Shade" & between(MonthDayYear, mdy("06-01-2025"), mdy("08-08-2025")) & between(Time_sec, 15.5*3600, 18*3600))
    )



Prelim_Stat_All <- 
  Prelim_Stat_Longest_Filtered %>% 
  #Prelim_Stat_Longest %>% 
  pivot_wider(names_from = Location,
              values_from = Temperature) %>% 
  filter(!if_all(everything(), is.na)) %>% 
  mutate(Time = hms(format(DateTime, "%H:%M:%S")),
         MonthDayYear = mdy(format(DateTime, "%D")),
         Month = month(MonthDayYear, label = T, abbr = T),
         Season = case_when(
           MonthDayYear >= mdy("12-21-2024") & MonthDayYear < mdy("03-20-2025") ~ "Winter",
           MonthDayYear >= mdy("03-20-2025") & MonthDayYear < mdy("06-21-2025") ~ "Spring",
           MonthDayYear >= mdy("06-21-2025") & MonthDayYear < mdy("09-22-2025") ~ "Summer",
           MonthDayYear >= mdy("09-22-2025") & MonthDayYear < mdy("12-21-2025") ~ "Fall",
           TRUE ~ "Winter"
         )) %>%
  #rename(Ambient = Amb) %>% 
  relocate(MonthDayYear, .after = 2) %>% 
  relocate(Campus, .after = "Shade") %>% 
  relocate(Time, .after = 3) %>% 
  filter(!Month %in% c("Feb", "Oct", "Nov"))


Ambient_Data <- Prelim_Stat_All |> 
  select(!c(Sun, Shade)) |> 
  dplyr::rename(Temperature = Ambient)

write_xlsx(Ambient_Data, "Ambient_Data.xlsx")
