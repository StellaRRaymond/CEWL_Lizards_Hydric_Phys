
#start and last date
Prelim_Stat_All %>%
  group_by(Campus) %>%
  summarize(
    first_date = min(DateTime, na.rm = TRUE),
    last_date  = max(DateTime, na.rm = TRUE))


#Prelim_Stat_Filtered <- 
  Prelim_Stat_All %>% 
  #mutate(Time = hms(Time),
  #       MonthDayYear = mdy(MonthDayYear)) %>% 
  filter(
    Campus == "Scripps",
    Site == "On",
    #DateTime >= as.POSIXct("2025-08-07 00:00:00", tz = "UTC"), 
    #DateTime <= as.POSIXct("2025-08-08 00:00:00", tz = "UTC")
    ) %>% 
#Prelim_Stat_Filtered_Longer <- Prelim_Stat_Filtered  
  pivot_longer(cols = c(Ambient, Sun, Shade),
               names_to = "Location",
               values_to = "Temperature") %>% 
  #filter(!(Location == "Sun")) %>% 
  ggplot(aes(x = DateTime, y = Temperature, color = Location))+
  geom_point()+ 
    ylim(0, 80)
    #labs(title = "On Scripps Loggers")
  #geom_vline(data = Noon_df, aes(xintercept = as.numeric(Noon)),
             #color = "grey", linetype = "dashed")



#noon visualization
Noon_df <- Prelim_Stat_All %>%
  mutate(Date = as.Date(DateTime)) %>%
  distinct(Date) %>%
  mutate(Noon = as.POSIXct(paste(Date, "12:00:00"), tz = "UTC"))
  
  
Prelim_Stat_All %>% 
  filter(!is.na(Site)
        , Campus == "CSUSM",
        #DateTime > as.POSIXct("2025-08-01 00:00:00"),
        DateTime < as.POSIXct("2025-07-15 00:00:00"),
        Time >= hms("08:00:00"),
        Time <= hms("20:00:00"),
        Site == "On"
  ) %>% 
  ggplot(aes(x = Time, y = Sun, group = Site))+
  #facet_wrap(vars(Site)) +
  #geom_point(aes(color = MonthDayYear), 
             #alpha = 0.5
   #          )+
  geom_point(aes(color = MonthDayYear))+
  labs(#title = "Whittier On_Shade Temperature",
       #subtitle = "Just Deployment-5/9",
       #subtitle = "After Filtering 15:30-18:00 (Data Deployment-8/9)",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
    #labels = function(x) format(x, "%H:%M")
    labels = scales::label_time(format = "%H")
    ) +
  ylim(10,60) +
  #geom_hline(yintercept = 32, linetype = "dashed", color = "black") +
  scale_color_gradientn(
    colors = viridis::viridis(10)) +
  theme_classic(base_size = 17)
  
