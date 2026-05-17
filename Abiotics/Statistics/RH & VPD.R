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
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}
if(!require(stringr)){
  install.packages("stringr")
  require(stringr)
}

if(!require(lme4)){
  install.packages("lme4")
  require(lme4)
}
if(!require(lmerTest)){
  install.packages("lmerTest")
  require(lmerTest)
}
if(!require(car)){
  install.packages("car")
  require(car)
}
if(!require(DHARMa)){
  install.packages("DHARMa")
  require(DHARMa)
}
if(!require(performance)){
  install.packages("performance")
  require(performance)
}
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}
#Set wd & check it
setwd("~/Downloads/All Data/")
getwd()

VPD_All <- read_xlsx(path = "~/Downloads/All Data/Cal Poly/Cal Poly Master.xlsx","RH",
                     col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
  select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
  pivot_longer(
    cols = -DateTime,
    names_to = c("Site", "Variable"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%
  mutate(Campus = "Cal Poly") %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/CSUSM/CSUSM Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
            select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              ) %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value
              ) %>%
            mutate(Campus = "CSUSM"),
    by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/Pepperdine/Pepperdine Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
              select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>% 
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )  %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "Pepperdine"),
    by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/Scripps/Scripps Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
              select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )  %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "Scripps"),
            by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/Whittier/Whittier Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
      select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )   %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "Whittier"),
            by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/CSUSB/CSUSB Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
      select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )   %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "CSUSB"),
           by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/CSULB/CSULB Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
              select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )   %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "CSULB"),
            by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/UCLA/UCLA Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
              select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )   %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "UCLA"),
            by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  full_join(
    read_xlsx(path = "~/Downloads/All Data/CLU/CLU Master.xlsx","RH",
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric")) %>%
              select(c("DateTime","On_RH","Off_RH","On_Temp","Off_Temp","On_VPD", "Off_VPD")) %>%
              pivot_longer(
                cols = -DateTime,
                names_to = c("Site", "Variable"),
                names_sep = "_",
                values_to = "Value"
              )   %>%
              pivot_wider(
                names_from = Variable,
                values_from = Value) %>%
              mutate(Campus = "CLU"),
            by = c("DateTime", "RH", "Temp", "Site", "VPD", "Campus")) %>%
  filter(!is.na(DateTime), !is.na(VPD)) %>%
  mutate(Time = hms(format(DateTime, "%H:%M:%S")),
         MonthDayYear = mdy(format(DateTime, "%D")),
         Month = month(DateTime, label = T, abbr = T))

VPD_All <- VPD_All %>% 
  #Removed only because it was removed from the other data set (an is likely sunned off)
  filter(!(Campus == "CSULB" & Site == "Off" & DateTime >= mdy_hms("03-05-2025 12:30:00") & DateTime <= mdy_hms("03-05-2025 18:00:00")),
         !(Campus == "CSULB" & Site == "Off" & DateTime >= mdy_hms("03-06-2025 13:00:00") & DateTime <= mdy_hms("03-06-2025 18:00:00")),) %>% 
  filter(!Month %in% c("Feb","Oct","Nov"),
         !Campus == "CSUSM",
         #More known sunning of shade logger only
         !(Campus == "Whittier" & Site == "On" &
           DateTime >= ymd_hms("2025-06-01 00:00:00") &
           DateTime <= ymd_hms("2025-08-07 19:00:00")),
         !(Campus == "Whittier" & Site == "On" &
           DateTime >= ymd_hms("2025-09-11 12:30:00")),
         #!(Campus == "Pepperdine" & Site == "On" & DateTime > as.POSIXct("2025-08-17 00:00:00"))
         )


VPD_HourlyAvgsLong <- VPD_All %>%
  group_by(Time, Site, Campus, Month) %>%
  summarize(
    mean = mean(VPD, na.rm = TRUE),
    sd   = sd(VPD, na.rm = TRUE),
    n    = sum(!is.na(VPD)),
    se   = sd(VPD, na.rm = TRUE) / sqrt(sum(!is.na(VPD))),
    cv   = sd(VPD, na.rm = TRUE) / mean(VPD, na.rm = TRUE) * 100) %>%
  ungroup() %>% 
  filter(n >= 3)
#!(Campus == "Cal Poly" & Month == "Mar")


VPD_lmer_df <- VPD_HourlyAvgsLong %>% 
  mutate(Time = as.numeric(Time)/3600,
         Campus = as.factor(Campus),
         Month = as.factor(Month)) 

VPD_lmer <- lmer(log(mean) ~  Site*Month + poly(Time, 4) +
                (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),
  data = VPD_lmer_df,
  REML = F)



anova(VPD_lmer)
pairs(emmeans(VPD_lmer, specs = ~ Site|Month))


plot(fitted(VPD_lmer), resid(VPD_lmer),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")





VPD_Graph <- VPD_HourlyAvgsLong %>%
  filter(!Campus == "CSUSM") %>%
  mutate(
    Month_num = match(as.character(Month), month.abb),
    Time_hr = time_length(Time, "hours"))

# Reference line: average across campuses
VPD_Graph_Ref <- VPD_Graph %>%
  group_by(Site, Month, Time_hr) %>%
  summarise(
    mean_ref = mean(mean, na.rm = TRUE),
    .groups = "drop")

okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot() +
  # campuses (grey)
  geom_line(data = VPD_Graph,
            aes(x = Time_hr, y = mean, group = interaction(Campus, Site)), color = "grey70", alpha = 0.6, na.rm = T, linewidth = 1) +
  # geom_point(data = VPD_Graph, 
  #            aes(x = Time_hr, y = mean, group = interaction(Campus, Site)),
  #            color = "grey70", alpha = 0.6) +
  # reference line (campus-averaged)
  geom_line(
    data = VPD_Graph_Ref,
    aes(x = Time_hr, y = mean_ref, color = Site),
    linewidth = 1.5) +
  facet_grid(cols = vars(Month)) +
  scale_color_manual(values = okabe_ito[c(2,1)],
                     breaks = c("On", "Off"),
                     labels = c(
                       "On"  = "Urban",
                       "Off" = "Non-urban")) +
  scale_x_continuous(
    name = "Time of Day",
    breaks = seq(2, 20, 6)) +
  labs(y = expression("Vapor Pressure Deficit (kPa)"),
       color = "Site") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))



VPD_Graph_Ref_15 <- VPD_Graph_Ref %>%
  left_join(
    VPD_All %>%
      group_by(Month) %>%
      summarise(Month_mid = median(DateTime, na.rm = TRUE)),
    by = "Month") %>% 
  filter(Time_hr > 7.5, Time_hr < 20)

ggplot() +
  geom_line(data = VPD_All, aes(x = DateTime, y = VPD, group = interaction(Campus, Site)), color = "grey70", alpha = 0.6) +
  # geom_point(data = VPD_All, aes(x = DateTime, y = VPD, group = interaction(Campus, Site)), color = "grey70", alpha = 0.6) +
  # reference line (campus-averaged)
  # geom_point(data = VPD_Graph_Ref_15, aes(x = Month_mid, y = mean_ref, color = Site), #linewidth = 1.2
  #           )+
  geom_smooth(data = VPD_Graph_Ref_15,aes(x = Month_mid, y = mean_ref, color = Site, group = Site
  #group = interaction(Site, Time_hr)
                                        ), se = F,na.rm = T, linewidth = 1.2
  )+
  scale_color_manual(values = okabe_ito[c(3,5)],
                     breaks = c("On", "Off"),
                     labels = c(
                       "On"  = "Urban",
                       "Off" = "Non-urban")) +
  #scale_x_discrete(name = "Month") +
  labs(y = expression("Vapor Pressure Deficit (kPa)"),
       color = "Site") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))














VPD_daytime_mean <- VPD_All %>%
  mutate(
    Time_hr = hour(DateTime) + minute(DateTime)/60
  ) %>%
  filter(Time_hr >= 7.5, Time_hr <= 20) %>%
  #filter(Time_hr == 12) %>% 
  group_by(Site, Month) %>%
  summarise(
    mean_vpd = mean(VPD, na.rm = TRUE),
    .groups = "drop")

VPD_daytime_mean <- VPD_daytime_mean %>%
  left_join(
    VPD_All %>%
      group_by(Month) %>% 
      summarise(Month_mid = median(DateTime)), by = "Month")



ggplot() +
  geom_line(
    data = VPD_All,
    aes(x = DateTime, y = VPD, 
        #group = interaction(Campus, Site), 
        color = Site), alpha = 0.4) +
  geom_point(
    data = VPD_All,
    aes(x = DateTime, y = VPD, 
        #group = interaction(Campus, Site),
        color = Site), alpha = 0.4, size = 0.5
  ) +
  scale_color_manual(values = c(
    "On"  = "grey40",
    "Off" = "grey75"),
    guide = "none") +
  geom_line(
    data = VPD_daytime_mean,
    aes(x = Month_mid, y = mean_vpd, group = Site, color = Site), color = okabe_ito[c(3,5)],
    linewidth = 1.5) +
  geom_point(
    data = VPD_daytime_mean,
    aes(x = Month_mid, y = mean_vpd, color = Site), color = okabe_ito[c(3,5)], size = 3) +
  #scale_color_manual(
    #values = okabe_ito[c(3,5)],
    #labels = c("Urban", "Non-urban")) +
  scale_x_datetime(
    date_breaks = "1 month", date_labels = "%b") +
  labs(
    y = expression("Vapor Pressure Deficit (kPa)")
    #, color = "Site"
    ) +
  theme_bw()

# ggplot() +
#   geom_line(
#     data = VPD_All,
#     aes(x = DateTime, y = VPD, group = interaction(Campus, Site), color = Site), alpha = 0.4) +
#   geom_point(
#     data = VPD_All,
#     aes(x = DateTime, y = VPD, group = interaction(Campus, Site), color = Site), alpha = 0.4, size = 0.5 ) +
#   scale_color_manual(
#     values = c("On" = "grey40", "Off" = "grey75"),
#     guide = "none") +
#   geom_line(
#     data = subset(VPD_daytime_mean, Site == "On"),
#     aes(x = Month_mid, y = mean_vpd, group = Site),
#     color = okabe_ito[3],
#     linewidth = 1.5) +
#   geom_line(
#     data = subset(VPD_daytime_mean, Site == "Off"),
#     aes(x = Month_mid, y = mean_vpd, group = Site),
#     color = okabe_ito[5],
#     linewidth = 1.5) +
#   geom_point(
#     data = subset(VPD_daytime_mean, Site == "On"),
#     aes(x = Month_mid, y = mean_vpd),
#     color = okabe_ito[3], size = 3) +
#   geom_point(
#     data = subset(VPD_daytime_mean, Site == "Off"),
#     aes(x = Month_mid, y = mean_vpd),
#     color = okabe_ito[5], size = 3) +
#   scale_x_datetime(
#     date_breaks = "1 month", date_labels = "%b") +
#   labs(y = expression("Vapor Pressure Deficit (kPa)")) +
#   theme_bw()








VPD_HourlyAvgsWide <- VPD_All %>%
  filter(!is.na(VPD), !is.na(RH)) %>%
  mutate(Time = format(DateTime, "%H:%M:%S")
         ) %>% 
  group_by(Time, Site, Campus, Month
  ) %>%
  summarize(
    across(
      c(RH, VPD),
      .fns = list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        n    = ~sum(!is.na(.x)),
        se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
        cv   = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{.col}_{.fn}")) %>%
    ungroup() %>% 
  filter(VPD_n > 3)
#%>%
  # mutate(Time = as.factor(as.character(Time)), Site = as.factor(Site), Campus = as.factor(Campus))
head(arrange(filter(VPD_HourlyAvgsWide, VPD_n > 0), n), VPD_n = 20)
# VPD_Temp_HourlyAvgsWide <- VPD_All %>%
#   filter(!is.na(VPD), !is.na(RH)) %>%
#   mutate(Time = format(DateTime, "%H:%M:%S"),
#          Month = month(DateTime, label = T, abbr = T)) %>% 
#   group_by(Time, Site, Campus, Month
#   ) %>%
#   summarize(
#     across(
#       c(RH, VPD, Temp),
#       .fns = list(
#         mean = ~mean(.x, na.rm = TRUE),
#         sd   = ~sd(.x, na.rm = TRUE),
#         n    = ~sum(!is.na(.x)),
#         se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
#         cv   = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
#       .names = "{.col}_{.fn}")) %>%
#   ungroup() %>% 
#   filter(VPD_n >2)

#GRAPHS!
#Seasonal VPD For All Campuses
ggplot(filter(VPD_All, 
              #!(Campus == "CSUSM")
              ), 
       aes(x=DateTime, y=VPD, color = Site))+
  geom_point(alpha = .5)+
  facet_wrap(vars(Campus))

#Half Hourly All Campuses
VPD_All %>% 
  filter(!(Campus == "CSUSM")
         #DateTime > as.POSIXct("2025-06-01 00:00:00"), DateTime < as.POSIXct("2025-05-09 00:00:00"), Time >= hms("14:00:00"), Time <= hms("20:00:00"), Site == "On"
  ) %>% 
  ggplot(aes(x = Time, y = VPD))+ 
  facet_grid(Campus~Site) +
  geom_jitter(aes(color = MonthDayYear), alpha = .75)+
  labs(title = "VPD Per Half Hour", y = "VPD", x = "Time (24-Hr)")+
  scale_x_time( breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
    labels = scales::label_time(format = "%H"))

VPD_All %>% 
  filter(!(Campus == "CSUSM")) %>% 
  ggplot(aes(x = Time, y = VPD, color = Campus))+ 
  facet_wrap(vars(Site)) +
  geom_smooth(se = F)+
  labs(title = "VPD Per Half Hour", y = "VPD", x = "Time (24-Hr)")+
  scale_x_time( breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
                labels = scales::label_time(format = "%H"))


#Half Hourly per Campus
VPD_All %>% 
  filter(#!(Campus == "C")
         #DateTime > as.POSIXct("2025-06-01 00:00:00"),
         #DateTime < as.POSIXct("2025-05-09 00:00:00"),
         #Time >= hms("14:00:00"), Time <= hms("20:00:00"),
         #Site == "On"
         )%>% 
  mutate(Time = hms(format(DateTime, "%H:%M:%S")),
         MonthDayYear = mdy(format(DateTime, "%D"))) %>% 
  ggplot(aes(x = Time, y = VPD))+ 
  facet_wrap(vars(Site)) +
  geom_jitter(aes(color = Campus), alpha = .75)+
  labs(title = "VPD Per Half Hour", y = "VPD", x = "Time (24-Hr)")+
  scale_x_time( breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
                labels = scales::label_time(format = "%H"))

ggplot(filter(VPD_All, 
              #Campus == "Pepperdine",
             # DateTime > as.POSIXct("2025-08-17 00:00:00")
              ), 
       aes(x=DateTime, y=Temp, color=Site))+
  geom_point()+ facet_wrap(vars(Campus))






ggplot(mutate(filter(VPD_Temp_HourlyAvgsWide, !(Campus == "CSUSM")), 
              Time = hms(as.character(Time))),
aes(x=Time, y=VPD_mean, color = Temp_mean))+
  geom_point()+
  #facet_wrap(vars(Campus))+
  facet_grid(Campus ~ Site)+
  scale_color_gradientn(
    colors = viridis::viridis(10)) +
  scale_x_time(breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
               labels = scales::label_time(format = "%H")) +theme_classic()



ggplot(mutate(filter(VPD_HourlyAvgsWide, !(Campus == "CSUSM")), 
              Time = hms(as.character(Time))),
aes(x=Time, y=VPD_mean, color = Campus, shape = Month))+
  geom_smooth(se = F, alpha = .5)+
  facet_wrap(vars(Site))+
  scale_x_time(breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
               labels = scales::label_time(format = "%H")) +
  theme_classic()




ggplot(mutate(filter(VPD_HourlyAvgsWide, !(Campus == "CSUSM")), 
              Time = hms(as.character(Time))),
       aes(x=Time, y=VPD_mean, color = Site, shape = Month))+
  geom_smooth(se = F, alpha = .5)+
  facet_wrap(vars(Campus))+
  scale_x_time(breaks = hms::as_hms(seq(7200, 79200, by = 7200)),
               labels = scales::label_time(format = "%H")) +
  theme_classic()



#Statistics
if(!require(lme4)){
  install.packages("lme4")
  require(lme4)
}
if(!require(lmerTest)){
  install.packages("lmerTest")
  require(lmerTest)
}
if(!require(car)){
  install.packages("car")
  require(car)
}
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}

VPD_Raw_Data <- VPD_All %>% 
  mutate(Time = as.factor(as.character(Time)))

VPD_Raw_lmer <- lmer(
  VPD ~  Site*Month + (1|Campus) + (1|Time),
  data = filter(VPD_Raw_Data,!Campus == "CSUSM"),
  REML = F)
Anova(VPD_Raw_lmer)
hist(resid(VPD_Raw_lmer))





VPD_HourlyAvgs_VPDlmer <- lmer(
  VPD_mean ~  Site*Month  + (1|Campus) + (1|Time),
  data = filter(VPD_HourlyAvgsWide,!Campus == "CSUSM"),
  REML = F)
#summary(VPD_HourlyAvgs_VPDlmer) #View Results
#anova(VPD_HourlyAvgs_VPDlmer) #View Results another way
Anova(VPD_HourlyAvgs_VPDlmer) #View Results another way
pairs(emmeans(VPD_HourlyAvgs_VPDlmer, specs = ~ Site|Month))
ranef(VPD_HourlyAvgs_VPDlmer)$Campus

qqnorm(resid(VPD_HourlyAvgs_VPDlmer))
qqline(resid(VPD_HourlyAvgs_VPDlmer))
hist(resid(VPD_HourlyAvgs_VPDlmer))
performance::check_model(VPD_HourlyAvgs_VPDlmer)


VPD_HourlyAvgs_Templmer <- lmer(
  VPD_mean ~  Site*Month  + (1|Campus) + (1|Temp_mean) + (1|Time)
  ,data = filter(VPD_Temp_HourlyAvgsWide, !Campus == "CSUSM"),
  REML = F)
Anova(VPD_HourlyAvgs_Templmer) #View Results another way
pairs(emmeans(VPD_HourlyAvgs_VPDlmer, specs = ~ Site|Month))
ranef(VPD_HourlyAvgs_Templmer)$Campus

qqnorm(resid(VPD_HourlyAvgs_Templmer))
qqline(resid(VPD_HourlyAvgs_Templmer))
hist(resid(VPD_HourlyAvgs_Templmer))
performance::check_model(VPD_HourlyAvgs_Templmer)




safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

Prelim_Stat_VPDMinMax <- VPD_All %>% 
  group_by(MonthDayYear, Site, Campus) %>% 
  summarize(across(.cols = c(RH, VPD), 
                   .fns = list(min = safe_min, max = safe_max)),
            .groups = "drop") %>% 
  pivot_longer(
    cols = ends_with("_min") | ends_with("_max"),
    names_to = c("Metric", "MinMax"), names_sep = "_", values_to = "Value") %>% 
  mutate(MonthDayYear = as.Date(MonthDayYear, format = "%m/%d/%y")) %>% 
  filter(!is.na(Value)) 

Avg_VPD_MaxandMin <- Prelim_Stat_VPDMinMax %>% 
  group_by(Metric, Site, Campus, MinMax) %>% 
  summarize(AvgValue = mean(Value))

ggplot(filter(Avg_VPD_MaxandMin, Metric == "VPD", !(Campus == "CSUSM")), aes(x= Campus, y=AvgValue, color = Site))+
  geom_point()+
  facet_wrap(vars(MinMax))


Avg_VPD_MaxandMin <- Avg_VPD_MaxandMin %>% 
  pivot_wider(names_from = MinMax,
              values_from = AvgValue)

write_xlsx(Avg_VPD_MaxandMin, "Avg_VPD_MaxandMin.xlsx")


Avg_VPD_MaxandMin_LM <- lmer(
  VPD_mean ~  Site + Month + TimeMonth*Time  + (1|Campus),
  data = Avg_VPD_MaxandMin,
  REML = F)

summary(Avg_VPD_MaxandMin_LM) #View Results
anova(Avg_VPD_MaxandMin_LM) #View Results another way
Anova(Avg_VPD_MaxandMin_LM) #View Results another way




#week before VPD
WeekPrior_VPD <- Prelim_Stat_VPDMinMax %>% 
  filter((MonthDayYear < as.POSIXct("2025-04-28") &
         MonthDayYear > as.POSIXct("2025-04-14") & 
         Campus != "Cal Poly") |
         (MonthDayYear < as.POSIXct("2025-05-04") &
         MonthDayYear > as.POSIXct("2025-04-21") & 
         Campus == "Cal Poly")) %>% 
  group_by(Metric, Site, Campus, MinMax) %>% 
  summarize(AvgValue = mean(Value)) %>% 
  pivot_wider(names_from = MinMax,
              values_from = AvgValue)

