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
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}

#If needed
if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}
if(!require(DHARMa)){
  install.packages("DHARMa")
  require(DHARMa)
}
if(!require(performance)){
  install.packages("performance")
  require(performance)
}

#Set wd & check it
setwd("~/Downloads/All Data/Statistics/")
getwd()
#Download sheet & read only 1-8
#Prelim_Stat_All <- read_xlsx("Preliminary Statistics Mastersheet 2025.xlsx")
#Prelim_Stat_All <- Prelim_Stat_All[, -c(9, 10)]

#Raw LMER
AllSun_lmer <- lmer(
  Sun ~  (1|Time) + Site*Month + (1|Campus),
  data = mutate(filter(Prelim_Stat_All, !is.na(Sun)),
                Time = factor(format(DateTime, "%H:%M:%S"))),
  REML = F)
Anova(AllSun_lmer) #View Results another way
pairs(emmeans(AllSun_lmer, specs = ~ Site))

#Wide Averages (skip)
# Prelim_Stat_HourlyAvgsWide <- Prelim_Stat_All %>%
#   #filter(Time >= hms("20:00:00") | Time <= hms("06:30:00")) %>%
#   filter(Time <= hms("20:00:00") | Time >= hms("06:30:00")) %>%
#   group_by(Time, Site, Campus, Month
#            ) %>%
#   summarize(
#     across(
#       c(Ambient, Sun, Shade),
#       .fns = list(
#         mean = ~mean(.x, na.rm = TRUE),
#         sd   = ~sd(.x, na.rm = TRUE),
#         n    = ~sum(!is.na(.x)),
#         se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
#         cv   = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
#       .names = "{.col}_{.fn}")) %>%
#   ungroup()

#write_xlsx(Prelim_Stat_All, "Prelim Stat All 2026.xlsx")

Prelim_Stat_HourlyAvgsLong <- Prelim_Stat_All %>%
  #filter(Time >= hms("20:00:00") | Time <= hms("06:30:00")) %>%
  filter(Time <= hms("20:00:00") & Time >= hms("06:30:00")) %>%
  pivot_longer(cols = c(Ambient, Sun, Shade), names_to = "Location", values_to = "Temperature") %>% 
  group_by(Time, Site, Campus, Month, Location) %>%
  summarize(
        mean = mean(Temperature, na.rm = TRUE),
        sd   = sd(Temperature, na.rm = TRUE),
        n    = sum(!is.na(Temperature)),
        se   = sd(Temperature, na.rm = TRUE) / sqrt(sum(!is.na(Temperature))),
        cv   = sd(Temperature, na.rm = TRUE) / mean(Temperature, na.rm = TRUE) * 100) %>%
  ungroup() %>% 
  filter(n >= 3,
         #!(Campus == "Cal Poly" & Month == "Mar")
         )

#check spread
Prelim_Stat_HourlyAvgsLong %>%
  count(n) 
  #%>% mutate(nn = nn/24)
head(arrange(filter(Prelim_Stat_HourlyAvgsLong, n>0), n), n=20)


  
# McGinnis_Pref <- Prelim_Stat_All %>% 
#   filter(!is.na(Shade)) %>% 
#   mutate(McGinnis = if_else(Shade < 35.9, T, F),
#          Time = as.factor(as.character(Time)), 
#          Site = as.factor(Site), 
#          Campus = as.factor(Campus))
McGinnis_threshold <- 35.9
McGinnis_Pref <- Prelim_Stat_All %>% 
  filter(Time <= hms("20:00:00") & Time >= hms("06:30:00")) %>% 
  mutate(
    Time = as.numeric(Time)/3600,
    across(c(Sun, Shade, Ambient),
           ~ if_else(is.na(.), NA_character_,
                     if_else(. < McGinnis_threshold, "under", "over")),
           .names = "{.col}_Threshold"),
    across(c(Sun_Threshold, Shade_Threshold, Ambient_Threshold), ~as.factor(.x))) %>%
  pivot_longer(
    cols = c(Sun_Threshold, Shade_Threshold, Ambient_Threshold),
    names_to = "Location",
    values_to = "Threshold")

Aggregated_McGinnis_Shade <- McGinnis_Pref %>%
  filter(!is.na(Threshold)) %>%
  group_by(Campus, Site, Month, Time, Location) %>%
  summarise(
    n_under = sum(Threshold == "under"),
    n_over  = sum(Threshold == "over"),
    n_total = n(),
    .groups = "drop")

McGinnis <- glmer(
  cbind(n_over, n_under) ~ Site*Month + poly(Time, 2) + (1|Campus),
  data = filter(Aggregated_McGinnis_Shade, 
                Location == "Shade_Threshold"),
  family = binomial)
summary(McGinnis)
Anova(McGinnis)

library(ggeffects)
pred_time_site <- ggpredict(McGinnis, terms = c("Time [all]", "Site"))
ggplot(pred_time_site, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  labs(
    x = "Time (hours)",
    y = "Predicted probability",
    color = "Site",
    fill = "Site"
  ) +
  theme_classic()

pred_time_site_month <- ggpredict(McGinnis, terms = c("Time [all]", "Site", "Month"))

# McGinnis_Agg_lmer <- glmer(
#   cbind(n_under, n_total - n_under) ~ 
#     Site * Month + poly(Time_sec, 2) + (1 | Campus),
#   data = filter(Aggregated_McGinnis, is.na()),
#   family = binomial
# )

#Return to the time piece

ggplot(McGinnis_Pref, aes(x = Site, fill = McGinnis)) +
  geom_bar()


Burgos_Pref <- Prelim_Stat_HourlyAvgsLong %>% 
  filter(Temperature > 32.74,
         Temperature < 34.98)

McGinnis_Pref %>% 
  filter(Campus == "Cal Poly") %>% 
  ggplot(aes(x=DateTime, y=Temperature, color=Site))+
  facet_wrap(vars(Location))+
  geom_point()

#write_xlsx(Prelim_Stat_HourlyAvgsWide, "Prelim_Stat_HourlyAvgsWide.xlsx")

#change time to hsm*
# Prelim_Stat_HourlyAvgs_hms <- Prelim_Stat_HourlyAvgsWide %>%
# filter(Campus == "UCLA") %>%
# mutate(Time = hms(Time))


#check accuracy
nrow(Prelim_Stat_HourlyAvgsWide)
(length(unique(Prelim_Stat_HourlyAvgsWide$Campus))*length(unique(Prelim_Stat_HourlyAvgsWide$Time))*length(unique(Prelim_Stat_HourlyAvgsWide$Site)))
#unique(Prelim_Stat_HourlyAvgs$Time)
#Prelim_Stat_All %>% filter(is.na(Time))
#GRAPHS
#count NAs
summary(is.na(Prelim_Stat_HourlyAvgsWide$Sun_cv))
#Plotting SUN
ggplot(filter(Prelim_Stat_HourlyAvgsLong, 
              #Location == "Sun", Campus == "Cal Poly", 
              #Month == "Feb"
              ),
       aes(x = as.numeric(Time), y = mean, shape = Campus))+
  facet_wrap(vars(Month), drop = TRUE)+
  #facet_grid(Month ~ Site) +
  #geom_point()+
  #scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  #stat_smooth(aes(color = Site), method = "loess", span = 0.3, se = FALSE, alpha = .2)+
  geom_smooth(aes(color = Site), method = "loess", span = 0.3, se = FALSE, alpha = .2, linewidth = 1.5)+
  labs(title = "Daytime Sun OTM",
       y = "Mean Temperature")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#count NA
summary(is.na(Prelim_Stat_HourlyAvgsLong$mean))
#Plotting SHADE
ggplot(filter(Prelim_Stat_HourlyAvgsLong, 
              Location == "Shade",
              #Campus == "CSUSB",
              #Site == "On"
              ),
       aes(x = Time, y = mean, color = Month, shape = Site
             ))+
  facet_wrap(vars(Campus), drop = TRUE)+
  geom_point()+
  #stat_smooth(method = "loess", span = 0.3, se = FALSE)+
  labs(title = "Nighttime Shade OTM CV",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#count NA
summary(is.na(Prelim_Stat_HourlyAvgsWide$Ambient_cv))
#Plotting AMBIENT
ggplot(mutate(filter(Prelim_Stat_HourlyAvgsLong, 
              Location == "Ambient",
              Time >= hms("20:00:00") | Time <= hms("06:30:00"),
              #filter(Time <= hms("20:00:00") | Time >= hms("07:00:00")) %>%
              #!is.na(Site),
              !Campus == "CSUSM",
              #Site == "On"
              ), NightTime = if_else(Time >= hms("20:00:00"),
                        as.numeric(Time - hms("20:00:00"), "hours"),
                        as.numeric(Time + hms("04:00:00"), "hours")  # 24 - 20 = 4
  )),
       aes(x = NightTime, y = mean, color = Month, shape = Site))+
  #facet_wrap(vars(Campus), drop = TRUE)+
  facet_grid(Campus ~ Site) +
  geom_point()+
  #stat_smooth(method = "loess", span = 0.3, se = FALSE)+
  labs(title = "Ambient CV",
       y = "Temperature (Cº)",
       #x = "Time (24-Hr)"
       x = "Hours Between 8:00 pm - 6:30pm"
       )+
  #scale_x_time(
   # breaks = hms::as_hms(seq(0, 86400, by = 14400)),
  # labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(filter(Prelim_Stat_HourlyAvgsLong, 
                     Location == "Ambient",
                     #Campus == "Cal Poly"
                     ),
aes(x = Time, y = cv, color = Month, shape = Site)) +
  #facet_wrap(vars(Campus), drop = TRUE)+
  facet_grid(Campus ~ Site) +
  geom_point()+
  #stat_smooth(method = "loess", span = 0.3, se = FALSE)+
  labs(title = "Ambient CV",
       y = "Temperature (Cº)",
       #x = "Time (24-Hr)"
       x = "Time (24 Hr)"
  )+
  scale_x_time(
   breaks = hms::as_hms(seq(0, 86400, by = 14400)),
   labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(filter(Prelim_Stat_All, Month =="Mar", Site == "Off"), aes(x = as.numeric(Time), group = MonthDayYear, y = Shade, color = MonthDayYear)) + geom_point()



summary(is.na(Prelim_Stat_All))
#individual campus All
Prelim_Stat_All %>% 
  #mutate(Time = hms(Time)) %>% 
  filter(!is.na(Site)
         ,Campus == "Whittier"
         ) %>% 
  ggplot(aes(x = Time, y = Ambient, color = Month))+
  facet_wrap(vars(Site)) +
  #geom_point()+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Sun OTM Temperature",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Checking for errors
Prelim_Stat_All %>% 
  #mutate(
    #Time = hms(Time),
         #MonthDayYear = mdy(MonthDayYear)) %>% 
  filter(!is.na(Site)
        , Campus == "Whittier"#, Site == "Off"
         #,DateTime > as.POSIXct("2025-07-14 00:00:00")
         #,DateTime < as.POSIXct("2025-08-17 00:00:00") 
  ) %>% 
  ggplot(aes(x = Time, y = Ambient, group = Month))+
  facet_wrap(vars(Site)) +
  geom_point(aes(color = MonthDayYear), alpha = 0.5)+
  stat_smooth(aes(color = Month), method = "loess", span = 0.3,
              se = FALSE, color = "white")+
  labs(#title = "Shade Temperature",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M")
    labels = scales::label_time(format = "%H")
    ) +
  scale_color_gradientn(
    colors = viridis::viridis(10)) +
  theme_classic(base_size = 17)
 #+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(filter(Prelim_Stat_HourlyAvgsWide, 
              !is.na(Site),
              !(Campus == "CSUSM"),
              Site == "On"
),
aes(x = Time, y = Ambient_mean, color = Site))+
  facet_grid(Campus ~ Month)+
  geom_point()+
  #stat_smooth(method = "loess", span = 0.3, se = FALSE)+
  labs(title = "Ambient Temperature",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Daily AVERAGES STATISTICS
#Lms
#HourlyAvgs_Sun_LM <- lm(Sun ~ Site + Campus + Time, Prelim_Stat_HourlyAvgs)
#HourlyAvgs_Shade_LM <- lm(Shade ~ Site + Campus + Time, Prelim_Stat_HourlyAvgs)
#HourlyAvgs_Amb_LM <- lm(Ambient ~ Site + Campus + Time, Prelim_Stat_HourlyAvgs)

#SHADE followup
#pairs(emmeans(HourlyAvgs_Shade_LM, specs = ~ Site))
#pairs(emmeans(HourlyAvgs_Shade_LM, specs = ~ Campus))

#original factor
# Prelim_Stat_HourlyAvgs_lmer <- Prelim_Stat_HourlyAvgsLong %>%
#   filter(!is.na(mean),) %>% 
#   mutate(Time = as.factor(as.character(Time)),
#          Site = as.factor(Site), 
#          Campus = as.factor(Campus))

#From factor to numeric
Prelim_Stat_HourlyAvgs_lmer <- Prelim_Stat_HourlyAvgsLong %>%
  filter(!is.na(mean)) %>% 
  mutate(Time = as.numeric(#hms(
    Time#)
                           )/3600, 
         Site = as.factor(Site), 
         Campus = as.factor(Campus),
    Month = as.factor(Month))
#sun mean & cv
HourlyAvgs_lmer_SunMean <- lmer(log(mean) ~ Site*Month + poly(Time,4) +
                                (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Sun"))
HourlyAvgs_lmer_SunCV <- lmer(log(cv)~Site*Month+poly(Time,4)+
                                (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Sun"))
#shade mean & cv
HourlyAvgs_lmer_ShadeMean <- lmer(log(mean) ~ Site*Month + poly(Time,4) +
                                    (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),, 
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Shade"))
HourlyAvgs_lmer_ShadeCV <- lmer(log(cv) ~ Site*Month + poly(Time,4) +
                                  (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),, 
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Shade"))
#ambient mean & cv
HourlyAvgs_lmer_AmbMean <- lmer(log(mean) ~ Site*Month + poly(Time,4) +
                                  (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),, 
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Ambient",
                                      !Campus == "CSUSM"))
HourlyAvgs_lmer_AmbCV <- lmer(log(cv) ~ Site*Month + poly(Time,4) +
                                (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus),, 
                        data = filter(Prelim_Stat_HourlyAvgs_lmer,
                                      Location == "Ambient",
                                      !Campus == "CSUSM"))

#Anova(HourlyAvgs_lmer_SunMean) 
anova(HourlyAvgs_lmer_ShadeMean)
pairs(emmeans(HourlyAvgs_lmer_AmbMean, specs = ~ Site|Month, adjust = "sidak"))
#ranef(HourlyAvgs_lmer)

qqnorm(resid(HourlyAvgs_lmer_SunMean))
qqline(resid(HourlyAvgs_lmer_SunMean))
hist(resid(HourlyAvgs_lmer_SunMean))
#check_collinearity(HourlyAvgs_lmer)
check_model(HourlyAvgs_lmer_SunMean)
DHARMa::checkModel(HourlyAvgs_lmer_SunMean)


# plot(simulateResiduals(fittedModel = HourlyAvgs_lmer, plot = F))
# qqplot(your_data, rnorm(length(your_data)))

pairs(emmeans(HourlyAvgs_lmer, specs = ~ Site))
ranef(HourlyAvgs_lmer)
#contrasts(HourlyAvgs_lmer)

#Return to the time piece
HourlyAvgs_lmer <- lmer(
  Shade_mean ~  Site + Month + Site*Month + (1|Time) + (1|Campus),
  data = mutate(filter(
    Prelim_Stat_HourlyAvgsWide,
    Shade_n > 0),
    Time = factor(as.character(Time))), REML = F)


ggplot(
  filter(
    mutate(
      Prelim_Stat_HourlyAvgsLong, 
      Month_num = match(as.character(Month), month.abb),
      Time = as.numeric(Time)), Location == "Sun", Month == "Jun"),
  aes(x = Time, y = mean, color = Campus, group = interaction(Campus, Site), shape= Site)) +
  facet_grid(vars(Month)) +
  geom_point() +
  geom_line()


okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7"  # reddish purple
)


library(ggeffects)

predictors <- ggpredict(HourlyAvgs_lmer_SunMean, terms = c("Time [all]", "Site", "Month"))
model_data <- as.data.frame(predictors)
ggplot(model_data, aes(x = x, y = predicted, color = group)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, linewidth = .5) +
  facet_wrap(~ facet)


Temp_Graph <- Prelim_Stat_HourlyAvgsLong %>%
  filter(
    #Location == "Ambient",
     !Campus == "CSUSM", #& Location == "Ambient", 
     !(Campus == "UCLA" & Site == "On" & Location == "Sun")) %>%
  mutate(
    Month_num = match(as.character(Month), month.abb),
    Time_hr = time_length(Time, "hours"),
    grey_site = if_else(Site == "On", "grey50", "grey75"))

# Reference line: average across campuses
Temp_Graph_Ref <- Temp_Graph %>%
  group_by(Site, Location, Month, Time_hr) %>%
  summarise(
    mean_ref = mean(mean, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(color_site = if_else(
    Site == "On", okabe_ito[1], okabe_ito[2]))
# HourlyAvgs_lmer_vis <- lmer(
#   mean ~ Site * Month + Site * Time + (1|Campus),
#   data = filter(Prelim_Stat_HourlyAvgs_lmer, Location == "Shade"))
# Temp_Graph_emm_Ref <- emmeans(
#   HourlyAvgs_lmer_vis,
#   ~ Site | Month + Time
# ) %>%
#   as.data.frame() %>%
#   mutate(Time_hr = time_length(Time, "hours"))
ggplot() +
  # campuses (grey)
  geom_line(data = Temp_Graph,
  aes(x = Time_hr, y = mean, group = interaction(Campus, Site), color = grey_site), alpha = 0.6, na.rm = T, linewidth = .5) +
  scale_color_identity(
    name = "Site",
    breaks = okabe_ito[c(1,2)],
    labels = c("Non-urban", "Urban"),
    guide = "legend") +
  # geom_smooth(data = Temp_Graph,
  #             se = F,
  #            aes(x = Time_hr, y = mean, group = interaction(Campus, Site)),
  #   color = "grey70", alpha = 0.6) +
  # geom_line(
  #   data = Temp_Graph_emm_Ref,
  #   aes(x = Time_hr, y = emmean, color = Site,
  #       group = interaction(Site, Month)),
  #   linewidth = 1.4) +
  geom_line(
    data = Temp_Graph_Ref,
    aes(x = Time_hr, y = mean_ref, color = color_site),
    linewidth = 1.2) +
  # geom_line(
  #   data = Temp_Graph_Ref,
  #   aes(x = Time_hr, y = mean_ref, color = Site),
  #   linewidth = 1.2) +
  #facet_grid(cols = vars(Month)) +
  facet_grid(Location ~ Month, scales = "free_y") +
  # scale_color_manual(values = okabe_ito[c(1,2)],
  #                   labels = c("On"  = "Urban", "Off" = "Non-urban")) +
  scale_x_continuous(
    name = "Time of Day",
    breaks = seq(6, 18, 4)) +
  labs(y = "Temperature (Cº)",
       color = "Site") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
        )



safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

Prelim_Stat_DayMinMax <- Prelim_Stat_All %>% 
  group_by(MonthDayYear, Site, Campus) %>% 
  summarize(across(.cols = Ambient:Shade, .fns = list(min = safe_min, max = safe_max)),
            .groups = "drop")

Avg_MinMax <- Prelim_Stat_DayMinMax %>% 
  filter(MonthDayYear < mdy("05-01-2025") | MonthDayYear > mdy("06-08-2025") & MonthDayYear < mdy("09-08-2025")) %>% 
  mutate(Season = ifelse(MonthDayYear < mdy("05-01-2025"), "Spring", "Fall")) %>% 
  group_by(Campus, Site, Season) %>% 
  summarize(
    across(c(Ambient_min, Ambient_max, Sun_min, Sun_max, Shade_max, Shade_max),
    ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

Prelim_Stat_DayMinMax <- Prelim_Stat_DayMinMax %>% 
  pivot_longer(
    cols = ends_with("_min") | ends_with("_max"),
    names_to = c("Location", "MinMax"),
    names_sep = "_",
    values_to = "Temperature"
  ) %>% 
  mutate(MonthDayYear = as.Date(MonthDayYear, format = "%m/%d/%y")) %>% 
  filter(!is.na(Temperature))


Prelim_Stat_MinMaxDiff <- Prelim_Stat_DayMinMax %>%
  pivot_wider(
    names_from = MinMax,
    values_from = Temperature
  ) %>%
  mutate(
    daily_breadth = max - min
  )
  
summary(is.na(Prelim_Stat_DayMinMax))
Prelim_Stat_DayMinMax %>% 
  filter(
    #!is.na(Temperature), !is.na(MonthDayYear), !is.na(Site)
         #Location == "Ambient"
          #Campus == "Cal Poly"
         #MinMax == "max"
  ) %>% 
  ggplot(aes(x = as.Date(MonthDayYear), y = Temperature, color = Site, shape = MinMax))+
  #facet_wrap(vars(Location)) +
  facet_grid(Location ~ Campus)+
  geom_point()+
  stat_smooth(aes(group = interaction(Site, MinMax), 
                  linetype = MinMax),
              #method = "lm",
              method = "loess", span = 0.8,
              se = FALSE)+
  labs(title = "Min & Max Temp per Day",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Prelim_Stat_MinMaxDiff %>% 
  filter(
    #!is.na(Temperature), !is.na(MonthDayYear), !is.na(Site)
    #Location == "Ambient"
    Campus == "CSULB"
    #MinMax == "max"
  ) %>% 
  ggplot(aes(x = as.Date(MonthDayYear), 
             y = daily_breadth, 
             color = Site))+
  facet_wrap(vars(Location)) +
  geom_point()+
  stat_smooth(
              #method = "lm",
              method = "loess", span = 0.6,
              se = FALSE)+
  labs(title = "Daily Breadth of Temps",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Linear Model for Sun Max
DayMax_Sun_LM <- Prelim_Stat_DayMinMax %>% 
  filter(
    Location == "Sun",
    MinMax == "max"
    ) %>% 
  lm(Temperature ~ Site + Time + Month*Time + (1|Campus), data = .)
# (NOT significant)
anova(DayMax_Sun_LM)
#MaxSun followup for "mean difference between Off and On averaged over Campus"
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}
pairs(emmeans(DayMax_Sun_LM, specs = ~ Site))
pairs(emmeans(DayMax_Sun_LM, specs = ~ Campus))



Temp_For_Deanne <- Prelim_Stat_HourlyAvgsLong %>% 
  filter(Campus == "CSUSB") %>% 
  mutate(Logger = "Temperature", 
         Time = as.numeric(hms(Time))/3600)
SM_For_Deanne <- SM_HourlyAvgsLong %>% 
  filter(Campus == "CSUSB") %>% 
  mutate(Logger = "SM", 
         Time = as.numeric(Time)/3600)
VPD_For_Deanne <- VPD_HourlyAvgsLong %>% 
  filter(Campus == "CSUSB") %>% 
  mutate(Logger = "VPD", 
         Time = as.numeric(Time)/3600)
For_Deanne <- Temp_For_Deanne %>%
  full_join(SM_For_Deanne, by = c("Time", "Site", "Campus", "Month", "Location", "mean", "sd", "n", "se", "cv", "Logger")) %>%
  full_join(VPD_For_Deanne, by = c("Time", "Site", "Campus", "Month", "mean", "sd", "n", "se", "cv", "Logger"))
write_xlsx(For_Deanne, "CSUSB_Abiotics.xlsx")



#DIFFERENCES
#Longest Format
Prelim_Stat_Longest <- Prelim_Stat_All %>% 
  pivot_longer(cols = c(Ambient, Sun, Shade),
               names_to = "Location", 
               values_to = "Temperature")

#wider ON OFF columns format with difference
Prelim_Stat_OnOff <- Prelim_Stat_Longest %>%
  group_by(DateTime, Campus, Location) %>%
  pivot_wider(
    names_from = Site,
    values_from = Temperature) %>%
  drop_na(c(On, Off)) %>% 
  mutate(temp_diff = On - Off) %>%
  ungroup()

#HMS for graphing purposes
Graph_OnOff_hms <- Prelim_Stat_OnOff %>% 
  group_by(Time, Campus, Location) %>% 
  filter(!is.na(temp_diff)) %>% 
  summarise(
    avg_temp_diff = mean(temp_diff, na.rm = TRUE),
    sd_temp_diff = sd(temp_diff, na.rm = TRUE),
    n = n(),
    se = sd(temp_diff)/sqrt(n),
    cv = sd(temp_diff)/mean(temp_diff) * 100,
    .groups = "drop") %>% 
  mutate(Time = hms(Time)) %>% 
  filter(cv > -500 & cv < 500)

#Graphing avg difference faceted by Location
ggplot(Graph_OnOff_hms,
       aes(x = Time, y = cv, color = Campus)) +
  facet_wrap(
    vars(Location), 
    drop = TRUE)+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  stat_smooth(method = "loess", span = 0.2,
              se = FALSE)+
  #labs(title = "Mean Temp Difference between On & Off",
   #    y = "Temperature (Cº)",
    #   x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)

#Indvidual Campus
ggplot(filter(Graph_OnOff_hms, Campus == "Whittier"),
       aes(x = Time, y = avg_temp_diff, color = Location)) +
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", 
           x = hms::as_hms("00:00:00"), 
           y = 5, label = "On hotter than Off", hjust = 0, size = 4) +
  annotate("text", 
           x = hms::as_hms("00:00:00"), 
           y = -5, label = "Off hotter than On", hjust = 0, size = 4) +
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Mean Temp Difference between On & Off",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M"),
    labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)
#+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Graphing avg difference faceted by campus
ggplot(Graph_OnOff_hms,
       aes(x = Time, y = avg_temp_diff, color = Location)) +
  facet_wrap(
    vars(Campus), 
    drop = TRUE)+
  geom_point()+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Mean Temp Difference between On & Off",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


HourlyDiffs_lmer <- lmer(
  Sun_cv ~  (1|Time) + Site + (1|Campus),
  data = Prelim_Stat_HourlyAvgsWide,
  REML = F)
summary(HourlyAvgs_lmer) #View Results
anova(HourlyAvgs_lmer) #View Results another way
Anova(HourlyAvgs_lmer) #View Results another way



#Graphing variance
Graph_OnOff_hms %>% 
  #filter(Campus == "Cal Poly") %>% 
  ggplot(aes(x = Time, y = var_temp_diff, color = Campus)) +
  facet_wrap(vars(Location), drop = TRUE)+
  geom_point()+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Variance of the Difference in Temp between On & Off",
       y = "Sampling Variance of Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Indvidual Campus Variance
ggplot(filter(Graph_OnOff_hms, Campus == "Cal Poly"),
       aes(x = Time, y = var_temp_diff, color = Location)) +
  facet_wrap(vars(Campus), drop = TRUE)+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Standard Deviation of Temp Differences",
       subtitle = "Differences between On & Off",
       y = "STDEV of Differences (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M"),
    labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)

#ALSO Graphing Variance in Differences
Graph_OnOff_hms %>% 
  #filter(Campus == "Cal Poly") %>% 
  ggplot(aes(x = Time, y = var_temp_diff, color = Location)) +
  facet_wrap(vars(Campus), drop = TRUE)+
  geom_point()+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Variance of the Difference in Temp between On & Off",
       y = "Sampling Variance of Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M"),
    labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)








Graph_OnOff_DMY <- Prelim_Stat_OnOff %>% 
  group_by(MonthDayYear, Campus, Location) %>% 
  summarise(
    avg_temp_diff = mean(temp_diff, na.rm = TRUE),
    var_temp_diff = var(temp_diff, na.rm = TRUE),
    sd_temp_diff = sd(temp_diff, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(MonthDayYear = mdy(MonthDayYear))

ggplot(filter(Graph_OnOff_DMY, Campus == "Scripps"),
       aes(x = MonthDayYear, y = var_temp_diff, color = Location)) +
  facet_wrap(vars(Campus), drop = TRUE)+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE)+
  labs(title = "Standard Deviation of Temp Differences",
       subtitle = "Differences between On & Off",
       y = "STDEV of Differences (Cº)",
       x = "Date")+
  #scale_x_time(
    #breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M"),
    #labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)
















#Check
Prelim_Stat_OnOff %>%
  select(!c(Time, Month, MonthDayYear)) %>%
  filter(Campus == "Cal Poly",
         Location == "Ambient"
         ) %>% 
  arrange(desc(temp_diff)) %>% 
  head(20)


ggplot(
  mutate(
    filter(
      Prelim_Stat_All, 
      Campus == "Cal Poly",
      DateTime > as.POSIXct("2025-06-01 00:00:00"),
      DateTime < as.POSIXct("2025-06-02 00:00:00") 
    ), 
    Time = hms(Time)),
  aes(x = DateTime, y = Sun, color = Site))+
  geom_point(alpha = .75)


ggplot(
  mutate(
    filter(Prelim_Stat_OnOff, 
           Campus == "Cal Poly",
           Location == "Sun",
           DateTime > as.POSIXct("2025-06-01 00:00:00"),
           DateTime < as.POSIXct("2025-06-02 00:00:00")), 
    Time = hms(Time)),
       aes(x = DateTime, 
           y = temp_diff,
          color = Location
  ))+
  labs(title = "Sun OnOff Differences")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(alpha = .75)
  



#Unfinished nonsesnse
#Prelim_Stat_Longest
unique(Prelim_Stat_Longest$Time)

Daylight_Hours <- Prelim_Stat_Longest %>% 
  filter(!is.na(Temperature), Campus == "Cal Poly") %>% 
  group_by(DateTime, Site, Campus, Location, Temperature) %>% 
  mutate(Time = hms(DateTime),
         day_avg = mean(Temperature, na.rm = TRUE),
         day_bredth = 
           mean(head(sort(, decreasing = TRUE), 15), na.rm = TRUE) - 
           mean(head(sort(Temperature), 15), na.rm = TRUE),
         max_temp = max(Temperature, na.rm = TRUE),
         time_max = Time[which.max(Temperature)]) %>% 
  mutate(BeforeMid = Temperature > max_temp - ((1/4)*day_bredth) & Time < time_max,
         AfterMid = Temperature > max_temp - ((1/5)*day_bredth) & Time > time_max) %>% 
  ungroup()

summary(Daylight_Hours$AfterSet)

ggplot(filter(Daylight_Hours, 
    #Campus == "Cal Poly",Site == "On", Location == "Sun",
    DateTime > as.POSIXct("2025-06-01 00:00:00"),
    DateTime < as.POSIXct("2025-06-02 00:00:00")),
  aes(x = DateTime, y = Temperature, color = AfterMid, shape = Site))+
  facet_wrap(vars(Location))+
  geom_point(alpha = .75)

Daylight_Check <- Daylight_Hours %>% 
  group_by(Location, Site, Campus, MonthDayYear) %>% 
  summarise(SunBefore = sum(BeforeMid == TRUE), SunAfter = sum(AfterMid == TRUE)) %>% 
  ungroup() %>% arrange(desc(SunBefore)) %>% filter(Location == "Sun")



#Lunatic ramblings
Daylight_Hours_ramblings <- Prelim_Stat_All %>% 
  mutate(Time = hms(Time),
         #MonthDayYear = mdy(MonthDayYear)
         ) %>% 
  filter(Time < hms("08:00:00") | Time > hms("20:00:00")) %>% 
  group_by(Site, Campus, MonthDayYear) %>% 
  summarise(OvernightShade = mean(Shade))

ggplot(Daylight_Hours_ramblings, aes(x=MonthDayYear, y=Shade, color = Site))+
  geom_point()+
  facet_wrap(vars(Campus))+
  labs(title = "Overnight Shade Averages")

lunatic <- Daylight_Hours_ramblings %>% 
  full_join(Prelim_Stat_All, by = c("Campus", "MonthDayYear", "Site")) %>% 
  group_by(Site, Campus, MonthDayYear) %>% 
  filter(Sun >= 1.25*OvernightShade)

lunatic %>% 
  mutate(Time = hms(Time),
         MonthDayYear = mdy(MonthDayYear)) %>% 
  filter(!is.na(Site)
         , Campus == "Cal Poly") %>% 
  ggplot(aes(x = Time, y = Sun, group = Site))+
  facet_wrap(vars(Site)) +
  geom_point(aes(color = MonthDayYear), alpha = 0.5)+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE, color = "white")+
  labs(title = "Baseless Sun Window",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M")
    labels = scales::label_time(format = "%H")
  ) +
  scale_color_gradientn(
    colors = viridis::viridis(10)) +
  theme_classic(base_size = 17)
#+theme(axis.text.x = element_text(angle = 45, hjust = 1))











#Use the code below for determining sun windows
summary(is.na(Earliest_ShadedSun))

#Earliest time of sunshade using max shade
Earliest_ShadedSun <- Prelim_Stat_All %>%
  mutate(Date = as_date(DateTime)) %>%
  filter(
    #Campus == "Cal Poly",
         if_all(c(Ambient, Shade, Sun), ~ !is.na(.))) %>% 
  group_by(Date, Site, Campus) %>% 
  mutate(MaxSunTime = DateTime[which.max(Sun)],
         AvgMaxShade = max(head(sort(Shade, decreasing = TRUE), 5), 
                           na.rm = TRUE)) %>%  
  mutate(MonthDayYear = mdy(MonthDayYear),
         Time = hms(Time)) %>%
  filter(DateTime > MaxSunTime, 
         Sun <= AvgMaxShade,
         !(Campus == "Cal Poly" & MonthDayYear == mdy("07/10/2025")),
         !(Campus == "Whittier" & MonthDayYear == mdy("07/24/2025"))) %>% 
  slice_min(order_by = DateTime, n = 1) %>%  
  ungroup()
#same thing but using average shade value
Earliest_ShadedSun <- Prelim_Stat_All %>%
  mutate(Date = as_date(DateTime)) %>%
  filter(
    #Campus == "Cal Poly",
    if_all(c(Ambient, Shade, Sun), ~ !is.na(.))) %>% 
  group_by(Date, Site, Campus) %>% 
  mutate(MaxSunTime = DateTime[which.max(Sun)],
         AvgMaxShade = mean(head(sort(Shade, decreasing = TRUE), 15), 
                           na.rm = TRUE)) %>%  
  mutate(MonthDayYear = mdy(MonthDayYear),
         Time = hms(Time)) %>%
  filter(DateTime > MaxSunTime, 
         Sun <= AvgMaxShade,
         !(Campus == "Cal Poly" & MonthDayYear == mdy("07/10/2025")),
         !(Campus == "Whittier" & MonthDayYear == mdy("07/24/2025"))) %>% 
  slice_min(order_by = DateTime, n = 1) %>%  
  ungroup()


Earliest_SunnedSun <- Prelim_Stat_All %>%
  mutate(Date = as_date(DateTime)) %>%
  filter(
    Campus == "Scripps",
    if_all(c(Ambient, Shade, Sun), ~ !is.na(.))) %>% 
  group_by(Date, Site, Campus) %>% 
  mutate(MaxSunTime = DateTime[which.max(Sun)],
         MaxShade = mean(head(sort(Shade, decreasing = TRUE), 15), na.rm = TRUE)) %>% 
  filter(DateTime < MaxSunTime, 
         Sun > (6/5) * MaxShade) %>% 
  slice_min(order_by = DateTime, n = 1) %>%   
  mutate(MonthDayYear = mdy(MonthDayYear),
         Time = hms(Time)) %>%
  ungroup()

ggplot(Earliest_SunnedSun, aes(x=MonthDayYear, y=MaxShade, color = Site))+
  facet_wrap(vars(Campus))+
  geom_point()+ 
  labs(title = "Maximum Temperatures for Shade Logger",
       subtitle = "Averaged from Highest 15 Values")+
  geom_smooth(se = F)


ggplot(Earliest_SunnedSun, aes(x=MonthDayYear, y=Time, color = Site))+
  facet_wrap(vars(Campus))+
  geom_point()+ labs(title = "Time of Sun Logger's First Sun")+
  geom_smooth(se = F)+ scale_y_time(labels = scales::label_time(format = "%H")) 

ggplot(
  #filter(
  Earliest_ShadedSun, 
             # Campus =="UCLA",
             # DateTime > as.POSIXct("2025-07-05 00:00:00"),
             # DateTime < as.POSIXct("2025-08-01 00:00:00")
  #),
       aes(x=MonthDayYear, y=Time, color = Site))+
  geom_point()+ labs(title = "Time of Sun Logger's full shade",
                     subtitle = "(Assuming the shade OTM is a reliable reference)") +
  geom_smooth(se = FALSE)+ scale_y_time(labels = scales::label_time(format = "%H"))+
  facet_wrap(vars(Campus))



#trying to filter for sunned loggers

Sun_Window <- rename(Earliest_ShadedSun, End_Sun = Time)  %>%
  inner_join(rename(Earliest_SunnedSun, Start_Sun = Time), by = c("Campus", "DateTime", "Site"))


Sun_Window <- Earliest_ShadedSun %>%
  rename(End_Sun_Time = DateTime) %>%
  select(Campus, Date, End_Sun_Time, Site) %>%
  inner_join(
    Earliest_SunnedSun %>%
      rename(Start_Sun_Time = DateTime) %>%
      select(Campus, Date, Start_Sun_Time, Site),
    by = c("Campus", "Date", "Site"))


Prelim_SuninSun_All <- Prelim_Stat_All %>%   
  mutate(Date = as_date(DateTime),
         Time = hms(Time)) %>%
  left_join(Sun_Window, by = c("Campus", "Date", "Site")) %>%
  mutate(
    Start_Sun_Time = hms::as_hms(Start_Sun_Time),
    End_Sun_Time = hms::as_hms(End_Sun_Time),
    Window = Time >= Start_Sun_Time & Time <= End_Sun_Time)

summary(Prelim_SuninSun_All$Window)

ggplot(filter(Prelim_SuninSun_All, 
              Campus == "Cal Poly"),
       aes(x = Time, y = Sun, color = Window))+
  facet_wrap(vars(Site)) +
  geom_point(alpha = 0.5)+
  stat_smooth(method = "loess", span = 0.3,
              se = FALSE, color = "white")+
  labs(title = "Fun in the Sun Time",
       y = "Temperature (Cº)",
       x = "Time (24-Hr)")+
  scale_x_time(
    breaks = hms::as_hms(seq(7200, 72000, by = 21600)),
    #labels = function(x) format(x, "%H:%M")
    labels = scales::label_time(format = "%H")) +
  theme_classic(base_size = 17)



Prelim_SuninSun_Only <- Prelim_SuninSun_All %>% 
  filter(Window == "TRUE") %>% 
  select(!c(End_Sun_Time, Start_Sun_Time, Window))%>%
  group_by(Time, Site, Campus) %>%
  summarize(
    across(
      c(Ambient, Sun, Shade),
      .fns = list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        n    = ~sum(!is.na(.x)),
        se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
        cv   = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{.col}_{.fn}")) %>%
  ungroup()

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

Prelim_SuninSun_Only$SiteWithinCampus <- interaction(Prelim_SuninSun_Only$Campus, Prelim_SuninSun_Only$Site, sep = "_")

SuninSun_lmer <- lmer(
  Sun_mean ~  Time + Site + (1|Campus),
  data = Prelim_SuninSun_Only,
  REML = F)
summary(SuninSun_lmer) #View Results
anova(SuninSun_lmer) #View Results another way
Anova(SuninSun_lmer) #View Results another way

ggplot(Prelim_SuninSun_Only, aes(x = Time, y = Shade_mean, color = Site))+
  geom_point()+
  facet_wrap(vars(Campus))+
  scale_x_time(breaks = hms::as_hms(seq(7200, 72000, by = 21600)))



