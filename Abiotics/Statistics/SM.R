if(!require(writexl)){
  install.packages("writexl")
  require(writexl)
}
if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
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

library(tidyverse)
library(emmeans)
library(lmerTest)
library(readxl)

#Set wd & check it
setwd("~/Downloads/All Data/")
getwd()

SM_All_PrecCalib <- read_xlsx(path = "~/Downloads/All Data/Cal Poly/Cal Poly Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "Cal Poly") |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/CSUSM/CSUSM Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "CSUSM"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/Pepperdine/Pepperdine Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "Pepperdine"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/Scripps/Scripps Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "Scripps"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/Whittier/Whittier Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "Whittier"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/UCLA/UCLA Master.xlsx","SM",
                     col_types = c("date", "numeric", "numeric", "numeric", "numeric")) |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "UCLA"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/CSUSB/CSUSB Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
  separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  #pivot_wider(names_from = Location, values_from = SM) |> 
  mutate(Campus = "CSUSB"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/CLU/CLU Master.xlsx","SM") |>
  pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
    separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
  mutate(Campus = "CLU"),
  by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  full_join(read_xlsx(path = "~/Downloads/All Data/CSULB/CSULB Master.xlsx","SM") |>
              pivot_longer(cols = -DateTime, 
               names_to = "Site_Location", 
               values_to = "SM") |>
              separate(Site_Location, into = c("Site", "Location"), sep = "_") |>
              mutate(Campus = "CSULB"),
            by = c("DateTime", "Site", "SM", "Location", "Campus")) |> 
  mutate(Month = month(DateTime, label = T, abbr = T),
         Time = hms(format(DateTime, "%H:%M:%S"))) |> 
  filter(!Month %in% c("Feb", "Oct", "Nov"))


SM_All <- SM_All_PrecCalib |>
  mutate(
    SM = case_when(
      Campus == "Cal Poly" & Site == "Off" & Location == "Shade"
      ~ SM*0.7626512473	+ 0.1157975606,
      Campus == "Cal Poly" & Site == "Off" & Location == "Sun" 
      ~ SM*0.9208141931	+ 0.06409380004,
      Campus == "Cal Poly" & Site == "On" & Location == "Shade"
      ~ SM*0.927229213 + 0.1055328493,
      Campus == "Cal Poly" & Site == "On" & Location == "Sun"
      ~ SM*0.8714872975	+ 0.1059928176,
      Campus == "CSUSB" & Site == "Off" & Location == "Shade"
      ~ SM*1.147883049 - 0.07195710998,
      Campus == "CSUSB" & Site == "Off" & Location == "Sun" 
      & DateTime < ymd("2025-04-02")
      ~ SM*0.9449593421 +	0.04963067249,
      # Campus == "CSUSB" & Site == "On" & Location == "Shade"
      # ~ SM*1.054337754 - 0.04200089063,
  #I have reason to believe this calibration may be invalid and thus it was commented out
      # Campus == "CSUSB" & Site == "On" & Location == "Sun" 
      # ~ SM*1.063970956 - 0.05517880129,
      Campus == "UCLA" & Site == "Off" & Location == "Shade"
      ~ SM*1.00754774	- 0.004184319254,
      Campus == "UCLA" & Site == "Off" & Location == "Sun"
      ~ SM*1.127664165 - 0.05965336487,
  #I have reason to believe this calibration may be invalid and thus it was commented out
      # Campus == "UCLA" & Site == "On" & Location == "Shade"
      # ~ SM*(1.794141579) - 0.1880992677,
      #No On_Sun calibration because the logger was destroyed
      Campus == "Whittier" & Site == "Off" & Location == "Shade"
      ~ SM*1.294021546 - 0.1041101845,
      Campus == "Whittier" & Site == "Off" & Location == "Sun"
      ~ SM*1.089305032 - 0.04532633929,
  #I have reason to believe this calibration may be invalid and thus it was commented out
      # Campus == "Whittier" & Site == "On" & Location == "Shade"
      # ~ SM*1.284492549 - 0.1269630441, 
      Campus == "Whittier" & Site == "On" & Location == "Sun"
      ~ SM*1.209386722 - 0.0831184615,
      Campus == "Scripps" & Site == "Off" & Location == "Shade"
      ~ SM*1.11785729	- 0.04209083718,
      Campus == "Scripps" & Site == "Off" & Location == "Sun"
      ~ SM*1.222856632 - 0.08809597617,
      Campus == "Scripps" & Site == "On" & Location == "Shade"
      ~ SM*1.063832017 - 0.02247110766,
      Campus == "Scripps" & Site == "On" & Location == "Sun" 
      ~ SM*1.180382915 - 0.05355395931,
      Campus == "Pepperdine" & Site == "Off" & Location == "Shade"
      ~ SM*1.033528382 - 0.01518944759,
      Campus == "Pepperdine" & Site == "Off" & Location == "Sun"
      ~ SM*0.9345619796 - 0.02029910189,
      Campus == "Pepperdine" & Site == "On" & Location == "Shade"
      ~ SM*0.9942100727	+ 0.01308747899,
      Campus == "Pepperdine" & Site == "On" & Location == "Sun" 
      ~ SM*1.119365525 - 0.0598695355,
      Campus == "CLU" & Site == "Off" & Location == "Shade"
      ~ SM*0.8026575529	+ 0.09798414304,
      Campus == "CLU" & Site == "Off" & Location == "Sun"
      ~ SM*0.8284017489	+ 0.08671550135,
      Campus == "CLU" & Site == "On" & Location == "Shade"
      ~ SM*0.7075278588	+ 0.1050748847,
      Campus == "CLU" & Site == "On" & Location == "Sun" 
      ~ SM*0.7596081387	+ 0.1262780708,
      #No Off_Sun for CSULB because it was chewed through early on
      Campus == "CSULB" & Site == "Off" & Location == "Sun"
      ~ SM*1.050806961 - 0.04829162008,
      Campus == "CSULB" & Site == "On" & Location == "Shade"
      ~ SM*1.455172923 - 0.1876963955,
      Campus == "CSULB" & Site == "On" & Location == "Sun" 
      ~ SM*1.181175115 - 0.07712122048,
      TRUE ~ SM
    )) |> 
  filter(!(Campus == "Pepperdine" & Site == "On" & Location == "Sun" & 
           DateTime > as.POSIXct("2025-09-12 08:00:00", tz = "UTC")),
        #!(Campus == "Whittier" & Site == "On" & Location == "Shade" & DateTime > as.POSIXct("2025-04-14 06:00:00") & DateTime < as.POSIXct("2025-04-26 03:00:00"))
         !(Campus =="Whittier" & Site == "On" & Location == "Sun" & DateTime > as.POSIXct("2025-09-05 08:00:00") & DateTime < as.POSIXct("2025-09-09 00:00:00")))
         
  # |> 9/12/25 8:00
  #  filter(!(Campus == "UCLA" & Site == "On" & Location == "Shade"),
  #         !(Campus == "CSUSM")) 
# # |> 
#   filter(!(Campus == "Cal Poly" & Site == "Off" & Location == "Sun"))
# ~ SM*,
# Campus == "" & Site == "On" & Location == "Shade"
# ~ SM*,
# Campus == "" & Site == "On" & Location == "Sun" 
# ~ SM*,

SM_All |>
  group_by(Campus) |>
  summarize(
    first_date = min(DateTime, na.rm = TRUE),
    last_date  = max(DateTime, na.rm = TRUE))


SM_HourlyAvgsLong <- SM_All |>
  group_by(Time, Site, Campus, Month, Location) |>
  summarize(
    mean = mean(SM, na.rm = TRUE),
    sd   = sd(SM, na.rm = TRUE),
    n    = sum(!is.na(SM)),
    se   = sd(SM, na.rm = TRUE) / sqrt(sum(!is.na(SM))),
    cv   = sd(SM, na.rm = TRUE) / mean(SM, na.rm = TRUE) * 100) |>
  ungroup() |> 
  filter(n >= 3)
#!(Campus == "Cal Poly" & Month == "Mar")

LM_SM_HourlyAvgsLong <- SM_HourlyAvgsLong |> 
  mutate(Time = as.numeric(Time)/3600,
         Site = factor(Site),
         Campus = factor(Campus),
         Location = factor(Location),
         Month = factor(Month, levels = month.abb[3:9])) 

lm_SM_Sun_2 <- lmerTest::lmer(mean ~ Site*Month +
                              poly(Time,2) + 
                              (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus)
                  , 
                  REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa"),
                  data = filter(LM_SM_HourlyAvgsLong, 
                                Location == "Sun"))

lm_SM_Shade <- lmerTest::lmer(mean ~ Site*Month + poly(Time,4) + 
                    (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus)
                  , 
                  REML = FALSE, 
                  control = lmerControl(optimizer = "bobyqa"),
                  data = filter(LM_SM_HourlyAvgsLong, 
                                Location == "Shade"))
# lm_SM_Sun <- lm(mean ~ Site*Month + poly(Time,4) + 
#                   Campus*Site, 
#                 data = filter(LM_SM_HourlyAvgsLong, 
#                               Location == "Sun"))

anova(lm_SM_Sun)
anova(lm_SM_Shade) 
pairs(emmeans(lm_SM_Sun, specs = ~ Site))

vif(lm_SM_Sun_2)
AIC(lm_SM_Sun_2)
plot(lm_SM_Sun_2)
qqnorm(resid(lm_SM_Sun_2))

#pairs(emmeans(lm_SM_Sun, specs = ~ Site|Campus))


summary(lm_SM_Shade)
#emmeans(lm_SM, specs = ~ Site|Month)
ranef(lm_SM)$Campus


plot(fitted(lm_SM_Sun), resid(lm_SM_Sun),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

residuals <- LM_SM_HourlyAvgsLong |>
  filter(Location == "Sun") |> 
  mutate(residuals = resid(lm_SM_Sun))



okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

SM_Graph <- SM_HourlyAvgsLong |>
  mutate(
    Month_num = match(as.character(Month), month.abb),
    Time_hr = time_length(Time, "hours"),
    grey_color = if_else(Site == "On", "grey60", "grey75"))

# Reference line: average across campuses
# SM_Graph_Ref <- SM_Graph |>
#   group_by(Site, Month, Time_hr) |>
#   summarise(
#     mean_ref = mean(mean, na.rm = TRUE),
#     .groups = "drop") |> 
#   mutate(Site_color = if_else(Site == "On", okabe_ito[2], okabe_ito[1]))

pred_SM_Sun <- emmeans(lm_SM_Sun, ~ Site | Month * Time, 
                       at = list(Time = seq(0, 23, by = .5))) |>
  as.data.frame() |>
  mutate(Time_hr = Time,
         Site = factor(Site, levels = c("On", "Off")),
         Location = "Sun")
pred_SM_Shade <- emmeans(lm_SM_Shade, ~ Site | Month * Time, 
                       at = list(Time = seq(0, 23, by = .5))) |>
  as.data.frame() |>
  mutate(Time_hr = Time,
         Site = factor(Site, levels = c("On", "Off")),
         Location = "Shade")

pred_SM <- pred_SM_Sun |> 
  full_join(pred_SM_Shade, 
            by = c("Site", "Month", "Time", "emmean", "SE", "df", "asymp.LCL", "asymp.UCL", "Time_hr", "Location")) |> 
  mutate(Site_color = if_else(Site == "On", okabe_ito[2], okabe_ito[1])) |> 
  arrange(Location, Month, Site, Time)

ggplot() +
  # campuses (grey)
  geom_line(data = SM_Graph,
            aes(x = Time_hr, y = mean, group = interaction(Campus, Site), color = grey_color, linetype = grey_color),
            alpha = 0.6, linewidth = 1, show.legend = F) +
  # geom_point(data = SM_Graph, 
  #            aes(x = Time_hr, y = mean, group = interaction(Campus, Site)),
  #            color = "grey70", alpha = 0.6) +
  # reference line (campus-averaged)
  geom_ribbon(data = pred_SM,
              aes(x = Time, ymin = asymp.LCL, ymax = asymp.UCL, 
                  fill = Site_color, group = Site
              ), alpha = .25)+
  scale_fill_identity(name = "Site",
                      breaks = okabe_ito[c(2, 1)])+
  geom_line(
    data = pred_SM,
    aes(x = Time, y = emmean, color = Site_color, group = interaction(Location, Month, Site)
        #group = Site
        ),
    linewidth = 1.2) +
  scale_color_identity(
    name = "Site",
    breaks = okabe_ito[c(2,1)],
    labels = c("Urban", "Non-urban"),
    guide = "legend") +
  facet_grid(Location ~ Month) +
  # scale_color_manual(values = okabe_ito[c(2,1)],
  #                    breaks = c("On", "Off"),
  #                    labels = c(
  #                      "On"  = "Urban",
  #                      "Off" = "Non-urban")) +
  scale_x_continuous(
    name = "Time of Day",
    breaks = seq(2, 20, 6)) +
  labs(y = expression("Soil Moisture (" * m^3 / m^3 * ")"),
       color = "Site") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))





#chat-provided fix
mod_sun <- lmer(
  mean ~ Site * Month + poly(Time, 2) + (1 | Campus),
  data = SM_model_dat |> filter(Location == "Sun"))

mod_shade <- lmer(
  mean ~ Site * Month + poly(Time, 2) + (1 | Campus),
  data = SM_model_dat |> filter(Location == "Shade"))

SM_model_dat <- SM_HourlyAvgsLong |>
  filter(Campus != "CSUSM") |>
  mutate(
    Time = as.numeric(Time)/360,
    Month = factor(Month, levels = month.abb[3:9]),   # Mar-Sep
    Site = factor(Site),
    Location = factor(Location),
    Campus = factor(Campus))

pred_campus_sun <- expand_grid(
  Time = sort(unique(SM_model_dat$Time)),
  Site = levels(SM_model_dat$Site),
  Month = levels(SM_model_dat$Month),
  Campus = levels(SM_model_dat$Campus)) |>
  mutate(
    fit = predict(mod_sun, newdata = ., re.form = NULL),
    Location = "Sun")
pred_campus_shade <- expand_grid(
  Time = sort(unique(SM_model_dat$Time)),
  Site = levels(SM_model_dat$Site),
  Month = levels(SM_model_dat$Month),
  Campus = levels(SM_model_dat$Campus)) |>
  mutate(
    fit = predict(mod_shade, newdata = ., re.form = NULL),
    Location = "Shade")
pred_campus <- bind_rows(pred_campus_sun, pred_campus_shade)

pred_ref_sun <- expand_grid(
  Time = sort(unique(SM_model_dat$Time)),
  Site = levels(SM_model_dat$Site),
  Month = levels(SM_model_dat$Month)) |>
  mutate(
    fit = predict(mod_sun, newdata = ., re.form = NA),
    Location = "Sun")
pred_ref_shade <- expand_grid(
  Time = sort(unique(SM_model_dat$Time)),
  Site = levels(SM_model_dat$Site),
  Month = levels(SM_model_dat$Month)) |>
  mutate(
    fit = predict(mod_shade, newdata = ., re.form = NA),
    Location = "Shade")
pred_ref <- bind_rows(pred_ref_sun, pred_ref_shade)

ggplot() +
  geom_line(
    data = pred_campus |> filter(Site == "On"),
    aes(x = Time, y = fit, group = Campus),
    color = "grey60",
    linewidth = 1,
    alpha = 0.6,
    show.legend = FALSE) +
  geom_line(
    data = pred_campus |> filter(Site == "Off"),
    aes(x = Time, y = fit, group = Campus),
    color = "grey75",
    linewidth = 1,
    alpha = 0.6,
    linetype = "dashed",
    show.legend = FALSE) +
  geom_line(
    data = pred_ref,
    aes(x = Time, y = fit, color = Site),
    linewidth = 1.4) +
  facet_grid(Location ~ Month) +
  scale_color_manual(
    name = "Site",
    values = c(
      "On" = okabe_ito[2],
      "Off" = okabe_ito[1]),
    labels = c(
      "On" = "Urban",
      "Off" = "Non-urban")) +
  scale_x_continuous(
    name = "Time of Day",
    breaks = seq(0, 240, 60),
    labels = c("0", "6", "12", "18", "24")) +
  labs(y = expression("Soil Moisture (" * m^3 / m^3 * ")")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18))

















Temp_SM_df <- SM_HourlyAvgsLong |> 
  full_join(Prelim_Stat_HourlyAvgsLong, 
            by = c("Time", "Campus", "Site", "Location", "Month")) |> 
  mutate(Time = as.numeric(Time)/3600)

 
ggplot(filter(Temp_SM_df, Location == "Shade"), aes(x=mean.x, y=mean.y,shape = Site, color = Month)) +
  geom_jitter()+
  labs(x = "Soil Moisture",
       y = "Temperature"
       ) +
  geom_smooth(aes(group = 1), color = "black", se = F) +
  #geom_line()
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18))

Temp_SM_lmer_df <- Temp_SM_df |>
  filter(Location == "Sun",
         !is.na(mean.x),
         !is.na(mean.x)) |> 
  mutate(Time = as.numeric(Time), 
         Site = as.factor(Site), 
         Campus = as.factor(Campus))

#Return to the time piece
Temp_SM_lmer <- lmer(
  mean.y ~  mean.x + Site*Month + Time + (1|Campus),
  data = filter(Temp_SM_lmer_df))

Anova(Temp_SM_lmer)
pairs(emmeans(Temp_SM_lmer, specs = ~ Site|Month, adjust = "tukey"))

qqnorm(resid(Temp_SM_lmer))
qqline(resid(Temp_SM_lmer))
hist(resid(Temp_SM_lmer))
performance::check_model(Temp_SM_lmer)
























ggplot(filter(SM_All, 
              #Campus == "Cal Poly",
              #DateTime >= as.POSIXct("2025-08-09 09:00:00", tz = "UTC"), 
              #DateTime <= as.POSIXct("2025-09-01 00:00:00", tz = "UTC")
              ), aes(x=DateTime, y=SM, color=Site, shape = Location))+
  geom_point()+
  facet_wrap(vars(Campus))

#comparing sites
ggplot(filter(SM_All, 
              Location == "Shade",
              Campus == "Whittier",
             #DateTime > as.POSIXct("2025-04-14 00:00:00"),
              #DateTime < as.POSIXct("2025-04-14 08:00:00")
), aes(x=DateTime, y=SM, color=Campus))+
  labs(title = "Shade SM")+
  geom_point()+
  facet_wrap(vars(Site))


ggplot(filter(SM_All, 
              Location == "Sun",
              Campus == "Whittier",
              DateTime > as.POSIXct("2025-09-01 08:00:00"),
              DateTime < as.POSIXct("2025-10-30 00:00:00")
), aes(x=DateTime, y=SM, color=Campus))+
  labs(title = "Sun SM")+
  geom_point()+
  facet_wrap(vars(Site))


# head(SM_All |> filter(
#   Campus == "UCLA",
#   Site == "Off",
#   Location == "Sun",
#   DateTime > as.POSIXct("2025-05-15 00:00:00"),
#   DateTime < as.POSIXct("2025-06-015 00:00:00")))


#Not reliable
# SM_HourlyAvgsWide <- SM_All |> 
#   pivot_wider(names_from = Location,
#               values_from = SM) |> 
#   mutate(Time = format(DateTime, "%H:%M:%S")) |> 
#   group_by(Time, Site, Campus, Month) |>
#   summarize(
#     across(
#       c(Sun, Shade),
#       .fns = list(
#         mean = ~mean(.x, na.rm = TRUE),
#          sd   = ~sd(.x, na.rm = TRUE),
#          n    = ~sum(!is.na(.x)),
#          se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
#          cv   = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100
#         ),
#       .names = "{.col}_{.fn}")) |>
#   ungroup() |> 
#   filter(Sun_n > 2, Shade_n > 2)
# 
# head(arrange(filter(SM_HourlyAvgsWide, Sun_n>0), Sun_n), n=20)


ggplot(mutate(filter(SM_HourlyAvgsWide,
              #Campus == "CSUSM"
),
              Time = hms::as_hms(Time)),
aes(x = Time, y = Sun_mean, color = Campus, shape = Month)) +
  facet_wrap(vars(Site), drop = TRUE)+
  #facet_grid(Site ~ Campus) +
  geom_point()+
  #stat_smooth(method = "loess", span = 0.3, se = FALSE)+
  labs(title = "Sun SM",
       y = "SM",
       #x = "Time (24-Hr)"
       x = "Time (24 Hr)"
  )+
  scale_x_time(
    breaks = hms::as_hms(seq(0, 86400, by = 14400)),
    labels = function(x) format(x, "%H:%M")) +
  theme_classic(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 
# SM_HourlyAvgsWide <- SM_HourlyAvgsWide |> 
#   mutate(Time = as.numeric(hms(Time))) |> 
#   filter(!is.na(Shade_mean),
#          !is.na(Shade_mean)) 
# 
# lm_SM <- lmer(Shade_mean ~ Site*Month + Time + (1|Campus), filter(SM_HourlyAvgsWide, #Campus != "Cal Poly"
#  )) 
# 
# Anova(lm_SM) 
# pairs(emmeans(lm_SM, specs = ~ Site|Month))
# #emmeans(lm_SM, specs = ~ Site|Month)
# ranef(lm_SM)$Campus
# 
# qqnorm(resid(lm_SM))
# qqline(resid(lm_SM))
# hist(resid(lm_SM))
# performance::check_model(lm_SM)






SM_VPD <- full_join(SM_All, VPD_All,
                    by = c("DateTime", "Site", "Campus")) |> 
  pivot_longer(cols = c("SM", "VPD"),
            names_to = "Metric",
            values_to = "Reading")

ggplot(filter(SM_VPD, Campus == "UCLA"), aes(x = DateTime, y = Reading, color = Site)) +
  geom_point() +
  facet_wrap(vars(Metric), scales = "free_y") + 
  theme_minimal()





