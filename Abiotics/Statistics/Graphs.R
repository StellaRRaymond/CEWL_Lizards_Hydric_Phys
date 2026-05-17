ggplot(
  filter(pivot_longer(Prelim_Stat_All, 
                      cols=c(Ambient, Sun, Shade), 
                      names_to = "Location", 
                      values_to = "Temperature"),
         Location == "Ambient", 
         Time == hms("12:00:00"), 
         !Campus == "CSUSM", 
         Campus == "Pepperdine"), 
      aes(x =MonthDayYear, y = Temperature, color = Campus, shape = Site)) +
  #geom_smooth(method = "loess", span = 0.8, se = F) +
  geom_point()+
  labs(title = "Ambient Temperatures at Noon")
  #facet_wrap(vars(Site))



df <- filter(Prelim_Stat_HourlyAvgs_lmer, Location == "Ambient")

df$resid <- residuals(HourlyAvgs_lmer)
df$fitted <- fitted(HourlyAvgs_lmer)

extreme_res <- df %>% 
  mutate(abs_resid = abs(resid)) %>%
  arrange(desc(abs_resid)) %>%
  slice(1:20)     # inspect the 20 worst offenders

extreme_res


VPD_Temp_HourlyAvgsWide$row_id <- 1:nrow(VPD_Temp_HourlyAvgsWide)
model_data <- VPD_Temp_HourlyAvgsWide %>% 
  filter(VPD_n > 2, 
         !Campus == "CSUSM")
model <- lmer(
  VPD_mean ~  Site*Month  + (1|Campus) + (1|Temp_mean) + (1|Time)
  ,data = filter(model_data, !Campus == "CSUSM"),
  REML = F)
model_data$resid <- resid(model, type = "pearson")
VPD_Temp_HourlyAvgsWide <- 
  left_join(VPD_Temp_HourlyAvgsWide, 
            model_data %>% select(row_id, resid),
            by = "row_id")

VPD_Temp_HourlyAvgsWide %>%
  arrange(desc(abs(resid))) %>%
  head(20)



VPD_resids <- VPD_Temp_HourlyAvgsWide %>%
  filter(!Campus == "CSUSM") %>%
  mutate(residual = residuals(VPD_HourlyAvgs_Templmer))

ggplot(VPD_resids, aes(x = Time, y = VPD_mean, color = residual)) +
  facet_wrap(vars(Campus))+
  geom_point(aes(color = residual > 0.5)) +
  scale_color_manual(values = c("black", "red"),
                     labels = c("≤ 0.5", "> 0.5"),
                     name = "Residual Size") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #labs(
    #title = "Residual Plot with Points > 0.5 Highlighted",
    #x = "Predicted VPD (mean)",
    #y = "Residuals") +
  theme_bw()



VPD_Raw_resids <- VPD_All %>%
  filter(!Campus == "CSUSM") %>%
  mutate(residual = residuals(VPD_Raw_lmer))

ggplot(VPD_Raw_resids, aes(x = Time, y = VPD, color = residual)) +
  facet_wrap(vars(Campus))+
  geom_point(aes(color = residual > 0.5)) +
  scale_color_manual(values = c("black", "red"),
                     labels = c("≤ 0.5", "> 0.5"),
                     name = "Residual Size") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # labs(
  #   title = "Residual Plot with Points > 0.5 Highlighted",
  #   x = "Predicted VPD (mean)",
  #   y = "Residuals") +
  theme_bw()



#average for all campuses
Graphing_HourlyAvgsLong <- Prelim_Stat_HourlyAvgsLong %>% 
  mutate(Month = match(as.character(Month), month.abb))

ggplot(filter(Graphing_HourlyAvgsLong, Location == "Sun"), aes(x = Month, y = cv, color = Site)) + geom_smooth(method = "loess", span = 0.75, se = F)


ggplot(Prelim_Stat_All, aes(x = DateTime, y = Sun, color = Site)) + geom_smooth(k = 4, se = F)










Feb_Dates <- Prelim_Stat_All %>% 
  filter(Month == "Mar")

Feb_Dates %>%
  group_by(Campus) %>%
  summarize(n_distinct(MonthDayYear))







