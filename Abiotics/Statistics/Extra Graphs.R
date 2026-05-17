library(dplyr)
library(lme4)
library(ggplot2)


dat <- Prelim_Stat_HourlyAvgsLong %>%
  filter(Location == "Ambient") %>%
  mutate(
    Site   = factor(Site),
    Month  = factor(Month),
    Campus = factor(Campus),
    Time   = factor(as.character(Time)),
    Time_hr = time_length(Time, "hours"))

HourlyAvgs_lmer2 <- lmer(
  mean ~ Site * Month * Time + (1 | Campus),
  data = dat)

emm_time <- emmeans(
  HourlyAvgs_lmer2,
  ~ Site | Month,
  by = "Time")

pred_df <- as.data.frame(emm_time) %>%
  mutate(Time_hr = time_length(Time, "hours"))

dat_avg <- dat %>%
  group_by(Site, Month, Time_hr) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE),
    .groups = "drop")

ggplot() +
  # Grey campus-level observations
  geom_line(
    data = dat,
    aes(x = Time_hr, y = mean, group = interaction(Campus, Site)),
    color = "grey70", alpha = 0.5
  ) +
  geom_point(
    data = dat,
    aes(x = Time_hr, y = mean, group = interaction(Campus, Site)),
    color = "grey70", alpha = 0.5
  ) +
  
  # Campus-averaged colored lines
  geom_line(
    data = dat_avg,
    aes(x = Time_hr, y = mean_val, color = Site, group = Site),
    linewidth = 1.4
  ) +
  
  facet_grid(cols = vars(Month)) +
  scale_x_continuous(name = "Time of Day", breaks = seq(0, 24, 4)) +
  labs(y = "Temperature (°C)", color = "Site") +
  theme_bw()
