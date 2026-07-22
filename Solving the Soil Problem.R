#HOBO soil moisture sensors (especially the S-SMC-M005 and related capacitance-based probes) are known to produce slightly negative volumetric water content estimates under very dry conditions, particularly when:

ggplot(#filter(
  LM_SM_HourlyAvgsLong,
              #Campus == "CLU"), 
       aes(x = Time, y = mean, color = Site, shape = Location)) +
  facet_wrap(~(Location:Campus)) +
  geom_point()

ggplot(#filter(
  LM_SM_HourlyAvgsLong,
  #Campus == "CLU"), 
  aes(x = Time, y = mean, color = Campus, shape = Location)) +
  facet_wrap(~(Location)) +
  geom_point()

ggplot(filter(LM_SM_HourlyAvgsLong, Campus == "UCLA"), 
       aes(x = Time, y = mean, color = Month)) +
  geom_jitter()+
  facet_wrap(~(Site:Location))

ggplot(filter(SM_All_PrecCalib, Campus == "Whittier"), 
       aes(x = DateTime, y = SM, color = Site)) +
  geom_point()+
  facet_wrap(~(Location))

ggplot(filter(SM_All, Campus == "CLU"), 
       aes(x = DateTime, y = SM, color = Site)) +
  geom_point()+
  facet_wrap(~(Location))




SM_All_PrecCalib |>
     group_by(Campus, Site) |>
     summarize(
           mean_SM = mean(SM, na.rm = TRUE),
            median_SM = median(SM, na.rm = TRUE),
           pct_neg = mean(SM < 0, na.rm = TRUE)*100
       )

SM_All_PrecCalib |>
     group_by(Campus) |>
     summarize(
           q05 = quantile(SM, .05, na.rm = TRUE),
           q25 = quantile(SM, .25, na.rm = TRUE),
           median = median(SM, na.rm = TRUE),
           q75 = quantile(SM, .75, na.rm = TRUE),
           q95 = quantile(SM, .95, na.rm = TRUE)
       )

