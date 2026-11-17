
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

ggplot(filter(LM_SM_HourlyAvgsLong, Campus == "Scripps"), 
       aes(x = Time, y = mean, color = Month)) +
  geom_jitter()+
  facet_wrap(~(Site:Location))

ggplot(filter(SM_All_PrecCalib, Campus == "Scripps"), 
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



ggplot(#filter(
  LM_VPD_HourlyAvgsLong,
  #Campus == "CLU"), 
  aes(x = Time, y = mean, color = Site)) +
  facet_wrap(~(Campus)) +
  geom_point()


ggplot(filter(
  LM_VPD_HourlyAvgsLong,
  Month == c("Apr", "May", "Jun")),
  #Campus == "CLU"), 
  aes(x = Time, y = mean, color = Month)) +
  facet_wrap(~(Site)) +
  geom_point()


library("glmmTMB")
library("car")
library("splines")

LM_SM_Sun_Zeros_df

Zeros_Sun <- glmmTMB(
  mean ~ Site * Month + poly(Time,2) + 
    (1|Campus) + (1|Site:Campus) +  (1|Month:Site:Campus),
  # ziformula = ~ Site * Month + poly(Time,2) + 
  #   (1|Campus) + (1|Site:Campus) +  (1|Month:Site:Campus),
  dispformula = ~ poly(Time,2),
  family = tweedie(link = "log"),
  control = glmmTMBControl(
    optCtrl = list(
      iter.max = 10000,
      eval.max = 10000
    )),
  data = LM_SM_Sun_Zeros_df
)

Anova(Zeros_Sun)

emm_site <- emmeans(Zeros_Sun, ~ Site, type = "response")
pairs(emm_site)


library("performance")
checklist <- c("normality","qq","linearity","homogeneity")
check_model(Zeros_Sun,check=checklist)
library("DHARMa")
Zeros_Sun_dharma <- simulateResiduals(fittedModel = Zeros_Sun,
  n = 2000, simulateREs = "conditional",
  refit = FALSE, plot = FALSE, seed = 2314)

plot(Zeros_Sun_dharma)

testUniformity(Zeros_Sun_dharma)
testQuantiles(Zeros_Sun_dharma)
testDispersion(Zeros_Sun_dharma)
testZeroInflation(Zeros_Sun_dharma)
testOutliers(Zeros_Sun_dharma)








##Diagnostic for Soil Moisture LMs

filtered_SM_sun <- filter(LM_SM_HourlyAvgsLong, Location == "Sun")
summary(filtered_SM_sun$mean)
#minimum is -0.03491

filtered_SM_shade <- filter(LM_SM_HourlyAvgsLong, Location == "Shade")
summary(filtered_SM_shade$mean)
#minimum is -0.053715

LM_SM_Shade_transformed_df_1 <- LM_SM_HourlyAvgsLong |>
  filter(Location == "Shade", Campus != "UCLA") |> 
  mutate(mean_transformed = mean + 1)

LM_SM_Shade_transformed_df_0.1 <- LM_SM_HourlyAvgsLong |>
  filter(Location == "Shade", Campus != "UCLA") |> 
  mutate(mean_transformed = mean + .1)

LM_SM_Shade_transformed_df_0.06 <- LM_SM_HourlyAvgsLong |>
  filter(Location == "Shade", Campus != "UCLA") |> 
  mutate(mean_transformed = mean + .06)

lm_SM_Sun_1 <- lmer(log(mean_transformed) ~ Site * Month + poly(Time,2) + 
                    (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus), 
                  REML = FALSE, control = lmerControl(optimizer = "bobyqa"),
                  data = LM_SM_Shade_transformed_df_1)
lm_SM_Sun_0.1 <- lmer(log(mean_transformed) ~ Site * Month + poly(Time,2) + 
                      (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus), 
                    REML = FALSE, control = lmerControl(optimizer = "bobyqa"),
                    data = LM_SM_Shade_transformed_df_0.1)
lm_SM_Sun_0.06 <- lmer(log(mean_transformed) ~ Site * Month + poly(Time,2) + 
                        (1|Campus) + (1|Site:Campus) + (1|Month:Site:Campus), 
                      REML = FALSE, control = lmerControl(optimizer = "bobyqa"),
                      data = LM_SM_Shade_transformed_df_0.06)

plot(lm_SM_Sun_1) #performs marginally better than the rest
plot(lm_SM_Sun_0.1) #performs marginally worse, with a slight fanning at the 0s
plot(lm_SM_Sun_0.06) #performs the worst

qqnorm(resid(lm_SM_Sun_1)) #performs marginally better than the rest
qqnorm(resid(lm_SM_Sun_0.1)) #performs marginally worse, with more curvature
qqnorm(resid(lm_SM_Sun_0.06)) #performs the worst

library(performance)
library(see)
checklist <- c("normality","qq","linearity","homogeneity")
check_model(lm_SM_Sun_1,check=checklist)
check_model(lm_SM_Sun_0.1,check=checklist)
check_model(lm_SM_Sun_0.06,check=checklist)
