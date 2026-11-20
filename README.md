This repository includes the data used in:
# Cool Lizards in Urban Oases: Hydric conditions and lizard physiology differ between urban and non-urban habitats. 

# Abstract:
Urban heat islands occur when temperatures in urban areas exceed those in non-urban areas. However, anthropogenic water inputs may alter thermal conditions in urban environments, especially during dry seasons in xeric regions. We lack a general understanding of how altered microclimates affect ecophysiological traits of urban wildlife. Here, we examined whether persistent and seasonal differences in temperature and water availability across 11 paired urban and non-urban sites were associated with hydric physiology in a widespread ectotherm, the Western Fence Lizard (Sceloporus occidentalis). Urban sites had substantially higher soil moisture and lower operative lizard temperatures during the hottest months of the year. Across seasons, urban lizards had lower plasma osmolality, consistent with greater hydration, than non-urban lizards. Cutaneous evaporative water loss did not differ between site types in spring but was lower in non-urban lizards in fall, indicating that physiological differences between site types were season-dependent. Overall, our findings are consistent with an urban Oasis Effect in which irrigated urban habitats are cooler and wetter than adjacent non-urban habitats during the dry season. These findings highlight the importance of considering water availability alongside temperature when evaluating the physiological consequences of urbanization for wildlife in seasonally dry environments at the landscape scale.

# Files included in this directory

## Abiotics (Folder)
Includes the folders of each campus
### Within "Abiotics"
CLU (Folder), CSULB (Folder), CSUSB (Folder), CSUSM (Folder), Cal Poly (Folder), Harvey Mudd (Folder), Pepperdine (Folder), SDSU (Folder), Scripps (Folder), UCLA (Folder), Whittier (Folder)
Each folder includes the master .xlsx sheet (used in the markdown files) and the temperature (temp), soil moisture (SM), and relative humidity (RH) .csv files for the respective campus. Each .xlsx master file includes 3 sheets: Temp, SM, and RH. All sheet contains the DateTime each data point was recorded (30 minute increments). Column B:G are the raw data for urban (on-campus; "On") and non-urban (off-campus; "Off") sun OTM, shade OTM, and ambient temperatures. Columns I:N are the calibration-adjusted values. the SM file contains the urban (on) and non-urban (off) soil moisture values in the sun and shade. RH files contain the relative humidity values on and off campus which were later used in R to calculate vapour pressure deficit (VPD). 

## Hydric Physiology Data (Folder)
Includes the hydric physiology data files
### Within "Hydric Physiology Data"
CEWL_Values.xlsx & CEWL_Values.csv: This file contains the compiled data output from the AquaFlux evaporimeter. 
Cool Lizards SVL.xlsx & Cool Lizards SVL.csv: This file contains the snout-vent length (SVL) for each lizard
Hydric_Physiology.xlsx & Hydric_Physiology.csv: This file contains the majority of the data collected from the lizards including when and where they were captured, when they were processed, lizard sex, gravidity status, mass (g), ectoparasite presence & count, hematocrit (%), average Posm reading, and lizard ID.

## Markdown Files
### Hydric Physiology (.Rmd)
Includes the data compilation, analysis and visualization for all three hydric physiology models (plasma osmolality, CEWL, and hematocrit)
### Map (.Rmd)
The creation of the map of Southern California labeling the location of each institution involved in the study
### Soil Moisture (.Rmd)
The data compilation, analysis and visualization for the two zero-inflated soil moisture models and the two VPD models. 
### Temperature (.Rmd)
The data compilation, analysis and visualization for the models exploring the mean and cv sun OTM, shade OTM, and ambient temperature trends.

## Other
### Hydric Physiology, Map, Soil Moisture, and Temperature (.pdf)
The knitted files from each respective .Rmd
### Temp All (.xlsx)
The cleaned & compiled datasheet generated in the Temperature.Rmd file, saved to be used in the Soil Moisture.Rmd for VPD calculations
