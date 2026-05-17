
Prelim_Stat_Longest_trial <- read_xlsx(
  path = "~/Downloads/All Data/Cal Poly/Cal Poly Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Cal Poly") %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CSUSM/CSUSM Master.xlsx","Temp") %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSUSM"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/Pepperdine/Pepperdine Master.xlsx","Temp") %>%
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
      path = "~/Downloads/All Data/Whittier/Whittier Master.xlsx","Temp") %>%
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
      path = "~/Downloads/All Data/CSUSB/CSUSB Master.xlsx","Temp")%>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSUSB"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CSULB/CSULB Master.xlsx","Temp") %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CSULB"), 
    by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(
    read_xlsx(
      path = "~/Downloads/All Data/CLU/CLU Master.xlsx","Temp") %>%
      select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
      pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
      separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
      mutate(Campus = "CLU"), by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  mutate(Location = recode(Location, "Amb" = "Ambient"))







CalPoly_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/Cal Poly/Cal Poly Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Cal Poly")
CSUSM_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/CSUSM/CSUSM Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "CSUSM")
Pepperdine_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/Pepperdine/Pepperdine Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Pepperdine")
Scripps_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/Scripps/Scripps Master.xlsx","Temp",
  col_types = c(
    "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Scripps")
Whittier_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/Whittier/Whittier Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "Whittier")
UCLA_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/UCLA/UCLA Master.xlsx","Temp",
  col_types = c(
    "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "UCLA")
CSUSB_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/CSUSB/CSUSB Master.xlsx","Temp")%>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "CSUSB")
CSULB_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/CSULB/CSULB Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "CSULB")
CLU_Temp_Longest <- read_xlsx(
  path = "~/Downloads/All Data/CLU/CLU Master.xlsx","Temp") %>%
  select(c("DateTime","On_Amb","On_Sun","On_Shade","Off_Amb","Off_Sun","Off_Shade")) %>% 
  pivot_longer(cols = -DateTime, names_to = "Site_Location", values_to = "Temperature") %>%
  separate(Site_Location, into = c("Site", "Location"), sep = "_") %>%
  mutate(Campus = "CLU")

Prelim_Stat_Longest <- CalPoly_Temp_Longest %>% 
  full_join(CSUSM_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(Pepperdine_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(Scripps_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(Whittier_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(UCLA_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(CSUSB_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(CSULB_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  full_join(CLU_Temp_Longest, by = c("DateTime", "Site", "Location", "Temperature", "Campus")) %>% 
  mutate(Location = recode(Location, "Amb" = "Ambient"))
