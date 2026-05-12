library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(spdep)
library(gt)
library(gtExtras)
library(spaMM)
library(RSpectra)
library(broom.mixed)
library(lme4)

data_2014 <- read_csv('Crash Condensed CSVs/2014_Crash_Condensed.csv')
data_2015 <- read_csv('Crash Condensed CSVs/2015_Crash_Condensed.csv')
data_2016 <- read_csv('Crash Condensed CSVs/2016_Crash_Condensed.csv')
data_2017 <- read_csv('Crash Condensed CSVs/2017_Crash_Condensed.csv')
data_2018 <- read_csv('Crash Condensed CSVs/2018_Crash_Condensed.csv')
data_2019 <- read_csv('Crash Condensed CSVs/2019_Crash_Condensed.csv')
data_2020 <- read_csv('Crash Condensed CSVs/2020_Crash_Condensed.csv')
data_2021 <- read_csv('Crash Condensed CSVs/2021_Crash_Condensed.csv')
data_2022 <- read_csv('Crash Condensed CSVs/2022_Crash_Condensed.csv')
data_2023 <- read_csv('Crash Condensed CSVs/2023_Crash_Condensed.csv')
data_2024 <- read_csv('Crash Condensed CSVs/2024_Crash_Condensed.csv')

data_all <- bind_rows(data_2014, data_2015, data_2016, data_2017, data_2018,
                      data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
data_all$id <- seq(1, nrow(data_all), by = 1)

# DRCOG Location and associated lat/long coordinates
location_coords <- data_all %>%
  filter(!is.na(latitude) & !is.na(longitude) & latitude != 0 & longitude != 0) %>%
  group_by(drcoglocat) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude)
  )

data_all <- data_all %>%
  left_join(location_coords, by = "drcoglocat", suffix = c("", "_imputed")) %>%
  mutate(
    latitude = ifelse(latitude == 0 | is.na(latitude), latitude_imputed, latitude),
    longitude = ifelse(longitude == 0 | is.na(longitude), longitude_imputed, longitude)
  ) %>% dplyr::select(-latitude_imputed, -longitude_imputed)

data_all_clean <- data_all %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter(latitude >= 39.5 & latitude <= 40.2) %>%
  filter(longitude >= -105.5 & longitude <= -104.5)

data_all_clean <- data_all_clean %>%
  mutate(across(-c(id, drcog_id, latitude, longitude, crashtime, time, drcoglocat, splimit, numbkilled, numbinjurd), as.factor))

summary(data_all_clean$latitude)
summary(data_all_clean$longitude)

# Transforming crash data to spatial entries (point geometries)
data_spat <- st_as_sf(data_all_clean, coords = c('longitude', 'latitude'), crs = 4326)
data_spat <- st_transform(data_spat, 2877)

# Loading in block group data
demo <- st_read('ODC_POP_AMERCMTYSVY20172021BLKGP_A_8640147160846075272/')
demo$area <- st_area(demo)

# Loading in street typology data
streets <- st_read('complete_streets/')
streets <- st_transform(streets, 2877)

# Ensuring same CRS
st_crs(data_spat)
st_crs(demo)
st_crs(streets)

data_spat <- data_spat[demo, ]
comb_data <- st_join(data_spat, demo, join = st_within)

### Exploratory Analysis
total_crashes <- nrow(comb_data)
total_injurd <- sum(comb_data$numbinjurd)
total_killed <- sum(comb_data$numbkilled)

total_bike <- sum(comb_data$bikeinv == 1)
total_ped <- sum(comb_data$pedinv == 1)
total_moto <- sum(comb_data$motoinv == 1)
total_sctr <- sum(comb_data$sctrinv == 1)

# Omitting 2 rows with NA severity level
svrty_data <- comb_data %>%
  filter(!is.na(svrty))
svrty_spread <- data.frame(Severity = c(0, 1, 2, 3, 4), 
                           Description = c('No Injuries', 'Possible Injuries', 'Minor Injuries', 'Severe Injuries', 'Fatal Injuries'),
                           Count = c(sum(svrty_data$svrty == 0),
                                     sum(svrty_data$svrty == 1),
                                     sum(svrty_data$svrty == 2),
                                     sum(svrty_data$svrty == 3),
                                     sum(svrty_data$svrty == 4)), 
                           Proportion = c(sum(svrty_data$svrty == 0)/nrow(svrty_data),
                                          sum(svrty_data$svrty == 1)/nrow(svrty_data),
                                          sum(svrty_data$svrty == 2)/nrow(svrty_data),
                                          sum(svrty_data$svrty == 3)/nrow(svrty_data),
                                          sum(svrty_data$svrty == 4)/nrow(svrty_data))) 

svrty_spread %>% gt()  %>% fmt_number(columns = Proportion, decimals = 3) %>% gt_theme_pff()

crash_mode <- data.frame(Mode = c('Motor Vehicle', 'Pedestrian', 'Motorcycle', 'Bicycle', 'Scooter'), 
                         Count = c(sum(comb_data$sctrinv == 0 &comb_data$motoinv == 0 & comb_data$pedinv == 0 & comb_data$bikeinv == 0), 
                                   sum(comb_data$pedinv == 1 & comb_data$sctrinv != 1 & comb_data$bikeinv != 1 & comb_data$motoinv != 1), 
                                   sum(comb_data$motoinv == 1 & comb_data$pedinv != 1),
                                   sum(comb_data$bikeinv == 1 & comb_data$pedinv != 1),
                                   sum(comb_data$sctrinv == 1 & comb_data$pedinv != 1)), 
                         Proportion = c(sum(comb_data$sctrinv == 0 &comb_data$motoinv == 0 & comb_data$pedinv == 0 & comb_data$bikeinv == 0)/ nrow(comb_data),
                                        sum(comb_data$pedinv == 1 & comb_data$sctrinv != 1 & comb_data$bikeinv != 1 & comb_data$motoinv != 1) / nrow(comb_data),
                                        sum(comb_data$motoinv == 1 & comb_data$pedinv != 1) / nrow(comb_data),
                                        sum(comb_data$bikeinv == 1 & comb_data$pedinv != 1) / nrow(comb_data),
                                        sum(comb_data$sctrinv == 1 & comb_data$pedinv != 1) / nrow(comb_data)))

crash_mode %>% gt() %>% fmt_number(columns = Proportion, decimal = 3) %>% tab_header(title = 'Crash Count by Mode') %>% gt_theme_pff()

comb_data %>% group_by(rdconditin) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))

comb_data %>% group_by(crashtype) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))


comb_data %>% group_by(lightngcon) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))


comb_data %>% group_by(weatherco) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))


comb_data %>% group_by(roaddescrp) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))


comb_data %>% group_by(hcf) %>%
  summarise(Count = n(),
            Killed = sum(numbkilled),
            Injured = sum(numbinjurd))                                   

# Block Group Data
crash_counts <- comb_data %>%
  st_drop_geometry() %>%
  group_by(STFID) %>%
  dplyr::summarise(crash_count = n(),
                   ped_crashes = sum(pedinv == 1, na.rm = T),
                   bike_crashes = sum(bikeinv == 1, na.rm = T),
                   moto_crashes = sum(motoinv ==1 , na.rm = T),
                   severe_crash = sum(svrty >= 3, na.rm = T))

block_group_datafull <- demo %>%
  left_join(crash_counts, by = 'STFID')

block_group_data <- block_group_datafull %>%
  dplyr::select(STFID, contains("PCT"), MED_HH_INC, TTL_POPULA, crash_count, ped_crashes, bike_crashes, moto_crashes, severe_crash, area, BACHELORS_, COMMUTE_LE)
block_group_data <- block_group_data %>%
  mutate(PCT_minority = PCT_HISPAN + PCT_BLACK + PCT_ASIAN + PCT_NATIVE + PCT_HAWAII +PCT_OTHERR + PCT_TWOORM,
         PCT_bach = BACHELORS_/TTL_POPULA) %>% dplyr::select(c(-PCT_NATIVE, -PCT_HAWAII, -PCT_OTHERR, -PCT_TWOORM))
block_group_data <- block_group_data %>%
  mutate(area_sqmi = as.numeric(area) / 27878400,
         POP_DENS = TTL_POPULA / area_sqmi,
         pop_dens_f= TTL_POPULA/area,
         prop_minority = PCT_minority/100,
         prop_white = PCT_WHITE / 100)
block_group_data$ID <- 1:nrow(block_group_data)    
block_group_data$pop_zero <- ifelse(block_group_data$TTL_POPULA == 0, 1, 0) # Zero-population block indicator

# Establishing neighborhoods
nb <- poly2nb(block_group_data, queen = T)
lw <- nb2listw(nb, style = "W", zero.policy = T)

# Imputing 37 blocks with missing median household income based on its neighboring blocks
block_group_data$MED_HH_INC_imp <- block_group_data$MED_HH_INC
residential <- block_group_data$TTL_POPULA > 0 
block_group_data$MED_HH_INC_imp[block_group_data$MED_HH_INC_imp ==0 & residential] <- lag.listw(lw, block_group_data$MED_HH_INC, NAOK = T)[block_group_data$MED_HH_INC_imp ==0 & residential]
sum(block_group_data$MED_HH_INC_imp == 0)

summary(block_group_data$crash_count)  
sd(block_group_data$crash_count)

## Adding total road length variable (major roadways)
# Intersect streets with block groups
streets_bg <- st_intersection(streets, dplyr::select(demo, STFID))

# Sum road length per block group
road_lengths <- streets_bg %>%
  st_drop_geometry() %>%
  group_by(STFID) %>%
  summarise(total_road_length = sum(Shape_STLe, na.rm = T))

# Join to block group data
block_group_data <- block_group_data %>%
  left_join(road_lengths, by = "STFID")

block_group_data <- block_group_data %>%
  mutate(total_road_length = ifelse(is.na(total_road_length), 0, total_road_length))

block_group_data <- block_group_data %>%
  dplyr::select(c(STFID, MED_HH_INC, crash_count, ped_crashes, bike_crashes, moto_crashes, severe_crash, POP_DENS, area_sqmi,MED_HH_INC_imp, total_road_length, geometry, prop_white, pop_zero, ID, prop_minority))

# Testing for autocorrelation in crash count
moran.test(block_group_data$crash_count, lw) # Positive Autocorrelation
moran.plot(block_group_data$crash_count, listw = lw)

# Spatial NB GLMM 
W <- nb2mat(nb, style = 'B', zero.policy = T)
W <- matrix(as.numeric(W), nrow = nrow(W), ncol = ncol(W))

model_spatial_nb <- fitme(crash_count ~ MED_HH_INC_imp + 
                            prop_minority +
                            POP_DENS + pop_zero + 
                            total_road_length +
                            adjacency(1 | ID), adjMatrix = W,
                          data = block_group_data, 
                          family = negbin())

summary.HLfit(model_spatial_nb, details = list(p_value = 'Wald'))
exp(fixef(model_spatial_nb))

spat_nb_res <- data.frame(Variable = c('Median Household Income', 
                                       'Percent Minoritized Population',
                                       'Population Density',
                                       'Zero Population',
                                       'Total Road Length'),
                          Effect_per_Unit = c('Per $10,000 Increase',
                                              'Per 1% Increase',
                                              'Per unit increase',
                                              'Blocks with zero population',
                                              'Per foot increase'),
                          Incident_Rate_Ratio = c('0.981 (~2% fewer crashes)',
                                                  '1.39 (39% more crashes)',
                                                  '0.999 (slightly fewer crashes)',
                                                  '0.77 (23% fewer crashes)',
                                                  '1.000009 (slightly more crashes)'),
                          p_val = c(0.045, 0.076, '<0.001', 0.37, '<0.001'))
spat_nb_res %>% gt() %>% gt_theme_pff() %>% cols_label(Variable = 'Predictor', 
                                                       Effect_per_Unit=    'Effect per Unit',
                                                       Incident_Rate_Ratio =   'Incident Rate Ratio',
                                                       p_val = 'P value')
                                                       

# Spatial NB GLMM for Pedestrian Crash Count
moran.test(block_group_data$ped_crashes, lw) # Positive Autocorrelation

model_spatial_nb_ped <- fitme(ped_crashes ~ MED_HH_INC_imp + 
                                prop_minority +
                                POP_DENS + pop_zero + 
                                total_road_length +
                                adjacency(1 | ID), adjMatrix = W,
                              data = block_group_data, 
                              family = negbin())

summary.HLfit(model_spatial_nb_ped, details = list(p_value = 'Wald'))
exp(fixef(model_spatial_nb_ped))

spat_nb_res2 <- data.frame(Variable = c('Median Household Income', 
                                       'Percent Minoritized Population',
                                       'Population Density',
                                       'Zero Population',
                                       'Total Road Length'),
                          Effect_per_Unit = c('Per $10,000 Increase',
                                              'Per 1% Increase',
                                              'Per unit increase',
                                              'Blocks with zero population',
                                              'Per foot increase'),
                          Incident_Rate_Ratio = c('0.952 (~5% fewer crashes)',
                                                  '1.62 (62% more crashes)',
                                                  '0.999 (slightly fewer crashes)',
                                                  '0.396 (~60% fewer crashes)',
                                                  '1.00001 (slightly more crashes)'),
                          p_val = c(0.001, 0.053, 0.513, 0.02, '<0.001'))
spat_nb_res2 %>% gt() %>% gt_theme_pff() %>% cols_label(Variable = 'Predictor', 
                                                       Effect_per_Unit=    'Effect per Unit',
                                                       Incident_Rate_Ratio =   'Incident Rate Ratio',
                                                       p_val = 'P value')

# Model 3: Mixed-effects Logistic Regression
env_data <- comb_data %>%
  dplyr::select(c(id, crashtype, numbkilled, numbinjurd, drcoglocat, rdconditin, lightngcon, weatherco, roaddescrp, splimit, svrty, hcf, geometry, rdsystem)) %>%
  filter(!is.na(svrty)) 
env_data$KSI <- ifelse(env_data$svrty == 4 | env_data$svrty == 3, 1, 0)
sum(env_data$KSI)
env_df <- st_join(env_data, block_group_data['STFID'])
env_df$weatherco <- case_when(
  env_df$weatherco %in% c(0) ~ 'Clear/Cloudy',
  env_df$weatherco %in% c(1, 3) ~ 'Rain',
  env_df$weatherco %in% c(2) ~ 'Snow',
  TRUE ~ 'Other/Unknown'
)
env_df$weatherco <- as.factor(env_df$weatherco)
levels(env_df$weatherco)

env_df$rdconditin <- case_when(
  env_df$rdconditin %in% c(1, 2) ~ 'Dry',
  env_df$rdconditin %in% c(4, 5, 7, 8,9, 10) ~ 'Snow/Ice',
  env_df$rdconditin %in% c(11, 12) ~ 'Wet',
  TRUE ~ 'Other/Unknown'
)
env_df$rdconditin <- as.factor(env_df$rdconditin)
levels(env_df$rdconditin)

env_df$rdsystem <- case_when(
  env_df$rdsystem %in% c(1) ~ 'Interstate',
  env_df$rdsystem %in% c(2) ~ 'State Highway',
  env_df$rdsystem %in% c(3) ~ 'Private property',
  env_df$rdsystem %in% c(4) ~ 'Other'
)
env_df$rdsystem <- as.factor(env_df$rdsystem)
levels(env_df$rdsystem)

env_df$lightngcon <- case_when(
  env_df$lightngcon %in% c(1) ~ 'Daylight',
  env_df$lightngcon %in% c(2) ~ 'Dawn or dusk',
  env_df$lightngcon %in% c(3) ~ 'Dark-lighted',
  env_df$lightngcon %in% c(4) ~ 'Dark-unlighted',
  TRUE ~ 'Other'
)
env_df$lightngcon <- as.factor(env_df$lightngcon)
levels(env_df$lightngcon)
env_df$lightngcon <- relevel(env_df$lightngcon, ref = "Daylight")
env_df <- env_df[demo, ]

model_ksi <- glmer(KSI ~ rdsystem + lightngcon + weatherco + rdconditin + (1 | STFID), data = env_df, family = binomial(link = 'logit'))
results <- tidy(model_ksi, effects = 'fixed')
results$OR <- exp(results$estimate)
results
summary(model_ksi)

## Visuals for Model 3 

results_ksi <- tidy(model_ksi, effects = "fixed") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    pct_change = (OR - 1) * 100,
    lower = exp(estimate - 1.96 * std.error),
    upper = exp(estimate + 1.96 * std.error),
    clean_term = case_when(
      term == "rdsystemOther" ~ "Road System: Other",
      term == "rdsystemPrivate property" ~ "Road System: Private Property",
      term == "rdsystemState Highway" ~ "Road System: State Highway",
      
      term == "lightngconDark-unlighted" ~ "Lighting: Dark (Unlighted)",
      term == "lightngconDawn or dusk" ~ "Lighting: Dawn/Dusk",
      term == "lightngconDaylight" ~ "Lighting: Daylight",
      term == "lightngconOther" ~ "Lighting: Other",
      term == 'lightngconDark-lighted' ~ 'Lighting: Dark (Lighted)',
      
      term == "weathercoOther/Unknown" ~ "Weather: Other/Unknown",
      term == "weathercoRain" ~ "Weather: Rain",
      term == "weathercoSnow" ~ "Weather: Snow",
      
      term == "rdconditinOther/Unknown" ~ "Road Condition: Other/Unknown",
      term == "rdconditinSnow/Ice" ~ "Road Condition: Snow/Ice",
      term == "rdconditinWet" ~ "Road Condition: Wet",
      TRUE ~ term
    )
  )

results_ksi <- results_ksi %>%
  mutate(
    clean_term = factor(clean_term, levels = rev(c(
      "Road System: Other",
      "Road System: Private Property",
      "Road System: State Highway",
      
      "Lighting: Dark (Unlighted)",
      "Lighting: Dawn/Dusk",
      "Lighting: Daylight",
      "Lighting: Other",
      "Lighting: Dark (Lighted)",
      
      "Weather: Other/Unknown",
      "Weather: Rain",
      "Weather: Snow",
      
      "Road Condition: Other/Unknown",
      "Road Condition: Snow/Ice",
      "Road Condition: Wet"
    )))
  )

results_ksi$group <- sub(":.*", "", results_ksi$clean_term)

odds_plot <- ggplot(results_ksi, aes(x = OR, y = clean_term, color = group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Odds Ratios for ME Logistic Regression Model",
    x = "Odds Ratio (log scale)",
    y = "",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "none"
  )
odds_plot
ref_table <- tibble::tibble(
  Group = c("Road System", "Lighting", "Weather", "Road Condition"),
  Reference = c("Interstate", "Daylight", "Clear", "Dry"),
  Color = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
)
ref_table %>%
  gt() %>%
  tab_header(
    title = "Reference Categories"
  ) %>%
  data_color(
    columns = Group,
    colors = scales::col_factor(
      palette = ref_table$Color,
      domain = ref_table$Group
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Group)
  ) %>%
  cols_hide(columns = Color)


# For GIS analysis
lisa <- localmoran(block_group_data$crash_count, lw)
block_group_data$local_I <- lisa[,1]
block_group_data$p_value <- lisa[,5]
st_write(block_group_data, "blockgroups.gpkg")

severe <- env_df %>% filter(KSI == 1)
coords <- st_coordinates(env_df)
knn <- knearneigh(coords, k = 8)
nb_knn <- knn2nb(knn)
lw_knn <- nb2listw(nb_knn, style = "W")
clus <- localmoran(env_df$KSI, lw_knn)
env_df$local_I <- clus[,1]
env_df$p_value <- clus[,5]
st_write(env_df, "env_df.gpkg")

# Additional Visuals/Tables

crash_countsum <- data.frame(Statistic = c('Minimum', 'Maximum', 'Median', 'Mean', 'Std. Dev.', 'Total'), Value = c(min(block_group_data$crash_count), max(block_group_data$crash_count), median(block_group_data$crash_count), mean(block_group_data$crash_count), sd(block_group_data$crash_count), sum(block_group_data$crash_count)))
crash_countsum %>% gt() %>% gt_theme_pff() %>% fmt_number(columns = everything(), decimals = 1)

pedcrash_countsum <- data.frame(Statistic = c('Minimum', 'Maximum', 'Median', 'Mean', 'Std. Dev.', 'Total'), Value = c(min(block_group_data$ped_crashes), max(block_group_data$ped_crashes), median(block_group_data$ped_crashes), mean(block_group_data$ped_crashes), sd(block_group_data$ped_crashes), sum(block_group_data$ped_crashes)))
pedcrash_countsum %>% gt() %>% gt_theme_pff() %>% fmt_number(columns = everything(), decimals = 1)

top_intersections <- data_all %>%
  filter(!is.na(drcoglocat), drcoglocat != "") %>%
  count(drcoglocat, sort = TRUE)

topKSI_intersections <- env_df %>%
  filter(!is.na(drcoglocat), drcoglocat != "", KSI == 1) %>%
  count(drcoglocat, sort = TRUE)

   
