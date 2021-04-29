#> libs
library(tidyverse)
library(sf)
library(here)


#### 1. DATA IN AND PREP ####

#> Get data
#> RPA1
RPA1 <- read_csv(here("In", "2018_All_CAP_RPA.csv"))
#> RPA2
RPA2 <- read_csv(here("In", "2018_All_CAP_RPA2.csv"))
#> Bind tables together
#> Bind the two RPA tables togather (they are split alphabetically by beneficiary name)
RPA_All <- rbind(RPA1, RPA2)
#> Select only relevant records
RPA <- RPA_All %>% 
  select("BeneficiaryCode", "PostcodePrefix_F202B", "TownCity_F202C", "Basic payment scheme", "Greening: practices beneficial for climate and environment", "Agri-environment-climate", "Forest environmental and climate services and forest conservation", "Forest-environment payments", "Investment in forest area development and improvement of the viability of forests", "Investments in physical assets", "Organic farming", "Support for Leader local development")
print(RPA)


# #> Code to get list of RPA records with GL postcode (not needed because intersect reveals other non GL postcodes intersect Glos - e.g. "CV")
# #> Get list of GL postcodes for extraction
# PCodes_Glos <- read_csv(here("In", "Pcode_Districts_Glos.csv"))
# #> Merge RPAs with Glos postcode districts
# RPA1_GL_ALL <- merge(RPA1, PCodes_Glos, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
# RPA2_GL_ALL <- merge(RPA2, PCodes_Glos, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)



#### 2. AREA CALCULATIONS (TOTAL LAND AREA) ####
#> Do area calcs on postcode clip and difference layers to get proportion of postcodes in and out of Glos boundary AND district boundaries
##> CALCULATE AREAS (TOTA: AREA)

#> 1. Total PC AREA
#> Area of pcode district polys that intersect Glos (area pre-calculated in Q)
PC_Glos_Int <- st_read(here("In", "Pcode_Polys_Glos_Int.shp"))
#> Remove geom
st_geometry(PC_Glos_Int) <- NULL
# Rename area column 
PC_Glos_Int <- rename(PC_Glos_Int, PC_Area_Total = AreaKm2)

#> Get a list of unique poscode districts that intersect Glos from above layer
pc.int.glos <- as.data.frame(unique(PC_Glos_Int$name))


#> 2. PC Clip Area (Area of intersecting pcode district within Glos)
PC_Glos_Clip <- st_read(here("In", "Pcode_Polys_Glos_CLIP.shp"))
#> Remove geom
st_geometry(PC_Glos_Clip) <- NULL
# Rename area column 
PC_Glos_Clip <- rename(PC_Glos_Clip, PC_Area_Clip = AreaKm2)

#> 3. PC Diff Area (Area of intersecting pcode district outside Glos)
PC_Glos_Diff <- st_read(here("In", "Pcode_Polys_Glos_Diff.shp"))
#> Remove geom
st_geometry(PC_Glos_Diff) <- NULL
# Rename area column 
PC_Glos_Diff <- rename(PC_Glos_Diff, PC_Area_Diff = AreaKm2)



# Extract relevant intersecting (with Glos county boundary) postcodes from RPA datasets
RPA_GL_Int <- merge(RPA, PC_Glos_Int, by.x = "PostcodePrefix_F202B", "name", all.x = FALSE)
#> Export 
write_csv(RPA_GL_Int, (here("Out", "RPA_Glos_Pcodes.csv")))


#> Merge the PC Ddistrict area values (i.e. total, clip, diff) together
m1 <- merge(PC_Glos_Int, PC_Glos_Clip, by = "name")
m2 <- merge(m1, PC_Glos_Diff, by = "name")
#> Export as table showing proportion (area) of postcode districts within and outside of Glos county boundary
write_csv(m2, here("Out", "Area_Stats_Glos_County.csv"))

# Repeat worksflow to get figures for postcode districts at district council level
# Set up a list of district names (as they appear in files) as input for loop


chelt <- st_read(here("In", "Districts", "Clip_PC_Chelt.shp"))
m <- merge


dists <- c("Chelt", "Cotswold", "FoD", "Glos", "Stroud", "Tewkes")
#> Initiate loop
for (i in 1:length(dists))
{
x <- st_read(here("In", "Districts", paste0("Clip_PC_", dists[i], ".shp")))
m <- merge(x, PC_Glos_Int, by= "name", all.x = TRUE)
st_geometry(m) <- NULL
m <- m %>% 
  select(name, AreaKm2, PC_Area_Total)
write_csv(m, here("Out", "Districts", "All_Land", paste0("Area_Stats_", dists[i], ".csv")))
}


#### 3. BPS CALCULATIONS (based on agri land area) ####

#> 2.1 GLOS - ALL

#> Add data from within Glos
agri.pc.glos <- st_read(here("In", "Glos_Agri", "corine_agri_pcodes_Glos.shp"))
#> Select only relevant columns
agri.pc.glos2 <- agri.pc.glos %>% 
  select(name, AreaKm2, Area_Agri)
#> Remove geom
st_geometry(agri.pc.glos2) <- NULL

#> Convert NAs to zeros in BPS data
RPA_GL_Int[is.na(RPA_GL_Int)] <- 0

#> Group and summarise variables by postcode

#> 1. BPS 2018
bps.glos <- RPA_GL_Int %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total = sum(`Basic payment scheme`))
#> Merge with pcode agri data
m.bps <- merge(bps.glos, agri.pc.glos2, by.x = "PostcodePrefix_F202B", by.y = "name")
#> Add data from outside glos (diff)
agri.pc.glos.diff <- st_read(here("In", "Glos_Agri", "corine_agri_pcodes_diff.shp"))
#> Select relevant columns
agri.pc.glos.diff2 <- agri.pc.glos.diff %>% 
  select(name, Area_Agri)
#> Change col name to avoid duplicate cols in merged dataset
agri.pc.glos.diff2 <- agri.pc.glos.diff2 %>% 
rename(Area_Agri_Diff = Area_Agri)
#> Remove geom
st_geometry(agri.pc.glos.diff2) <- NULL
#> Merge with m.bps
m.bps <- merge(m.bps, agri.pc.glos.diff2, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = TRUE)
#> Change NAs to zero in agri diff col
m.bps$Area_Agri_Diff[is.na(m.bps$Area_Agri_Diff)] = 0
#> remove unwanted cols
m.bps <- m.bps %>% 
  select(-AreaKm2)
#> Compute BPS payment for postcode (current - 2018) based on agri area proportions
#> Total agri land
m.bps$Total_Agri_Land <- m.bps$Area_Agri + m.bps$Area_Agri_Diff
m.bps$Agri_land_glos_pc <- m.bps$Area_Agri / m.bps$Total_Agri_Land
m.bps$Agri_land_glos_pc <- round(m.bps$Agri_land_glos_pc, 6)
m.bps$BPS_Total_2018 <- m.bps$Agri_land_glos_pc * m.bps$BPS_Total
m.bps$BPS_Total_2018 <- round(m.bps$BPS_Total_2018, 2)


#> 2. BPS 2021
RPA_GL_Int.2021 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2021 reductions
RPA_GL_Int.2021$BPS_2021 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2021
RPA_GL_Int.2021 <- RPA_GL_Int.2021 %>% 
  mutate(BPS_2021 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.95,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.90,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.80,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.75))
# Group by postcode an summarise
RPA_GL_Int.2021.g <- RPA_GL_Int.2021 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2021 = sum(BPS_2021))
# merge with main BPS table
m.2021 <- merge(m.bps, RPA_GL_Int.2021.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2021$BPS_Total_2021 <- m.2021$BPS_Total_2021 * m.2021$Agri_land_glos_pc


#> 3. BPS 2022
RPA_GL_Int.2022 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2022 reductions
RPA_GL_Int.2022$BPS_2022 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2022
RPA_GL_Int.2022 <- RPA_GL_Int.2022 %>% 
  mutate(BPS_2022 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.80,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.75,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.65,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.60))
# Group by postcode an summarise
RPA_GL_Int.2022.g <- RPA_GL_Int.2022 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2022 = sum(BPS_2022))
# merge with main BPS table
m.2022 <- merge(m.2021, RPA_GL_Int.2022.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2022$BPS_Total_2022 <- m.2022$BPS_Total_2022 * m.2022$Agri_land_glos_pc


#> 4. BPS 2023
RPA_GL_Int.2023 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2023 reductions
RPA_GL_Int.2023$BPS_2023 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2023
RPA_GL_Int.2023 <- RPA_GL_Int.2023 %>% 
  mutate(BPS_2023 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.65,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.60,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.50,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.45))
# Group by postcode an summarise
RPA_GL_Int.2023.g <- RPA_GL_Int.2023 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2023 = sum(BPS_2023))
# merge with main BPS table
m.2023 <- merge(m.2022, RPA_GL_Int.2023.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2023$BPS_Total_2023 <- m.2023$BPS_Total_2023 * m.2023$Agri_land_glos_pc


#> 5. BPS 2024
RPA_GL_Int.2024 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2024 reductions
RPA_GL_Int.2024$BPS_2024 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2024
RPA_GL_Int.2024 <- RPA_GL_Int.2024 %>% 
  #> <=£30k - 5% reduction
  mutate(BPS_2024 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.50,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.45,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.35,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.30))
# Group by postcode an summarise
RPA_GL_Int.2024.g <- RPA_GL_Int.2024 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2024 = sum(BPS_2024))
# merge with main BPS table
m.2024 <- merge(m.2023, RPA_GL_Int.2024.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2024$BPS_Total_2024 <- m.2024$BPS_Total_2024 * m.2024$Agri_land_glos_pc


#> 6. BPS 2025
RPA_GL_Int.2025 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2025 reductions
RPA_GL_Int.2025$BPS_2025 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2025
RPA_GL_Int.2025 <- RPA_GL_Int.2025 %>% 
  mutate(BPS_2025 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.40,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.35,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.25,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.20))
# Group by postcode an summarise
RPA_GL_Int.2025.g <- RPA_GL_Int.2025 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2025 = sum(BPS_2025))
# merge with main BPS table
m.2025 <- merge(m.2024, RPA_GL_Int.2025.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2025$BPS_Total_2025 <- m.2025$BPS_Total_2025 * m.2025$Agri_land_glos_pc


#> 7. BPS 2026
RPA_GL_Int.2026 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2026 reductions
RPA_GL_Int.2026$BPS_2026 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2026
RPA_GL_Int.2026 <- RPA_GL_Int.2026 %>% 
  mutate(BPS_2026 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.25,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.25,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.15,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.15))
# Group by postcode an summarise
RPA_GL_Int.2026.g <- RPA_GL_Int.2026 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2026 = sum(BPS_2026))
# merge with main BPS table
m.2026 <- merge(m.2025, RPA_GL_Int.2026.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2026$BPS_Total_2026 <- m.2026$BPS_Total_2026 * m.2026$Agri_land_glos_pc



#> 8. BPS 2027
RPA_GL_Int.2027 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Add new column for 2027 reductions
RPA_GL_Int.2027$BPS_2027 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2027
RPA_GL_Int.2027 <- RPA_GL_Int.2027 %>% 
  mutate(BPS_2027 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.15,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.15,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.10,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.10))
# Group by postcode an summarise
RPA_GL_Int.2027.g <- RPA_GL_Int.2027 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2027 = sum(BPS_2027))
# merge with main BPS table
m.2027 <- merge(m.2026, RPA_GL_Int.2027.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2027$BPS_Total_2027 <- m.2027$BPS_Total_2027 * m.2027$Agri_land_glos_pc


#EXPORT FINAL BPS Table

write_csv(m.2027, here("Out", "BPS", "BPS_Glos_Reductions.csv"))





write_csv(m, here("Out", "Districts", "All_Land", paste0("Area_Stats_", dists[i], ".csv")))




#### 4. GREENING CALCULATIONS (based on agri land area) ####
#> Prepare table
grn.glos <- RPA_GL_Int %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total = sum(`Greening: practices beneficial for climate and environment`))
#> Merge with pcode agri data
m.grn <- merge(grn.glos, agri.pc.glos2, by.x = "PostcodePrefix_F202B", by.y = "name")
#> Add data from outside glos (diff)
agri.pc.glos.diff <- st_read(here("In", "Glos_Agri", "corine_agri_pcodes_diff.shp"))
#> Select relevant columns
agri.pc.glos.diff2 <- agri.pc.glos.diff %>% 
  select(name, Area_Agri)
#> Change col name to avoid duplicate cols in merged dataset
agri.pc.glos.diff2 <- agri.pc.glos.diff2 %>% 
  rename(Area_Agri_Diff = Area_Agri)
#> Remove geom
st_geometry(agri.pc.glos.diff2) <- NULL
#> Merge with m.grn
m.grn <- merge(m.grn, agri.pc.glos.diff2, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = TRUE)
#> Change NAs to zero in agri diff col
m.grn$Area_Agri_Diff[is.na(m.grn$Area_Agri_Diff)] = 0
#> remove unwanted cols
m.grn <- m.grn %>% 
  select(-AreaKm2)
#> Compute grn payment for postcode (current - 2018) based on agri area proportions
#> Total agri land
m.grn$Total_Agri_Land <- m.grn$Area_Agri + m.grn$Area_Agri_Diff
m.grn$Agri_land_glos_pc <- m.grn$Area_Agri / m.grn$Total_Agri_Land
m.grn$Agri_land_glos_pc <- round(m.grn$Agri_land_glos_pc, 4)
m.grn$grn_Total_2018 <- m.grn$Agri_land_glos_pc * m.grn$grn_Total
m.grn$grn_Total_2018 <- round(m.grn$grn_Total_2018, 2)



#> 2. grn 2021
RPA_GL_Int.2021 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2021 reductions
RPA_GL_Int.2021$grn_2021 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2021
RPA_GL_Int.2021 <- RPA_GL_Int.2021 %>% 
  mutate(grn_2021 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.95,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.90,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.80,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.75))
# Group by postcode an summarise
RPA_GL_Int.2021.g <- RPA_GL_Int.2021 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2021 = sum(grn_2021))
# merge with main grn table
m.2021 <- merge(m.grn, RPA_GL_Int.2021.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2021$grn_Total_2021 <- m.2021$grn_Total_2021 * m.2021$Agri_land_glos_pc


#> 3. grn 2022
RPA_GL_Int.2022 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2022 reductions
RPA_GL_Int.2022$grn_2022 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2022
RPA_GL_Int.2022 <- RPA_GL_Int.2022 %>% 
  mutate(grn_2022 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.80,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.75,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.65,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.60))
# Group by postcode an summarise
RPA_GL_Int.2022.g <- RPA_GL_Int.2022 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2022 = sum(grn_2022))
# merge with main grn table
m.2022 <- merge(m.2021, RPA_GL_Int.2022.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2022$grn_Total_2022 <- m.2022$grn_Total_2022 * m.2022$Agri_land_glos_pc


#> 4. grn 2023
RPA_GL_Int.2023 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2023 reductions
RPA_GL_Int.2023$grn_2023 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2023
RPA_GL_Int.2023 <- RPA_GL_Int.2023 %>% 
  mutate(grn_2023 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.65,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.60,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.50,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.45))
# Group by postcode an summarise
RPA_GL_Int.2023.g <- RPA_GL_Int.2023 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2023 = sum(grn_2023))
# merge with main grn table
m.2023 <- merge(m.2022, RPA_GL_Int.2023.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2023$grn_Total_2023 <- m.2023$grn_Total_2023 * m.2023$Agri_land_glos_pc


#> 5. grn 2024
RPA_GL_Int.2024 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2024 reductions
RPA_GL_Int.2024$grn_2024 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2024
RPA_GL_Int.2024 <- RPA_GL_Int.2024 %>% 
  #> <=£30k - 5% reduction
  mutate(grn_2024 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.50,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.45,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.35,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.30))
# Group by postcode an summarise
RPA_GL_Int.2024.g <- RPA_GL_Int.2024 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2024 = sum(grn_2024))
# merge with main grn table
m.2024 <- merge(m.2023, RPA_GL_Int.2024.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2024$grn_Total_2024 <- m.2024$grn_Total_2024 * m.2024$Agri_land_glos_pc


#> 6. grn 2025
RPA_GL_Int.2025 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2025 reductions
RPA_GL_Int.2025$grn_2025 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2025
RPA_GL_Int.2025 <- RPA_GL_Int.2025 %>% 
  mutate(grn_2025 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.40,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.35,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.25,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.20))
# Group by postcode an summarise
RPA_GL_Int.2025.g <- RPA_GL_Int.2025 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2025 = sum(grn_2025))
# merge with main grn table
m.2025 <- merge(m.2024, RPA_GL_Int.2025.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2025$grn_Total_2025 <- m.2025$grn_Total_2025 * m.2025$Agri_land_glos_pc


#> 7. grn 2026
RPA_GL_Int.2026 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2026 reductions
RPA_GL_Int.2026$grn_2026 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2026
RPA_GL_Int.2026 <- RPA_GL_Int.2026 %>% 
  mutate(grn_2026 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.25,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.25,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.15,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.15))
# Group by postcode an summarise
RPA_GL_Int.2026.g <- RPA_GL_Int.2026 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2026 = sum(grn_2026))
# merge with main grn table
m.2026 <- merge(m.2025, RPA_GL_Int.2026.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2026$grn_Total_2026 <- m.2026$grn_Total_2026 * m.2026$Agri_land_glos_pc



#> 8. grn 2027
RPA_GL_Int.2027 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
#> Add new column for 2027 reductions
RPA_GL_Int.2027$grn_2027 <- NA
#> Set up a function to calculate reductions based on grn reduction bands for 2027
RPA_GL_Int.2027 <- RPA_GL_Int.2027 %>% 
  mutate(grn_2027 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.15,
                              (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.15,
                              (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.10,
                              `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.10))
# Group by postcode an summarise
RPA_GL_Int.2027.g <- RPA_GL_Int.2027 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(grn_Total_2027 = sum(grn_2027))
# merge with main grn table
m.2027 <- merge(m.2026, RPA_GL_Int.2027.g, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
m.2027$grn_Total_2027 <- m.2027$grn_Total_2027 * m.2027$Agri_land_glos_pc


#EXPORT FINAL grn Table

write_csv(m.2027, here("Out", "Greening", "Greening_Glos_Reductions.csv"))

#### 5. BPS Calcs by District ####


# ##> 1. TEWKES
# # Get total agri land from m.bps
# ag.land.pcode.tot <- m.bps %>% 
#   select(PostcodePrefix_F202B, Total_Agri_Land)
# # Get District shapefiles containign agri area variable
# shp.Tewkes <- st_read(here("In", "Districts", "Clip_PC_Tewkes_Agri.shp"))
# #Remove geom
# st_geometry(shp.Tewkes) <- NULL
# #> aGRI LAND AREAS
# #Select relevant cols
# shp.Tewkes <- shp.Tewkes %>% 
#   select(name, Area_Agri)
# #? Get Tewkes data from main BPS table
# #> Get Tewekes psotcodes
# pc.d.Tewkes <- shp.Tewkes %>% 
#   select(name)
# #> Extract from main bps table with merge
# bps.Tewkes <- merge(RPA_GL_Int, pc.d.Tewkes, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
# #> Seelct cols
# bps.Tewkes <- bps.Tewkes %>% 
#   select(PostcodePrefix_F202B, `Basic payment scheme`)
# #> Group by pcode and summarise
# bps.Tewkes.2018 <- bps.Tewkes %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(Total_BPS_2018 = sum(`Basic payment scheme`))
# #> Merge with Tewkes shapefile to get area info
# bps.Tewkes.main <- merge(shp.Tewkes, bps.Tewkes.2018, by.x = "name", by.y = "PostcodePrefix_F202B")
# #> Merge with agri diff table
# bps.Tewkes.main <- merge(bps.Tewkes.main, agri.pc.glos.diff2, by.x = "name", by.y = "name", all.x = TRUE)
# #> Remove NAs in agri diff
# bps.Tewkes.main$Area_Agri_Diff[is.na(bps.Tewkes.main$Area_Agri_Diff)] <- 0
# #> Calculate total agri area
# bps.Tewkes.main$Total_Agri_Area <- bps.Tewkes.main$Area_Agri + bps.Tewkes.main$Area_Agri_Diff
# bps.Tewkes.main$Area_Agri_Diff <- round(bps.Tewkes.main$Area_Agri_Diff, 2)
# bps.Tewkes.main$Total_Agri_Area <- round(bps.Tewkes.main$Total_Agri_Area, 2)
# bps.Tewkes.main$Area_Agri <- round(bps.Tewkes.main$Area_Agri, 2)
# #> Propertion agri area
# bps.Tewkes.main$Agri_Area_pc <- bps.Tewkes.main$Area_Agri / bps.Tewkes.main$Total_Agri_Area
# bps.Tewkes.main$Agri_Area_pc <- round(bps.Tewkes.main$Agri_Area_pc, 2)
# #> Reorder cols
# bps.Tewkes.main <- bps.Tewkes.main %>% 
#   select(name, Area_Agri, Area_Agri_Diff, Total_Agri_Area, Agri_Area_pc, Total_BPS_2018)
# 
# 
# #> 2. BPS 2021
# bps.Tewkes.2021 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2021$BPS_2021 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2021
# bps.Tewkes.2021 <- bps.Tewkes.2021 %>% 
#   mutate(BPS_2021 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.95,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.90,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.80,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.75))
# # Group by postcode an summarise
# bps.Tewkes.2021  <- bps.Tewkes.2021  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2021 = sum(BPS_2021))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2021, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2021 <- bps.Tewkes.main$BPS_Total_2021 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# 
# #> 3. BPS 2022
# bps.Tewkes.2022 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2022$BPS_2022 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2022
# bps.Tewkes.2022 <- bps.Tewkes.2022 %>% 
#   mutate(BPS_2022 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.80,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.75,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.65,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.60))
# # Group by postcode an summarise
# bps.Tewkes.2022  <- bps.Tewkes.2022  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2022 = sum(BPS_2022))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2022, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2022 <- bps.Tewkes.main$BPS_Total_2022 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# 
# #> 4. BPS 2023
# bps.Tewkes.2023 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2023$BPS_2023 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2023
# bps.Tewkes.2023 <- bps.Tewkes.2023 %>% 
#   mutate(BPS_2023 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.65,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.60,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.50,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.45))
# # Group by postcode an summarise
# bps.Tewkes.2023  <- bps.Tewkes.2023  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2023 = sum(BPS_2023))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2023, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2023 <- bps.Tewkes.main$BPS_Total_2023 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# 
# #> 5. BPS 2024
# bps.Tewkes.2024 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2024$BPS_2024 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2024
# bps.Tewkes.2024 <- bps.Tewkes.2024 %>% 
#   mutate(BPS_2024 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.50,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.45,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.35,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.30))
# bps.Tewkes.2024  <- bps.Tewkes.2024  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2024 = sum(BPS_2024))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2024, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2024 <- bps.Tewkes.main$BPS_Total_2024 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# 
# #> 6. BPS 2025
# bps.Tewkes.2025 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2025$BPS_2025 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2025
# bps.Tewkes.2025 <- bps.Tewkes.2025 %>% 
#   mutate(BPS_2025 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.40,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.35,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.25,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.20))
# bps.Tewkes.2025  <- bps.Tewkes.2025  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2025 = sum(BPS_2025))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2025, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2025 <- bps.Tewkes.main$BPS_Total_2025 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# #> 7. BPS 2026
# bps.Tewkes.2026 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2026$BPS_2026 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2026
# bps.Tewkes.2026 <- bps.Tewkes.2026 %>% 
#   mutate(BPS_2026 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.25,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.25,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.15,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.15))
# bps.Tewkes.2026  <- bps.Tewkes.2026  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2026 = sum(BPS_2026))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2026, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2026 <- bps.Tewkes.main$BPS_Total_2026 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# #> 8. BPS 2027
# bps.Tewkes.2027 <- bps.Tewkes
# #> Create col for year reduction
# bps.Tewkes.2027$BPS_2027 <- NA
# #> Set up a function to calculate reductions based on BPS reduction bands for 2027
# bps.Tewkes.2027 <- bps.Tewkes.2027 %>% 
#   mutate(BPS_2027 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.15,
#                               (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.15,
#                               (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.10,
#                               `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.10))
# bps.Tewkes.2027  <- bps.Tewkes.2027  %>% 
#   group_by(PostcodePrefix_F202B) %>% 
#   summarise(BPS_Total_2027 = sum(BPS_2027))
# # merge with main BPS table
# bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2027, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# # Change values according to agi land area proportion
# bps.Tewkes.main$BPS_Total_2027 <- bps.Tewkes.main$BPS_Total_2027 * bps.Tewkes.main$Agri_Area_pc
# 
# 
# #EXPORT FINAL BPS Table
# 
# write_csv(bps.Tewkes.main, here("Out", "BPS", "Districts", "BPS_Tewkes_Reductions.csv"))


#> For loop version


districts <- c("Tewkes", "Chelt", "Cotswold", "FoD", "Glos", "Stroud")

for (i in 1:length(districts))
{

##> 1. TEWKES
# Get total agri land from m.bps
ag.land.pcode.tot <- m.bps %>% 
  select(PostcodePrefix_F202B, Total_Agri_Land)
# Get District shapefiles containign agri area variable
shp.Tewkes <- st_read(here("In", "Districts", paste0("Clip_PC_", districts[i], "_Agri.shp")))
#Remove geom
st_geometry(shp.Tewkes) <- NULL
#> aGRI LAND AREAS
#Select relevant cols
shp.Tewkes <- shp.Tewkes %>% 
  select(name, Area_Agri)
#? Get Tewkes data from main BPS table
#> Get Tewekes psotcodes
pc.d.Tewkes <- shp.Tewkes %>% 
  select(name)
#> Extract from main bps table with merge
bps.Tewkes <- merge(RPA_GL_Int, pc.d.Tewkes, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
#> Seelct cols
bps.Tewkes <- bps.Tewkes %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`)
#> Group by pcode and summarise
bps.Tewkes.2018 <- bps.Tewkes %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(Total_BPS_2018 = sum(`Basic payment scheme`))
#> Merge with Tewkes shapefile to get area info
bps.Tewkes.main <- merge(shp.Tewkes, bps.Tewkes.2018, by.x = "name", by.y = "PostcodePrefix_F202B")
#> Merge with agri diff table
bps.Tewkes.main <- merge(bps.Tewkes.main, agri.pc.glos.diff2, by.x = "name", by.y = "name", all.x = TRUE)
#> Remove NAs in agri diff
bps.Tewkes.main$Area_Agri_Diff[is.na(bps.Tewkes.main$Area_Agri_Diff)] <- 0
#> Calculate total agri area
bps.Tewkes.main$Total_Agri_Area <- bps.Tewkes.main$Area_Agri + bps.Tewkes.main$Area_Agri_Diff
bps.Tewkes.main$Area_Agri_Diff <- round(bps.Tewkes.main$Area_Agri_Diff, 2)
bps.Tewkes.main$Total_Agri_Area <- round(bps.Tewkes.main$Total_Agri_Area, 2)
bps.Tewkes.main$Area_Agri <- round(bps.Tewkes.main$Area_Agri, 2)
#> Propertion agri area
bps.Tewkes.main$Agri_Area_pc <- bps.Tewkes.main$Area_Agri / bps.Tewkes.main$Total_Agri_Area
bps.Tewkes.main$Agri_Area_pc <- round(bps.Tewkes.main$Agri_Area_pc, 2)
#> Reorder cols
bps.Tewkes.main <- bps.Tewkes.main %>% 
  select(name, Area_Agri, Area_Agri_Diff, Total_Agri_Area, Agri_Area_pc, Total_BPS_2018)


#> 2. BPS 2021
bps.Tewkes.2021 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2021$BPS_2021 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2021
bps.Tewkes.2021 <- bps.Tewkes.2021 %>% 
  mutate(BPS_2021 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.95,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.90,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.80,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.75))
# Group by postcode an summarise
bps.Tewkes.2021  <- bps.Tewkes.2021  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2021 = sum(BPS_2021))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2021, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2021 <- bps.Tewkes.main$BPS_Total_2021 * bps.Tewkes.main$Agri_Area_pc



#> 3. BPS 2022
bps.Tewkes.2022 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2022$BPS_2022 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2022
bps.Tewkes.2022 <- bps.Tewkes.2022 %>% 
  mutate(BPS_2022 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.80,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.75,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.65,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.60))
# Group by postcode an summarise
bps.Tewkes.2022  <- bps.Tewkes.2022  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2022 = sum(BPS_2022))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2022, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2022 <- bps.Tewkes.main$BPS_Total_2022 * bps.Tewkes.main$Agri_Area_pc



#> 4. BPS 2023
bps.Tewkes.2023 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2023$BPS_2023 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2023
bps.Tewkes.2023 <- bps.Tewkes.2023 %>% 
  mutate(BPS_2023 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.65,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.60,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.50,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.45))
# Group by postcode an summarise
bps.Tewkes.2023  <- bps.Tewkes.2023  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2023 = sum(BPS_2023))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2023, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2023 <- bps.Tewkes.main$BPS_Total_2023 * bps.Tewkes.main$Agri_Area_pc



#> 5. BPS 2024
bps.Tewkes.2024 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2024$BPS_2024 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2024
bps.Tewkes.2024 <- bps.Tewkes.2024 %>% 
  mutate(BPS_2024 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.50,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.45,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.35,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.30))
bps.Tewkes.2024  <- bps.Tewkes.2024  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2024 = sum(BPS_2024))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2024, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2024 <- bps.Tewkes.main$BPS_Total_2024 * bps.Tewkes.main$Agri_Area_pc



#> 6. BPS 2025
bps.Tewkes.2025 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2025$BPS_2025 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2025
bps.Tewkes.2025 <- bps.Tewkes.2025 %>% 
  mutate(BPS_2025 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.40,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.35,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.25,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.20))
bps.Tewkes.2025  <- bps.Tewkes.2025  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2025 = sum(BPS_2025))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2025, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2025 <- bps.Tewkes.main$BPS_Total_2025 * bps.Tewkes.main$Agri_Area_pc


#> 7. BPS 2026
bps.Tewkes.2026 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2026$BPS_2026 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2026
bps.Tewkes.2026 <- bps.Tewkes.2026 %>% 
  mutate(BPS_2026 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.25,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.25,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.15,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.15))
bps.Tewkes.2026  <- bps.Tewkes.2026  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2026 = sum(BPS_2026))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2026, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2026 <- bps.Tewkes.main$BPS_Total_2026 * bps.Tewkes.main$Agri_Area_pc


#> 8. BPS 2027
bps.Tewkes.2027 <- bps.Tewkes
#> Create col for year reduction
bps.Tewkes.2027$BPS_2027 <- NA
#> Set up a function to calculate reductions based on BPS reduction bands for 2027
bps.Tewkes.2027 <- bps.Tewkes.2027 %>% 
  mutate(BPS_2027 = case_when(`Basic payment scheme` <=30000 ~ `Basic payment scheme` * 0.15,
                              (`Basic payment scheme` >30000 & `Basic payment scheme` <=50000) ~ `Basic payment scheme` * 0.15,
                              (`Basic payment scheme` >50000 & `Basic payment scheme` <=150000) ~ `Basic payment scheme` * 0.10,
                              `Basic payment scheme` > 150000 ~ `Basic payment scheme` * 0.10))
bps.Tewkes.2027  <- bps.Tewkes.2027  %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(BPS_Total_2027 = sum(BPS_2027))
# merge with main BPS table
bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2027, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
bps.Tewkes.main$BPS_Total_2027 <- bps.Tewkes.main$BPS_Total_2027 * bps.Tewkes.main$Agri_Area_pc


#EXPORT FINAL BPS Table

write_csv(bps.Tewkes.main, here("Out", "BPS", "Districts", paste0("BPS_", districts[i], "_Reductions.csv")))

}


# 6. Greening calcs by district ####
#> For loop version


districts <- c("Tewkes", "Chelt", "Cotswold", "FoD", "Glos", "Stroud")

for (i in 1:length(districts))
{
  
  ##> 1. TEWKES
  # Get total agri land from m.bps
  ag.land.pcode.tot <- m.bps %>% 
    select(PostcodePrefix_F202B, Total_Agri_Land)
  # Get District shapefiles containign agri area variable
  shp.Tewkes <- st_read(here("In", "Districts", paste0("Clip_PC_", districts[i], "_Agri.shp")))
  #Remove geom
  st_geometry(shp.Tewkes) <- NULL
  #> aGRI LAND AREAS
  #Select relevant cols
  shp.Tewkes <- shp.Tewkes %>% 
    select(name, Area_Agri)
  #? Get Tewkes data from main BPS table
  #> Get Tewekes psotcodes
  pc.d.Tewkes <- shp.Tewkes %>% 
    select(name)
  #> Extract from main bps table with merge
  bps.Tewkes <- merge(RPA_GL_Int, pc.d.Tewkes, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
  #> Seelct cols
  bps.Tewkes <- bps.Tewkes %>% 
    select(PostcodePrefix_F202B, `Greening: practices beneficial for climate and environment`)
  #> Group by pcode and summarise
  bps.Tewkes.2018 <- bps.Tewkes %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(Total_BPS_2018 = sum(`Greening: practices beneficial for climate and environment`))
  #> Merge with Tewkes shapefile to get area info
  bps.Tewkes.main <- merge(shp.Tewkes, bps.Tewkes.2018, by.x = "name", by.y = "PostcodePrefix_F202B")
  #> Merge with agri diff table
  bps.Tewkes.main <- merge(bps.Tewkes.main, agri.pc.glos.diff2, by.x = "name", by.y = "name", all.x = TRUE)
  #> Remove NAs in agri diff
  bps.Tewkes.main$Area_Agri_Diff[is.na(bps.Tewkes.main$Area_Agri_Diff)] <- 0
  #> Calculate total agri area
  bps.Tewkes.main$Total_Agri_Area <- bps.Tewkes.main$Area_Agri + bps.Tewkes.main$Area_Agri_Diff
  bps.Tewkes.main$Area_Agri_Diff <- round(bps.Tewkes.main$Area_Agri_Diff, 2)
  bps.Tewkes.main$Total_Agri_Area <- round(bps.Tewkes.main$Total_Agri_Area, 2)
  bps.Tewkes.main$Area_Agri <- round(bps.Tewkes.main$Area_Agri, 2)
  #> Propertion agri area
  bps.Tewkes.main$Agri_Area_pc <- bps.Tewkes.main$Area_Agri / bps.Tewkes.main$Total_Agri_Area
  bps.Tewkes.main$Agri_Area_pc <- round(bps.Tewkes.main$Agri_Area_pc, 2)
  #> Reorder cols
  bps.Tewkes.main <- bps.Tewkes.main %>% 
    select(name, Area_Agri, Area_Agri_Diff, Total_Agri_Area, Agri_Area_pc, Total_BPS_2018)
  
  
  #> 2. BPS 2021
  bps.Tewkes.2021 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2021$BPS_2021 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2021
  bps.Tewkes.2021 <- bps.Tewkes.2021 %>% 
    mutate(BPS_2021 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.95,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.90,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.80,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.75))
  # Group by postcode an summarise
  bps.Tewkes.2021  <- bps.Tewkes.2021  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2021 = sum(BPS_2021))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2021, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2021 <- bps.Tewkes.main$BPS_Total_2021 * bps.Tewkes.main$Agri_Area_pc
  
  
  
  #> 3. BPS 2022
  bps.Tewkes.2022 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2022$BPS_2022 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2022
  bps.Tewkes.2022 <- bps.Tewkes.2022 %>% 
    mutate(BPS_2022 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.80,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.75,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.65,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.60))
  # Group by postcode an summarise
  bps.Tewkes.2022  <- bps.Tewkes.2022  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2022 = sum(BPS_2022))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2022, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2022 <- bps.Tewkes.main$BPS_Total_2022 * bps.Tewkes.main$Agri_Area_pc
  
  
  
  #> 4. BPS 2023
  bps.Tewkes.2023 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2023$BPS_2023 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2023
  bps.Tewkes.2023 <- bps.Tewkes.2023 %>% 
    mutate(BPS_2023 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.65,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.60,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.50,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.45))
  # Group by postcode an summarise
  bps.Tewkes.2023  <- bps.Tewkes.2023  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2023 = sum(BPS_2023))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2023, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2023 <- bps.Tewkes.main$BPS_Total_2023 * bps.Tewkes.main$Agri_Area_pc
  
  
  
  #> 5. BPS 2024
  bps.Tewkes.2024 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2024$BPS_2024 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2024
  bps.Tewkes.2024 <- bps.Tewkes.2024 %>% 
    mutate(BPS_2024 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.50,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.45,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.35,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.30))
  bps.Tewkes.2024  <- bps.Tewkes.2024  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2024 = sum(BPS_2024))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2024, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2024 <- bps.Tewkes.main$BPS_Total_2024 * bps.Tewkes.main$Agri_Area_pc
  
  
  
  #> 6. BPS 2025
  bps.Tewkes.2025 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2025$BPS_2025 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2025
  bps.Tewkes.2025 <- bps.Tewkes.2025 %>% 
    mutate(BPS_2025 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.40,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.35,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.25,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.20))
  bps.Tewkes.2025  <- bps.Tewkes.2025  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2025 = sum(BPS_2025))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2025, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2025 <- bps.Tewkes.main$BPS_Total_2025 * bps.Tewkes.main$Agri_Area_pc
  
  
  #> 7. BPS 2026
  bps.Tewkes.2026 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2026$BPS_2026 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2026
  bps.Tewkes.2026 <- bps.Tewkes.2026 %>% 
    mutate(BPS_2026 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.25,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.25,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.15,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.15))
  bps.Tewkes.2026  <- bps.Tewkes.2026  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2026 = sum(BPS_2026))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2026, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2026 <- bps.Tewkes.main$BPS_Total_2026 * bps.Tewkes.main$Agri_Area_pc
  
  
  #> 8. BPS 2027
  bps.Tewkes.2027 <- bps.Tewkes
  #> Create col for year reduction
  bps.Tewkes.2027$BPS_2027 <- NA
  #> Set up a function to calculate reductions based on BPS reduction bands for 2027
  bps.Tewkes.2027 <- bps.Tewkes.2027 %>% 
    mutate(BPS_2027 = case_when(`Greening: practices beneficial for climate and environment` <=30000 ~ `Greening: practices beneficial for climate and environment` * 0.15,
                                (`Greening: practices beneficial for climate and environment` >30000 & `Greening: practices beneficial for climate and environment` <=50000) ~ `Greening: practices beneficial for climate and environment` * 0.15,
                                (`Greening: practices beneficial for climate and environment` >50000 & `Greening: practices beneficial for climate and environment` <=150000) ~ `Greening: practices beneficial for climate and environment` * 0.10,
                                `Greening: practices beneficial for climate and environment` > 150000 ~ `Greening: practices beneficial for climate and environment` * 0.10))
  bps.Tewkes.2027  <- bps.Tewkes.2027  %>% 
    group_by(PostcodePrefix_F202B) %>% 
    summarise(BPS_Total_2027 = sum(BPS_2027))
  # merge with main BPS table
  bps.Tewkes.main <- merge(bps.Tewkes.main, bps.Tewkes.2027, by.x = "name", by.y = "PostcodePrefix_F202B", all.x = TRUE)
  # Change values according to agi land area proportion
  bps.Tewkes.main$BPS_Total_2027 <- bps.Tewkes.main$BPS_Total_2027 * bps.Tewkes.main$Agri_Area_pc
  
  
  #EXPORT FINAL BPS Table
  
  write_csv(bps.Tewkes.main, here("Out", "Greening", "Districts", paste0("Greening_", districts[i], "_Reductions.csv")))
  
}






































