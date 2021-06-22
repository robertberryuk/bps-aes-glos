#> BASED ON ORIGINAL SCRIPT BUT UPDATED FOR 2020 RPA

#> libs
library(tidyverse)
library(sf)
library(here)



#### 1. DATA IN AND PREP ####

#> Get data
#> RPA1
RPA1 <- read_csv(here("In", "2020_All_CAP_RPA.csv"))
#> RPA2
RPA2 <- read_csv(here("In", "2020_All_CAP_RPA2.csv"))
#> Bind tables together
#> Bind the two RPA tables together (they are split alphabetically by beneficiary name)
RPA_All <- rbind(RPA1, RPA2)
#> Remove duplicates
RPA_All <- RPA_All %>% 
  distinct()
#> Select only relevant records
RPA <- RPA_All %>% 
  select("BeneficiaryCode", "PostcodePrefix_F202B", "TownCity_F202C", "Basic payment scheme", "Greening: practices beneficial for climate and environment", "Agri-environment-climate", "Forest environmental and climate services and forest conservation", "Forest-environment payments", "Investment in forest area development and improvement of the viability of forests", "Investments in physical assets", "Organic farming", "Support for Leader local development")
glimpse(RPA)
glimpse(RPA_All)


# #> Code to get list of RPA records with GL postcode (not needed because intersect reveals other non GL postcodes intersect Glos - e.g. "CV")
# #> Get list of GL postcodes for extraction
# PCodes_Glos <- read_csv(here("In", "Pcode_Districts_Glos.csv"))
# #> Merge RPAs with Glos postcode districts
# RPA1_GL_ALL <- merge(RPA1, PCodes_Glos, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
# RPA2_GL_ALL <- merge(RPA2, PCodes_Glos, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)



#### 2. AREA CALCULATIONS (TOTAL LAND AREA) ####
#> Do area calcs on postcode clip and difference layers to get proportion of postcodes in and out of Glos boundary AND district boundaries
##> CALCULATE AREAS (TOTA: AREA)

#> 2.1 Total PC AREA ####-----
#> Area of pcode district polys that intersect Glos (area pre-calculated in Q)
PC_Glos_Int <- st_read(here("In", "Pcode_Polys_Glos_Int.shp"))

#> Calculate new field to hold area of postcode districts - original calc done in QGIS
PC_Glos_Int$AreaKmNew <- st_area(PC_Glos_Int)
#> Convert from m2 to km2
PC_Glos_Int$AreaKmNew <- PC_Glos_Int$AreaKmNew / 1000000
PC_Glos_Int$AreaKmNew <- round(PC_Glos_Int$AreaKmNew, digits = 2)
PC_Glos_Int$AreaKmNew <- as.numeric(PC_Glos_Int$AreaKmNew)
#> Drop QGIS-generated area column
PC_Glos_Int$AreaKm2 <- NULL
#> Rename newly genrated area column 
PC_Glos_Int <- PC_Glos_Int %>% 
  rename(PC_Area_Total = AreaKmNew)
#> Remove geom
st_geometry(PC_Glos_Int) <- NULL


#> Get a list of unique poscode districts that intersect Glos from above layer
pc.int.glos <- as.data.frame(unique(PC_Glos_Int$name))


#> 2.2. PC Clip Area (Area of intersecting pcode district within Glos alc done in QGIS) ####----
PC_Glos_Clip <- st_read(here("In", "Pcode_Polys_Glos_CLIP.shp"))
#> Calculate new field to hold area of postcode districts - original calc done in QGIS
PC_Glos_Clip$AreaKmNew <- st_area(PC_Glos_Clip)
#> Convert from m2 to km2
PC_Glos_Clip$AreaKmNew <- PC_Glos_Clip$AreaKmNew / 1000000
PC_Glos_Clip$AreaKmNew <- round(PC_Glos_Clip$AreaKmNew, digits = 2)
PC_Glos_Clip$AreaKmNew <- as.numeric(PC_Glos_Clip$AreaKmNew)
#> Drop QGIS-generated area column
PC_Glos_Clip$AreaKm2 <- NULL
#> Rename newly generated area column 
PC_Glos_Clip <- PC_Glos_Clip %>% 
  rename(PC_Area_Clip = AreaKmNew)
#> Remove geom
st_geometry(PC_Glos_Clip) <- NULL



#> 2.3 PC Diff Area (Area of intersecting pcode district outside Glos - calc done in QGIS) ####----
PC_Glos_Diff <- st_read(here("In", "Pcode_Polys_Glos_Diff.shp"))
#> Calculate new field to hold area of postcode districts - original calc done in QGIS
PC_Glos_Diff$AreaKmNew <- st_area(PC_Glos_Diff)
#> Convert from m2 to km2
PC_Glos_Diff$AreaKmNew <- PC_Glos_Diff$AreaKmNew / 1000000
PC_Glos_Diff$AreaKmNew <- round(PC_Glos_Diff$AreaKmNew, digits = 2)
PC_Glos_Diff$AreaKmNew <- as.numeric(PC_Glos_Diff$AreaKmNew)
#> Drop QGIS-generated area column
PC_Glos_Diff$AreaKm2 <- NULL
#> Rename newly genrrated area column 
PC_Glos_Diff <- PC_Glos_Diff %>% 
  rename(PC_Area_Diff = AreaKmNew)
#> Remove geom
st_geometry(PC_Glos_Diff) <- NULL



#> 2.4 Prepare area stats at Glos level ####----
# Extract relevant intersecting (with Glos county boundary) postcodes from RPA datasets
RPA_GL_Int <- merge(RPA, PC_Glos_Int, by.x = "PostcodePrefix_F202B", "name", all.x = FALSE)
#> Export 
write_csv(RPA_GL_Int, (here("Out", "1_UPDATED", "RPA_Glos_Pcodes.csv")))

#> Merge the PC district area values (i.e. total, clip, diff) together
m1 <- merge(PC_Glos_Int, PC_Glos_Clip, by = "name")
m2 <- merge(m1, PC_Glos_Diff, by = "name", all.x = TRUE)
#> Change NAs to zero (where NA means "no land outside Glos")
m2[is.na(m2)] <- 0 
#> Change "PC_Area_Clip" for GL20 to value of total for GL20 - this is a repair due to geom error which meant some land was allocated outside of Glos when it should all be in glos
m2[m2$name=="GL2", "PC_Area_Clip"] <- m2[m2$name=="GL2", "PC_Area_Total"]
m2[m2$name=="GL2", "PC_Area_Diff"] <- 0
#> Export as table showing proportion (area) of postcode districts within and outside of Glos county boundary
write_csv(m2, here("Out", "1_UPDATED", "Area_Stats_Glos_County.csv"))



#> 2.5 Prepare area stats at district level ####----
# Repeat workflow to get figures for postcode districts at district council level
# Set up a list of district names (as they appear in files) as input for loop
dists <- c("Chelt", "Cotswold", "FoD", "Glos", "Stroud", "Tewkes")

for (i in 1:length(dists))
{
  x <- st_read(here("In", "Districts", paste0("Clip_PC_", dists[i], ".shp")))
  x$AreaNew <- st_area(x)
  x$AreaNew <- x$AreaNew/ 1000000
  x$AreaNew <- round(x$AreaNew, digits = 2)
  x$AreaNew <- as.numeric(x$AreaNew)
  #> Drop QGIS-generated area column
  x$AreaKm2 <- NULL
  #> Rename newly generated area column 
  x <- x %>% 
    rename(AreaKm2 = AreaNew)
  m <- merge(x, PC_Glos_Int, by= "name", all.x = TRUE)
  st_geometry(m) <- NULL
  m <- m %>% 
    select(name, AreaKm2, PC_Area_Total)
  write_csv(m, here("Out", "1_UPDATED", "Districts", "All_Land", paste0("Area_Stats_", dists[i], ".csv")))
}




#### 3. P1 CALCULATIONS GLOS ALL (based on agri land area) ####

#> Add agri land areas data from within Glos
agri.pc.glos <- st_read(here("In", "Glos_Agri", "corine_agri_pcodes_Glos.shp"))
#> Select only relevant columns
agri.pc.glos2 <- agri.pc.glos %>% 
  select(name, Area_Agri)
#> Remove geom
st_geometry(agri.pc.glos2) <- NULL
#> Merge with m2 to append area variables
agri.glos <- merge(m2, agri.pc.glos2, by = "name", all.x = TRUE)
#> Round agri area
agri.glos$Area_Agri <- round(agri.glos$Area_Agri, digits = 2)
#> Rename cols
agri.glos <- agri.glos %>% 
  rename(PC_Area_In = PC_Area_Clip) %>% 
  rename(PC_Area_Out = PC_Area_Diff)
#> Add agri land areas data from outside Glos
agri.pc.glos.diff <- st_read(here("In", "Glos_Agri", "corine_agri_pcodes_diff.shp"))
#> Remove geom
st_geometry(agri.pc.glos.diff) <- NULL
#> Select only relevant cols
agri.pc.glos.diff <- agri.pc.glos.diff %>% 
  select(name, Area_Agri)
#> Round
agri.pc.glos.diff$Area_Agri <- round(agri.pc.glos.diff$Area_Agri, digits = 2)
#> Change col name
agri.pc.glos.diff <- agri.pc.glos.diff %>% 
  rename(Area_Agri_Out = Area_Agri)
#> Merge with "agri.glos" to append area of agri outside of glod to table
agri.glos <- merge(agri.glos, agri.pc.glos.diff, by = "name", all.x = TRUE)
#> Change NAs to zero
agri.glos$Area_Agri_Out[is.na(agri.glos$Area_Agri_Out)] <- 0
a$x[is.na(a$x)] <- 0


#> Convert NAs to zeros in BPS data
RPA_GL_Int[is.na(RPA_GL_Int)] <- 0


glimpse(RPA_GL_Int)

#> 3.1 P1 2020 ####----
p1.2020 <- RPA_GL_Int %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2020_Unadjusted = sum(`Basic payment scheme`) + sum(`Greening: practices beneficial for climate and environment`))
sum.original.p1 <- sum(p1.2020$P1_2020_Unadjusted)
#> Merge with agri.glos to append 2020 bps
p1 <- merge(p1.2020, agri.glos, by.x = "PostcodePrefix_F202B", by.y = "name")
#> Calculate percentage of area of agricultural land in in each postcode area that is wihin Gloucestershire
#> This will be used to calculate final BPS figure for each year
#> Check sum first
sum.p1.merge <- sum(p1$P1_2020)
#> Calculate total agri area
p1$Area_Agri_Tot <- p1$Area_Agri + p1$Area_Agri_Out
#> Calcucate percentage of agricultural land within glos - for final p1 calculations
p1$Pcent_Agri <-  p1$Area_Agri /p1$Area_Agri_Tot
#> Round
p1$Pcent_Agri <- round(p1$Pcent_Agri, digits = 2)
#> Change "inf" values (caused by dividing by 0 above) to zero
p1$Pcent_Agri[!is.finite(p1$Pcent_Agri)] <- 0
#> Move 2020 figures to last column
p1 <- p1  %>% 
  relocate(P1_2020_Unadjusted, .after = last_col())
#> Calculate P1 payment based on area of agri land within each postcode area
p1$P1_2020 <- p1$P1_2020_Unadjusted * p1$Pcent_Agri
#> Round 
p1$P1_2020 <-round(p1$P1_2020, digits = 0)
#> Check sum
sum.p1.2020 <- sum(p1$P1_2020)


#> 3.2 P1 2021 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2021 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2021$P1_2020 <- p1.2021$`Basic payment scheme` + p1.2021$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2021 reductions
p1.2021 <- p1.2021 %>% 
  mutate(P1_2021 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.95,
                              (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.90,
                              (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.80,
                              P1_2020 > 150000 ~ P1_2020 * 0.75))
# Group by postcode an summarise
p1.2021 <- p1.2021 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2021 = sum(P1_2021))
#> Round
p1.2021$P1_2021 <- round(p1.2021$P1_2021, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2021, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2021 <- p1$P1_2021 * p1$Pcent_Agri
#> Round
p1$P1_2021 <- round(p1$P1_2021, digits = 0)
#> Check sum
sum.p1.2021 <- sum(p1$P1_2021)


#> 3.3 P1 2022 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2022 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2022$P1_2020 <- p1.2022$`Basic payment scheme` + p1.2022$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2022 reductions
p1.2022 <- p1.2022 %>% 
  mutate(P1_2022 = case_when(P1_2020<=30000 ~ P1_2020* 0.80,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.75,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.65,
                             P1_2020> 150000 ~ P1_2020* 0.60))
# Group by postcode an summarise
p1.2022 <- p1.2022 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2022 = sum(P1_2022))
#> Round
p1.2022$P1_2022 <- round(p1.2022$P1_2022, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2022, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2022 <- p1$P1_2022 * p1$Pcent_Agri
#> Round
p1$P1_2022 <- round(p1$P1_2022, digits = 0)
#> Check sum
sum.p1.2022 <- sum(p1$P1_2022)


#> 3.4 P1 2023 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2023 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2023$P1_2020 <- p1.2023$`Basic payment scheme` + p1.2023$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2023 reductions
p1.2023 <- p1.2023 %>% 
  mutate(P1_2023 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.65,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.60,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.50,
                             P1_2020 > 150000 ~ P1_2020 * 0.45))
# Group by postcode an summarise
p1.2023 <- p1.2023 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2023 = sum(P1_2023))
#> Round
p1.2023$P1_2023 <- round(p1.2023$P1_2023, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2023, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2023 <- p1$P1_2023 * p1$Pcent_Agri
#> Round
p1$P1_2023 <- round(p1$P1_2023, digits = 0)
#> Check sum
sum.p1.2023 <- sum(p1$P1_2023)


#> 3.5 P1 2024 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2024 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2024$P1_2020 <- p1.2024$`Basic payment scheme` + p1.2024$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2024 reductions
p1.2024 <- p1.2024 %>% 
  mutate(P1_2024 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.50,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.45,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.35,
                             P1_2020 > 150000 ~ P1_2020 * 0.30))
# Group by postcode an summarise
p1.2024 <- p1.2024 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2024 = sum(P1_2024))
#> Round
p1.2024$P1_2024 <- round(p1.2024$P1_2024, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2024, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2024 <- p1$P1_2024 * p1$Pcent_Agri
#> Round
p1$P1_2024 <- round(p1$P1_2024, digits = 0)
#> Check sum
sum.p1.2024 <- sum(p1$P1_2024)


#> 3.6 P1 2025 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2025 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2025$P1_2020 <- p1.2025$`Basic payment scheme` + p1.2025$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2025 reductions
p1.2025 <- p1.2025 %>% 
  mutate(P1_2025 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.40,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.35,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.25,
                             P1_2020 > 150000 ~ P1_2020 * 0.20))
# Group by postcode an summarise
p1.2025 <- p1.2025 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2025 = sum(P1_2025))
#> Round
p1.2025$P1_2025 <- round(p1.2025$P1_2025, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2025, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2025 <- p1$P1_2025 * p1$Pcent_Agri
#> Round
p1$P1_2025 <- round(p1$P1_2025, digits = 0)
#> Check sum
sum.p1.2025 <- sum(p1$P1_2025)


#> 3.7 P1 2026 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2026 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2026$P1_2020 <- p1.2026$`Basic payment scheme` + p1.2026$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2026 reductions
p1.2026 <- p1.2026 %>% 
  mutate(P1_2026 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.25,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.25,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.15,
                             P1_2020 > 150000 ~ P1_2020 * 0.15))
# Group by postcode an summarise
p1.2026 <- p1.2026 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2026 = sum(P1_2026))
#> Round
p1.2026$P1_2026 <- round(p1.2026$P1_2026, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2026, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2026 <- p1$P1_2026 * p1$Pcent_Agri
#> Round
p1$P1_2026 <- round(p1$P1_2026, digits = 0)
#> Check sum
sum.p1.2026 <- sum(p1$P1_2026)


#> 3.8 P1 2027 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
p1.2027 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. BPS + Greening)
p1.2027$P1_2020 <- p1.2027$`Basic payment scheme` + p1.2027$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2027 reductions
p1.2027 <- p1.2027 %>% 
  mutate(P1_2027 = case_when(P1_2020<=30000 ~ P1_2020* 0.15,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.15,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.10,
                             P1_2020> 150000 ~ P1_2020* 0.10))
# Group by postcode an summarise
p1.2027 <- p1.2027 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2027 = sum(P1_2027))
#> Round
p1.2027$P1_2027 <- round(p1.2027$P1_2027, digits = 0)
# merge with main P1 results table (m.)
p1 <- merge(p1, p1.2027, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1$P1_2027 <- p1$P1_2027 * p1$Pcent_Agri
#> Round
p1$P1_2027 <- round(p1$P1_2027, digits = 0)
#> Check sum
sum.p1.2027 <- sum(p1$P1_2027)


#EXPORT FINAL P1 Table
write_csv(p1, here("Out", "1_UPDATED", "P1", "P1_Reductions_Glos.csv"))



write_csv(m, here("Out", "Districts", "All_Land", paste0("Area_Stats_", dists[i], ".csv")))




#### 4. P1 CALCS BY DISTRICT ####

#> Initiate for loop and looping variables (district file names)
districts <- c("Tewkes", "Chelt", "Cotswold", "FoD", "Glos", "Stroud")

for (i in 1:length(districts)){

x <- st_read(here("In", "Districts", paste0("Clip_PC_", districts[i], "_Agri.shp")))


# x <- st_read(here("In", "Districts", "Clip_PC_Chelt_Agri.shp"))

#Remove geom
st_geometry(x) <- NULL
#> aGRI LAND AREAS
#Select relevant cols
x <- x %>% 
  select(name, Area_Agri)
#> Get Tewekes psotcodes
pc.d.x <- x %>% 
  select(name)
#> Extract from main P1 table with merge
p1.x <- merge(RPA_GL_Int, pc.d.x, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = FALSE)
#> Select cols
p1.x <- p1.x %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Calculate total p1 payment
p1.x$P1_2020 <- p1.x$`Basic payment scheme` + p1.x$`Greening: practices beneficial for climate and environment`
#> Check sum first
sum.p1.2020.x <- sum(p1.x$P1_2020)
#> Group and summarise
p1.x <- p1.x %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2020_Unadjusted = sum(P1_2020))
#> Merge with shapefile table to append area of agri land within pcode area
p1.x <- merge(p1.x, x, by.x = "PostcodePrefix_F202B", by.y = "name", all.x = TRUE)
#> Extract total agri area of postcode in district from main table
x.agri.tot <- p1 %>% 
  select(PostcodePrefix_F202B, Area_Agri_Tot)
#> Merge with district table
p1.x <- merge(p1.x, x.agri.tot, by = "PostcodePrefix_F202B", all.x = TRUE)
#> Round agri area
p1.x$Area_Agri <- round(p1.x$Area_Agri, digits = 2)
#> Round
p1.x$P1_2020_Unadjusted <- round(p1.x$P1_2020_Unadjusted, digits = 0)
#> Calcucate percentage of agricultural land within pc area - for final p1 calculations
p1.x$Pcent_Agri <-  p1.x$Area_Agri /p1.x$Area_Agri_Tot
#> Round
p1$Pcent_Agri <- round(p1$Pcent_Agri, digits = 2)
#> Change "inf" values (caused by dividing by 0 above) to zero
p1$Pcent_Agri[!is.finite(p1$Pcent_Agri)] <- 0
#> Move 2020 figures to last column
p1.x <- p1.x  %>% 
  relocate(P1_2020_Unadjusted, .after = last_col())
#> Calculate P1 payment based on area of agri land within each postcode area
p1.x$P1_2020 <- p1.x$P1_2020_Unadjusted * p1.x$Pcent_Agri
#> Round 
p1.x$P1_2020 <- round(p1.x$P1_2020, digits = 0)
#> Check sum
sum.p1.x.2020 <- sum(p1.x$P1_2020)





#> 4.1 P1 2021 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2021 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2021$P1_2020 <- x2021$`Basic payment scheme` + x2021$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2021 reductions
x2021 <- x2021 %>% 
  mutate(P1_2021 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.95,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.90,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.80,
                             P1_2020 > 150000 ~ P1_2020 * 0.75))
# Group by postcode an summarise
x2021 <- x2021 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2021 = sum(P1_2021))
#> Round
x2021$P1_2021 <- round(x2021$P1_2021, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2021, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2021 <- p1.x$P1_2021 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2021 <- round(p1.x$P1_2021, digits = 0)
#> Check sum
sum.x2021 <- sum(p1.x$P1_2021)


#> 4.2 P1 2022 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2022 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2022$P1_2020 <- x2022$`Basic payment scheme` + x2022$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2022 reductions
x2022 <- x2022 %>% 
  mutate(P1_2022 = case_when(P1_2020<=30000 ~ P1_2020* 0.80,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.75,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.65,
                             P1_2020> 150000 ~ P1_2020* 0.60))
# Group by postcode an summarise
x2022 <- x2022 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2022 = sum(P1_2022))
#> Round
x2022$P1_2022 <- round(x2022$P1_2022, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2022, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2022 <- p1.x$P1_2022 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2022 <- round(p1.x$P1_2022, digits = 0)
#> Check sum
sum.x2022 <- sum(p1.x$P1_2022)


#> 4.3 P1 2023 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2023 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2023$P1_2020 <- x2023$`Basic payment scheme` + x2023$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2023 reductions
x2023 <- x2023 %>% 
  mutate(P1_2023 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.65,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.60,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.50,
                             P1_2020 > 150000 ~ P1_2020 * 0.45))
# Group by postcode an summarise
x2023 <- x2023 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2023 = sum(P1_2023))
#> Round
x2023$P1_2023 <- round(x2023$P1_2023, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2023, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2023 <- p1.x$P1_2023 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2023 <- round(p1.x$P1_2023, digits = 0)
#> Check sum
sum.x2023 <- sum(p1.x$P1_2023)


#> 4.4 P1 2024 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2024 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2024$P1_2020 <- x2024$`Basic payment scheme` + x2024$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2024 reductions
x2024 <- x2024 %>% 
  mutate(P1_2024 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.50,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.45,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.35,
                             P1_2020 > 150000 ~ P1_2020 * 0.30))
# Group by postcode an summarise
x2024 <- x2024 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2024 = sum(P1_2024))
#> Round
x2024$P1_2024 <- round(x2024$P1_2024, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2024, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2024 <- p1.x$P1_2024 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2024 <- round(p1.x$P1_2024, digits = 0)
#> Check sum
sum.x2024 <- sum(p1.x$P1_2024)


#> 4.5 P1 2025 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2025 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2025$P1_2020 <- x2025$`Basic payment scheme` + x2025$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2025 reductions
x2025 <- x2025 %>% 
  mutate(P1_2025 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.40,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.35,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.25,
                             P1_2020 > 150000 ~ P1_2020 * 0.20))
# Group by postcode an summarise
x2025 <- x2025 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2025 = sum(P1_2025))
#> Round
x2025$P1_2025 <- round(x2025$P1_2025, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2025, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2025 <- p1.x$P1_2025 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2025 <- round(p1.x$P1_2025, digits = 0)
#> Check sum
sum.x2025 <- sum(p1.x$P1_2025)


#> 4.6 P1 2026 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2026 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2026$P1_2020 <- x2026$`Basic payment scheme` + x2026$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2026 reductions
x2026 <- x2026 %>% 
  mutate(P1_2026 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.25,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.25,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.15,
                             P1_2020 > 150000 ~ P1_2020 * 0.15))
# Group by postcode an summarise
x2026 <- x2026 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2026 = sum(P1_2026))
#> Round
x2026$P1_2026 <- round(x2026$P1_2026, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2026, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2026 <- p1.x$P1_2026 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2026 <- round(p1.x$P1_2026, digits = 0)
#> Check sum
sum.x2026 <- sum(p1.x$P1_2026)


#> 4.7 P1 2027 ####----
#> Need to work out reductions on a farm-by-farm basis, so need to return to original RPA data to calculate
x2027 <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`)
#> Column to hold total total P1 payments (i.e. P1 + Greening)
x2027$P1_2020 <- x2027$`Basic payment scheme` + x2027$`Greening: practices beneficial for climate and environment`
#> Add new column showing 2027 reductions
x2027 <- x2027 %>% 
  mutate(P1_2027 = case_when(P1_2020<=30000 ~ P1_2020* 0.15,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.15,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.10,
                             P1_2020> 150000 ~ P1_2020* 0.10))
# Group by postcode an summarise
x2027 <- x2027 %>% 
  group_by(PostcodePrefix_F202B) %>% 
  summarise(P1_2027 = sum(P1_2027))
#> Round
x2027$P1_2027 <- round(x2027$P1_2027, digits = 0)
# merge with main P1 results table (m.)
p1.x <- merge(p1.x, x2027, by.x = "PostcodePrefix_F202B", by.y = "PostcodePrefix_F202B", all.x = TRUE)
# Change values according to agi land area proportion
p1.x$P1_2027 <- p1.x$P1_2027 * p1.x$Pcent_Agri
#> Round
p1.x$P1_2027 <- round(p1.x$P1_2027, digits = 0)
#> Check sum
sum.x2027 <- sum(p1.x$P1_2027)



#EXPORT FINAL P1 districts Tables

# write_csv(p1, here("Out", "1_UPDATED", "P1", "Districts", "Chelt_Reductions.csv"))


write_csv(p1.x, here("Out", "1_UPDATED", "P1", "Districts", paste0("P1_", districts[i], "_Reductions.csv")))

}



#### 5. FARM AREA CALCS ####----
#> Calculate approx area of farm using P1 payments

raw <- RPA_GL_Int %>% 
  select(PostcodePrefix_F202B, `Basic payment scheme`, `Greening: practices beneficial for climate and environment`) %>% 
  
  mutate(BPS_acres = `Basic payment scheme` / 162.77) %>% 
  mutate(Greening_acres = `Greening: practices beneficial for climate and environment` / 70.445) %>% 
  mutate(Tot_Acres = BPS_acres + Greening_acres) %>% 
  mutate(P1_2020 = `Basic payment scheme` + `Greening: practices beneficial for climate and environment`) 
#> Add ID col
raw$Farm_UID<- 1:nrow(raw)
#> Move ID to front
raw <- raw%>%
  select(Farm_UID, everything())

#> Reduction columns
#> 2021
raw  <- raw %>% 
  mutate(P1_2021 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.95,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.90,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.80,
                             P1_2020 > 150000 ~ P1_2020 * 0.75))
#> 2022
raw  <- raw %>% 
  mutate(P1_2022 = case_when(P1_2020<=30000 ~ P1_2020* 0.80,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.75,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.65,
                             P1_2020> 150000 ~ P1_2020* 0.60))
#> 2023
raw  <- raw %>% 
  mutate(P1_2023 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.65,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.60,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.50,
                             P1_2020 > 150000 ~ P1_2020 * 0.45))
#> 2024
raw  <- raw %>% 
  mutate(P1_2024 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.50,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.45,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.35,
                             P1_2020 > 150000 ~ P1_2020 * 0.30))
#> 2025
raw  <- raw %>% 
  mutate(P1_2025 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.40,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.35,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.25,
                             P1_2020 > 150000 ~ P1_2020 * 0.20))
#> 2026
raw  <- raw %>% 
  mutate(P1_2026 = case_when(P1_2020 <=30000 ~ P1_2020 * 0.25,
                             (P1_2020 >30000 & P1_2020 <=50000) ~ P1_2020 * 0.25,
                             (P1_2020 >50000 & P1_2020 <=150000) ~ P1_2020 * 0.15,
                             P1_2020 > 150000 ~ P1_2020 * 0.15))
#> 2027
raw  <- raw %>% 
  mutate(P1_2027 = case_when(P1_2020<=30000 ~ P1_2020* 0.15,
                             (P1_2020>30000 & P1_2020<=50000) ~ P1_2020* 0.15,
                             (P1_2020>50000 & P1_2020<=150000) ~ P1_2020* 0.10,
                             P1_2020> 150000 ~ P1_2020* 0.10))

#Export
write_csv(raw, here("Out", "1_UPDATED", "P1", "P1_Reductions_by_Farm_with_Farm_Size.csv"))


