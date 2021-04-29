#> libs
library(tidyverse)
library(sf)
library(here)
library(lubridate)
library(anytime)
library(miscTools)


#> Get data
ES <- st_read(here("In", "AES", "ES_Glos.shp"), stringsAsFactors = FALSE)

##### ENVIRONEMNTAL STEWARDSHIP #####
#### 1. GLOUCESTERSHIRE - ALL ####

# Create non geom versions for Glos-ALL calculations
ES.ng <- ES
st_geometry(ES.ng) <- NULL

#> Filter records to get ones that were live in 2018
#> Environmental stewardship (ES)
#> New start date field in date format
ES.ng$STARTDAT.2 <- as.Date(ES.ng$STARTDAT, format = "%d/%m/%Y") # correct lowercase y
#> New start date field in date format
ES.ng$ENDDATE.2 <- as.Date(ES.ng$ENDDATE, format = "%d/%m/%Y") # correct lowercase y
#> get agrrement that were live in 2018

#> Create column for length of agreement in years
ES.ng$Years <- as.numeric(ES.ng$ENDDATE.2 - ES.ng$STARTDAT.2) / 365
ES.ng$Years <- round(ES.ng$Years, 0)
ES.ng$TOTCOST <- as.numeric(ES.ng$TOTCOST)

#> Create column for average cost
ES.ng$AV.YEAR.COST <- ES.ng$TOTCOST / ES.ng$Years
#> Change NAs to Zero
ES.ng$AV.YEAR.COST[is.na(ES.ng$AV.YEAR.COST)] <- 0


#Export table
write_csv(ES.ng, here("Out", "AES", "ES", "ES_Glos_Raw-Processed.csv"))

#> Calcs for agreements live in 2018 - earlist end date is "2020-04-30" - so all records need to be 
live.2018 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(STARTDAT.2 <= "2017-12-31", ENDDATE.2 <= "2040-12-31")   
#> Sum Av cost for annual total for 2018
x.2018 <- sum(live.2018$AV.YEAR.COST)
x.2019 <- sum(live.2018$AV.YEAR.COST)  #2019 is the same - as same number of live agreements running throughout 2019-2018
#> Create new dataframe to hold this
totals <- c(x.2018, x.2019)
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0)))
#> Add figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)


#> Work out 2020 totals
#> Filter for agreements ending in 2020
end.2020 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(STARTDAT.2 <= "2017-12-31", ENDDATE.2 <= "2020-12-31")
#> Work out how mnay days in year that agreement running for
end.2020$YearStart <- as.Date("2020-01-01")
end.2020$Days <- as.numeric(end.2020$ENDDATE.2 - end.2020$YearStart)
#> Work out proipertion of year agreement is active
end.2020$Year_Prop <- end.2020$Days / 365
#> New column for cost of 2020 based on proportion
end.2020$Cost_Actual <- end.2020$Year_Prop * end.2020$AV.YEAR.COST
#> Total amount for agreements ending in 2020
tot.2020 <- sum(end.2020$Cost_Actual)
#> Subtract from 2018 baseline total to caclulate final total for 2020
x.2020 <- x.2018 - tot.2020
#> Update df
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0)))
totals <- c(x.2018, x.2019, x.2020)
#> AInsert figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)


#> Work out 2021 totals
#> Filter for agreements ending in 2021
end.2021 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(ENDDATE.2  >= "2021-01-01", ENDDATE.2 <= "2021-12-31")
#> Work out how mnay days in year that agreement running for
end.2021$YearStart <- as.Date("2021-01-01")
end.2021$Days <- as.numeric(end.2021$ENDDATE.2 - end.2021$YearStart)
#> Work out proipertion of year agreement is active
end.2021$Year_Prop <- end.2021$Days / 365
#> New column for cost of 2021 based on proportion
end.2021$Cost_Actual <- end.2021$Year_Prop * end.2021$AV.YEAR.COST
#> Total amount for agreements ending in 2021
tot.2021 <- sum(end.2021$Cost_Actual)
#> Subtract from 2018 baseline total to caclulate final total for 2021
x.2021  <- x.2018 - (tot.2021 + tot.2020)
#> Update df
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0), Total_2021 = numeric(0)))
totals <- c(x.2018, x.2019, x.2020, x.2021)
#> AInsert figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)



#> Work out 2022 totals
#> Filter for agreements ending in 2022
end.2022 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(ENDDATE.2  >= "2022-01-01", ENDDATE.2 <= "2022-12-31")
#> Work out how mnay days in year that agreement running for
end.2022$YearStart <- as.Date("2022-01-01")
end.2022$Days <- as.numeric(end.2022$ENDDATE.2 - end.2022$YearStart)
#> Work out proipertion of year agreement is active
end.2022$Year_Prop <- end.2022$Days / 365
#> New column for cost of 2021 based on proportion
end.2022$Cost_Actual <- end.2022$Year_Prop * end.2022$AV.YEAR.COST
#> Total amount for agreements ending in 2021
tot.2022 <- sum(end.2022$Cost_Actual)
#> Subtract from 2018 baseline total to caclulate final total for 2021
x.2022  <- x.2018 - (tot.2021 + tot.2020 + tot.2022)
#> Update df
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0), Total_2021 = numeric(0), Total_2022 = numeric(0)))
totals <- c(x.2018, x.2019, x.2020, x.2021, x.2022)
#> AInsert figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)



#> Work out 2023 totals
#> Filter for agreements ending in 2023
end.2023 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(ENDDATE.2  >= "2023-01-01", ENDDATE.2 <= "2023-12-31")
#> Work out how mnay days in year that agreement running for
end.2023$YearStart <- as.Date("2023-01-01")
end.2023$Days <- as.numeric(end.2023$ENDDATE.2 - end.2023$YearStart)
#> Work out proipertion of year agreement is active
end.2023$Year_Prop <- end.2023$Days / 365
#> New column for cost of 2021 based on proportion
end.2023$Cost_Actual <- end.2023$Year_Prop * end.2023$AV.YEAR.COST
#> Total amount for agreements ending in 2021
tot.2023 <- sum(end.2023$Cost_Actual)
#> Subtract from 2018 baseline total to caclulate final total for 2021
x.2023  <- x.2018 - (tot.2021 + tot.2020 + tot.2022 + tot.2023)
#> Update df
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0), Total_2021 = numeric(0), Total_2022 = numeric(0), Total_2023 = numeric(0)))
totals <- c(x.2018, x.2019, x.2020, x.2021, x.2022, x.2023)
#> AInsert figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)



#> Work out 2024 totals
#> Filter for agreements ending in 2024
end.2024 <- ES.ng %>%
  select(AGREF, TOTCOST, Years, AV.YEAR.COST, STARTDAT.2, ENDDATE.2, Years) %>% 
  filter(ENDDATE.2  >= "2024-01-01", ENDDATE.2 <= "2024-12-31")
#> Work out how mnay days in year that agreement running for
end.2024$YearStart <- as.Date("2024-01-01")
end.2024$Days <- as.numeric(end.2024$ENDDATE.2 - end.2024$YearStart)
#> Work out proipertion of year agreement is active
end.2024$Year_Prop <- end.2024$Days / 365
#> New column for cost of 2021 based on proportion
end.2024$Cost_Actual <- end.2024$Year_Prop * end.2024$AV.YEAR.COST
#> Total amount for agreements ending in 2021
tot.2024 <- sum(end.2024$Cost_Actual)
#> Subtract from 2018 baseline total to caclulate final total for 2021
x.2024  <- x.2018 - (tot.2021 + tot.2020 + tot.2022 + tot.2023 + tot.2024)
#> Update df
ES_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0), Total_2021 = numeric(0), Total_2022 = numeric(0), Total_2023 = numeric(0), Total_2024 = numeric(0)))
totals <- c(x.2018, x.2019, x.2020, x.2021, x.2022, x.2023, x.2024)
#> AInsert figure for 2018
df <- miscTools::insertRow(ES_Summary, 1, totals)
df <- as.data.frame(df)

# Export table as CSV
write_csv(df, here("Out", "AES", "ES", "ES_Glos.csv"))



##### COUNTRYSIDE STEWARDSHIP #####
#### 1. GLOUCESTERSHIRE - ALL ####



#> Get data
CS <- st_read(here("In", "AES", "CS_Glos.shp"), stringsAsFactors = FALSE)

# Create non geom versions for Glos-ALL calculations
CS.ng <- CS
st_geometry(CS.ng) <- NULL


#> New start date field in date format
CS.ng$STARTDATE.2 <- lubridate::dmy_hms(CS.ng$STARTDATE) 
#> New end date field in date format
CS.ng$ENDDATE.2 <- lubridate::dmy_hms(CS.ng$ENDDATE) 
#> Format dates
CS.ng$STARTDATE.2 <- as.Date(CS.ng$STARTDATE.2, format = "%d/%m/%Y")
CS.ng$ENDDATE.2 <- as.Date(CS.ng$ENDDATE.2, format = "%d/%m/%Y")

#> Create a colum to get number of years
CS.ng$Years <- as.numeric(CS.ng$ENDDATE.2 - CS.ng$STARTDATE.2)
CS.ng$Years <- CS.ng$Years /365
CS.ng$Years <- round(CS.ng$Years, 0)
#> List of agreement duractions in years - unique durations
years.unique <- unique(CS.ng$Years)


#> Delete unwanted cols from CS.ng to make it easier to work with
CS.ng <- CS.ng %>% 
  select(CSREF, STARTDATE.2, ENDDATE.2, AVGANNCOST, Years)

# #> Calcs for agreements live in 2018 - earlist end date is "2020-04-30" - so all records need to be 
# cs.2018 <- CS.ng %>%
#   select(STARTDATE.2, ENDDATE.2) %>% 
#   filter(STARTDATE.2 >= as.Date("2016-01-01") & ENDDATE.2 <= as.Date("2017-12-31"))  


#Create individual year columns to assign agreements to different years based on start date and duration - code for that to follow below
CS.ng$y.2016 <- 0
CS.ng$y.2017 <- 0 
CS.ng$y.2018 <- 0
CS.ng$y.2019 <- 0
CS.ng$y.2020 <- 0
CS.ng$y.2021 <- 0
CS.ng$y.2022 <- 0
CS.ng$y.2023 <- 0
CS.ng$y.2024 <- 0


#> Get unique combinations of agreements-years
unique.agree2 <- count(CS.ng, STARTDATE.2, Years) %>% 
  ungroup()
#> Export
write_csv(unique.agree2, (here("Out", "Unique_Agree-Year_for_checking.csv")))

#> 1. Agreements starting in 2016 running for 5 years 
cs.2016.5 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2016-01-01") & STARTDATE.2 <= as.Date("2016-12-31") & Years == 5)
#> Update relevant cols
cs.2016.5$y.2016 = 1
cs.2016.5$y.2017 = 1
cs.2016.5$y.2018 = 1
cs.2016.5$y.2019 = 1
cs.2016.5$y.2020 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2016[match(cs.2016.5$CSREF,  CS.ng$CSREF)] <- cs.2016.5$y.2016
CS.ng$y.2017[match(cs.2016.5$CSREF,  CS.ng$CSREF)] <- cs.2016.5$y.2017
CS.ng$y.2018[match(cs.2016.5$CSREF,  CS.ng$CSREF)] <- cs.2016.5$y.2018
CS.ng$y.2019[match(cs.2016.5$CSREF,  CS.ng$CSREF)] <- cs.2016.5$y.2019
CS.ng$y.2020[match(cs.2016.5$CSREF,  CS.ng$CSREF)] <- cs.2016.5$y.2020
  

#> 2. Agreements starting in 2016 running for 10 years
cs.2016.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2016-01-01") & STARTDATE.2 <= as.Date("2016-12-31") & Years == 10)
#> Update relevant cols
cs.2016.10$y.2016 = 1
cs.2016.10$y.2017 = 1
cs.2016.10$y.2018 = 1
cs.2016.10$y.2019 = 1
cs.2016.10$y.2020 = 1
cs.2016.10$y.2021 = 1
cs.2016.10$y.2022 = 1
cs.2016.10$y.2023 = 1
cs.2016.10$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2016[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2016
CS.ng$y.2017[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2017
CS.ng$y.2018[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2018
CS.ng$y.2019[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2019
CS.ng$y.2020[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2020
CS.ng$y.2021[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2021
CS.ng$y.2022[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2022
CS.ng$y.2023[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2023
CS.ng$y.2024[match(cs.2016.10$CSREF,  CS.ng$CSREF)] <- cs.2016.10$y.2024


#> 3. Agreements starting in 2017 running for 2 years 
cs.2017.2 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2017-01-01") & STARTDATE.2 <= as.Date("2017-12-31") & Years == 2)
#> Update relevant cols
cs.2017.2$y.2017 = 1
cs.2017.2$y.2018 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2017[match(cs.2017.2$CSREF,  CS.ng$CSREF)] <- cs.2017.2$y.2017
CS.ng$y.2018[match(cs.2017.2$CSREF,  CS.ng$CSREF)] <- cs.2017.2$y.2018



#> 4. Agreements starting in 2017 running for 5 years
cs.2017.5 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2017-01-01") & STARTDATE.2 <= as.Date("2017-12-31") & Years == 5)
#> Update relevant cols
cs.2017.5$y.2017 = 1
cs.2017.5$y.2018 = 1
cs.2017.5$y.2019 = 1
cs.2017.5$y.2020 = 1
cs.2017.5$y.2021 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2017[match(cs.2017.5$CSREF,  CS.ng$CSREF)] <- cs.2017.5$y.2017
CS.ng$y.2018[match(cs.2017.5$CSREF,  CS.ng$CSREF)] <- cs.2017.5$y.2018
CS.ng$y.2019[match(cs.2017.5$CSREF,  CS.ng$CSREF)] <- cs.2017.5$y.2019
CS.ng$y.2020[match(cs.2017.5$CSREF,  CS.ng$CSREF)] <- cs.2017.5$y.2020
CS.ng$y.2021[match(cs.2017.5$CSREF,  CS.ng$CSREF)] <- cs.2017.5$y.2021


#> 5. Agreements starting in 2017 running for 10 years
cs.2017.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2017-01-01") & STARTDATE.2 <= as.Date("2017-12-31") & Years == 10)
#> Update relevant cols
cs.2017.10$y.2017 = 1
cs.2017.10$y.2018 = 1
cs.2017.10$y.2019 = 1
cs.2017.10$y.2020 = 1
cs.2017.10$y.2021 = 1
cs.2017.10$y.2022 = 1
cs.2017.10$y.2023 = 1
cs.2017.10$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2017[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2017
CS.ng$y.2018[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2018
CS.ng$y.2019[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2019
CS.ng$y.2020[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2020
CS.ng$y.2021[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2021
CS.ng$y.2022[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2022
CS.ng$y.2023[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2023
CS.ng$y.2024[match(cs.2017.10$CSREF,  CS.ng$CSREF)] <- cs.2017.10$y.2024



#> 6. Agreements starting in 2018 running for 2 years 
cs.2018.2 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2018-01-01") & STARTDATE.2 <= as.Date("2018-12-31") & Years == 2)
#> Update relevant cols
cs.2018.2$y.2018 = 1
cs.2018.2$y.2019 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2018[match(cs.2018.2$CSREF,  CS.ng$CSREF)] <- cs.2018.2$y.2018
CS.ng$y.2019[match(cs.2018.2$CSREF,  CS.ng$CSREF)] <- cs.2018.2$y.2019


#> 7. Agreements starting in 2018 running for 5 years 
cs.2018.5 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2018-01-01") & STARTDATE.2 <= as.Date("2018-12-31") & Years == 5)
#> Update relevant cols
cs.2018.5$y.2018 = 1
cs.2018.5$y.2019 = 1
cs.2018.5$y.2020 = 1
cs.2018.5$y.2021 = 1
cs.2018.5$y.2022 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2018[match(cs.2018.5$CSREF,  CS.ng$CSREF)] <- cs.2018.5$y.2018
CS.ng$y.2019[match(cs.2018.5$CSREF,  CS.ng$CSREF)] <- cs.2018.5$y.2019
CS.ng$y.2020[match(cs.2018.5$CSREF,  CS.ng$CSREF)] <- cs.2018.5$y.2020
CS.ng$y.2021[match(cs.2018.5$CSREF,  CS.ng$CSREF)] <- cs.2018.5$y.2021
CS.ng$y.2022[match(cs.2018.5$CSREF,  CS.ng$CSREF)] <- cs.2018.5$y.2022


#> 8. Agreements starting in 2018 running for 10 years 
cs.2018.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2018-01-01") & STARTDATE.2 <= as.Date("2018-12-31") & Years == 10)
#> Update relevant cols
cs.2018.10$y.2018 = 1
cs.2018.10$y.2019 = 1
cs.2018.10$y.2020 = 1
cs.2018.10$y.2021 = 1
cs.2018.10$y.2022 = 1
cs.2018.10$y.2023 = 1
cs.2018.10$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2018[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2018
CS.ng$y.2019[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2019
CS.ng$y.2020[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2020
CS.ng$y.2021[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2021
CS.ng$y.2022[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2022
CS.ng$y.2023[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2023
CS.ng$y.2024[match(cs.2018.10$CSREF,  CS.ng$CSREF)] <- cs.2018.10$y.2024


#> 9. Agreements starting in 2019 running for 2 years 
cs.2019.2 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2019-01-01") & STARTDATE.2 <= as.Date("2019-12-31") & Years == 2)
#> Update relevant cols
cs.2019.2$y.2019 = 1
cs.2019.2$y.2020 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2019[match(cs.2019.2$CSREF,  CS.ng$CSREF)] <- cs.2019.2$y.2019
CS.ng$y.2020[match(cs.2019.2$CSREF,  CS.ng$CSREF)] <- cs.2019.2$y.2020

#> 10. Agreements starting in 2019 running for 5 years 
cs.2019.5 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2019-01-01") & STARTDATE.2 <= as.Date("2019-12-31") & Years == 5)
#> Update relevant cols
cs.2019.5$y.2019 = 1
cs.2019.5$y.2020 = 1
cs.2019.5$y.2021 = 1
cs.2019.5$y.2022 = 1
cs.2019.5$y.2023 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2019[match(cs.2019.5$CSREF,  CS.ng$CSREF)] <- cs.2019.5$y.2019
CS.ng$y.2020[match(cs.2019.5$CSREF,  CS.ng$CSREF)] <- cs.2019.5$y.2020
CS.ng$y.2021[match(cs.2019.5$CSREF,  CS.ng$CSREF)] <- cs.2019.5$y.2021
CS.ng$y.2022[match(cs.2019.5$CSREF,  CS.ng$CSREF)] <- cs.2019.5$y.2022
CS.ng$y.2023[match(cs.2019.5$CSREF,  CS.ng$CSREF)] <- cs.2019.5$y.2023


#> 11. Agreements starting in 2019 running for 10 years 
cs.2019.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2019-01-01") & STARTDATE.2 <= as.Date("2019-12-31") & Years == 10)
#> Update relevant cols
cs.2019.10$y.2019 = 1
cs.2019.10$y.2020 = 1
cs.2019.10$y.2021 = 1
cs.2019.10$y.2022 = 1
cs.2019.10$y.2023 = 1
cs.2019.10$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2019[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2019
CS.ng$y.2020[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2020
CS.ng$y.2021[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2021
CS.ng$y.2022[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2022
CS.ng$y.2023[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2023
CS.ng$y.2024[match(cs.2019.10$CSREF,  CS.ng$CSREF)] <- cs.2019.10$y.2024


#> 12. Agreements starting in 20 running for 2years 
cs.2020.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2020-01-01") & STARTDATE.2 <= as.Date("2020-12-31") & Years == 2)
#> Update relevant cols
cs.2020.10$y.2020 = 1
cs.2020.10$y.2021 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2020[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2020
CS.ng$y.2021[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2021


#> 12. Agreements starting in 20 running for 2years 
cs.2020.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2020-01-01") & STARTDATE.2 <= as.Date("2020-12-31") & Years == 2)
#> Update relevant cols
cs.2020.10$y.2020 = 1
cs.2020.10$y.2021 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2020[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2020
CS.ng$y.2021[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2021



#> 12. Agreements starting in 20 running for 5 years 
cs.2020.5 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2020-01-01") & STARTDATE.2 <= as.Date("2020-12-31") & Years == 5)
#> Update relevant cols
cs.2020.5$y.2020 = 1
cs.2020.5$y.2021 = 1
cs.2020.5$y.2022 = 1
cs.2020.5$y.2023 = 1
cs.2020.5$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2020[match(cs.2020.5$CSREF,  CS.ng$CSREF)] <- cs.2020.5$y.2020
CS.ng$y.2021[match(cs.2020.5$CSREF,  CS.ng$CSREF)] <- cs.2020.5$y.2021
CS.ng$y.2022[match(cs.2020.5$CSREF,  CS.ng$CSREF)] <- cs.2020.5$y.2022
CS.ng$y.2023[match(cs.2020.5$CSREF,  CS.ng$CSREF)] <- cs.2020.5$y.2023
CS.ng$y.2024[match(cs.2020.5$CSREF,  CS.ng$CSREF)] <- cs.2020.5$y.2024



#> 13. Agreements starting in 20 running for 10 years 
cs.2020.10 <- CS.ng %>%
  select(CSREF, STARTDATE.2, Years, y.2016, y.2017, y.2018, y.2019, y.2020, y.2021, y.2022, y.2023, y.2024) %>% 
  filter(STARTDATE.2 >= as.Date("2020-01-01") & STARTDATE.2 <= as.Date("2020-12-31") & Years == 10)
#> Update relevant cols
cs.2020.10$y.2020 = 1
cs.2020.10$y.2021 = 1
cs.2020.10$y.2022 = 1
cs.2020.10$y.2023 = 1
cs.2020.10$y.2024 = 1

# Update HER using "MATCH" (base R) on the imported lookup table
CS.ng$y.2020[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2020
CS.ng$y.2021[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2021
CS.ng$y.2022[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2022
CS.ng$y.2023[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2023
CS.ng$y.2024[match(cs.2020.10$CSREF,  CS.ng$CSREF)] <- cs.2020.10$y.2024


# Remove records where average annual cost id "unavailable"
CS.final <- CS.ng[!(CS.ng$AVGANNCOST=="*Unavailable"),]

# Export final raw data table
write_csv(CS.final, here("Out", "AES", "CS", "CS_Glos_Raw-Processed.csv"))

# Change annual cost column to numerical
CS.final$AVGANNCOST <- as.numeric(CS.final$AVGANNCOST)
CS.final$AVGANNCOST <- round(CS.final$AVGANNCOST, 2)


#> Calculate annual costs

#> Total for 2018
CS.2018.Total <- CS.final %>% 
  filter(y.2018 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2018 <- sum(CS.2018.Total$AVGANNCOST)

#> Total for 2019
CS.2019.Total <- CS.final %>% 
  filter(y.2019 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2019 <- sum(CS.2019.Total$AVGANNCOST)


#> Total for 2020
CS.2020.Total <- CS.final %>% 
  filter(y.2020 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2020 <- sum(CS.2020.Total$AVGANNCOST)


#> Total for 2021
CS.2021.Total <- CS.final %>% 
  filter(y.2021 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2021 <- sum(CS.2021.Total$AVGANNCOST)

#> Total for 2022
CS.2022.Total <- CS.final %>% 
  filter(y.2022 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2022 <- sum(CS.2022.Total$AVGANNCOST)

#> Total for 2023
CS.2023.Total <- CS.final %>% 
  filter(y.2023 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2023 <- sum(CS.2023.Total$AVGANNCOST)

#> Total for 2024
CS.2024.Total <- CS.final %>% 
  filter(y.2024 == 1) %>% 
  select(AVGANNCOST)
#> Variable to hold total sumemd value for year
CS.sum.2024 <- sum(CS.2024.Total$AVGANNCOST)


#> Create dataframe of annual costs
CS_Summary <- as.matrix(data.frame(Total_2018 = numeric(0), Total_2019 = numeric(0), Total_2020 = numeric(0), Total_2021 = numeric(0), Total_2022 = numeric(0), Total_2023 = numeric(0), Total_2024 = numeric(0)))
totals.cs <- c(CS.sum.2018, CS.sum.2019, CS.sum.2020, CS.sum.2021, CS.sum.2022, CS.sum.2023, CS.sum.2024)
#> AInsert figure for 2018
df2 <- miscTools::insertRow(CS_Summary, 1, totals.cs)
df2 <- as.data.frame(df2)

#Export table
write_csv(df2, here("Out", "AES", "CS", "CS_Glos.csv"))





