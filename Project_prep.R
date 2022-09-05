#getwd()
setwd("C:/Users/kelly/OneDrive/Documents/Stats with R Certificate/Quarter 3/Homework/Final Project")

library(dplyr)
library(tidyr)
library(readr)

#import data
fulldata = read_csv("HTS Trip Level.csv")


#include variables I want for this analysis.
data = subset(fulldata, select = c(person_dim_id, trip_id, hhid, survey_year, daynum, mode_simple, mode_freq_1, dest_purpose_cat, 
                                   origin_purpose_cat, age, gender, employment, education, o_puma10, d_puma10, trip_path_distance, 
                                   benefits_3, final_home_puma10, vehicle_count, hhincome_broad, license, travelers_total, seattle_home,
                                   google_duration, speed_mph
                                   ))

 

#remove bad data and unused levels.

table(data$employment)
data = data[data$gender != "45-54 years",]
#data$employment = droplevels(data$employment, "18-64 years")



table(data$license)

#check for missing data not reported as NA
table(data$mode_simple)
table(data$mode_freq_1) #some missing, but unclear why there was skip logic.
table(data$origin_purpose_cat)
table(data$dest_purpose_cat)
table(data$benefits_3) #only if employed full or PT. Will Recode so that skipped are not employed because this is still useful info. 


#recode mode_freq_1
data$mode_freq_1 = replace(data$mode_freq_1, data$mode_freq_1 == "I do this, but not in the past 30 days", "Less than monthly") #only works with characters

#recode benefits_3 to say if transit benefits are offered
data$transit_benefits = dplyr::recode(data$benefits_3,
                              `I don't know` =0, `Missing: Skip Logic`=0, `Not offered`=0, `Offered, and I use` =1, 
                              `Offered, but I don't use`=1, `Missing: Non-response` = 0,  `Missing: Skip logic` = 0) 

data$transit_benefits = as.factor(data$transit_benefits)

table(data$transit_benefits)

#recode PUMA names if I'm going to use PUMAs
#check PUMA names
table(data$o_puma10)

#strip 53 from beginning of final_home_puma10 to match other PUMA formats
data$final_home_puma10 = sub('..', '', data$final_home_puma10)
table(data$final_home_puma10)

#recode PUMA names
data$final_home_puma10 = dplyr::recode(data$final_home_puma10,
                              `11501` ="Central Tacoma", `11502`="West Tacoma", `11503`="Lakewood & JBLM", `11504` ="South Tacoma", 
                              `11505`="East Tacoma & Bonney Lake", `11506`="Puyallup & South Hill", `11507`="SE Pierce", `11601`="NW Seattle", 
                              `11602`="NE Seattle", `11603`="Central Seattle", `11604`="SE Seattle", `11605`="Duwamish", `11606`="Shoreline & Kenmore", 
                              `11607`="Redmond & Kirkland", `11608`="Bellevue", `11609`="Issaquah", 
                              `11610`="Renton", `11611`="Burien & Tukwila", `11612`="Federal Way & Vashon", `11613`="Kent", `11614`="Auburn", 
                              `11615`="Maple Valley", `11616`="East Redmond", `11701`="Lynnwood", 
                              `11702`="Mukilteo", `11703`="Everett", `11704`="Mill Creek", `11705`="Monroe", `11706`="Marysville", 
                              `11801`="Bainbridge & Silverdale", `11802`="Bremerton", `.default` = "Other") 

data$o_puma10 = dplyr::recode(data$o_puma10,
    `11501` ="Central Tacoma", `11502`="West Tacoma", `11503`="Lakewood & JBLM", `11504` ="South Tacoma", 
    `11505`="East Tacoma & Bonney Lake", `11506`="Puyallup & South Hill", `11507`="SE Pierce", `11601`="NW Seattle", 
    `11602`="NE Seattle", `11603`="Central Seattle", `11604`="SE Seattle", `11605`="Duwamish", `11606`="Shoreline & Kenmore", 
    `11607`="Redmond & Kirkland", `11608`="Bellevue", `11609`="Issaquah", 
    `11610`="Renton", `11611`="Burien & Tukwila", `11612`="Federal Way & Vashon", `11613`="Kent", `11614`="Auburn", 
    `11615`="Maple Valley", `11616`="East Redmond", `11701`="Lynnwood", 
    `11702`="Mukilteo", `11703`="Everett", `11704`="Mill Creek", `11705`="Monroe", `11706`="Marysville", 
    `11801`="Bainbridge & Silverdale", `11802`="Bremerton") 

data$d_puma10 = dplyr::recode(data$d_puma10,
                              `11501` ="Central Tacoma", `11502`="West Tacoma", `11503`="Lakewood & JBLM", `11504` ="South Tacoma", 
                              `11505`="East Tacoma & Bonney Lake", `11506`="Puyallup & South Hill", `11507`="SE Pierce", `11601`="NW Seattle", 
                              `11602`="NE Seattle", `11603`="Central Seattle", `11604`="SE Seattle", `11605`="Duwamish", `11606`="Shoreline & Kenmore", 
                              `11607`="Redmond & Kirkland", `11608`="Bellevue", `11609`="Issaquah", 
                              `11610`="Renton", `11611`="Burien & Tukwila", `11612`="Federal Way & Vashon", `11613`="Kent", `11614`="Auburn", 
                              `11615`="Maple Valley", `11616`="East Redmond", `11701`="Lynnwood", 
                              `11702`="Mukilteo", `11703`="Everett", `11704`="Mill Creek", `11705`="Monroe", `11706`="Marysville", 
                              `11801`="Bainbridge & Silverdale", `11802`="Bremerton") 



#recode employment status
data$employment = dplyr::recode(data$employment,
                              `Employed but not currently working (e.g., on leave, furloughed 100%)` ="Not employed", `Employed part time (fewer than 35 hours/week, paid)`="Employed",
                              `Employed full time (35+ hours/week, paid)` = "Employed", `Missing: Skip Logic` = "Child", `Not currently employed` = "Not employed", `Retired` = "Not employed", 
                              `Self-employed` = "Self-employed", `Unpaid volunteer or intern` = "Employed") 

table(data$license)

#recode seattle_home
data$seattle_home = ifelse(data$seattle_home=="Home in Seattle",1,0)


#change characters to factors
library(magrittr)
cols <- c("mode_simple", "mode_freq_1", "dest_purpose_cat", "origin_purpose_cat", "final_home_puma10", 
"o_puma10", "d_puma10", "age", "employment", "gender", 'vehicle_count', 'hhincome_broad')

data %<>%
  mutate_at(cols, factor)

names(data)

#change vehicle to numeric
data$vehicle_count = dplyr::recode(data$vehicle_count,
                                          `0 (no vehicles)` = 0, `1` = 1, `1 vehicle` = 1, `10 or more vehicles` = 10, `2` = 2, 
                                          `2 vehicles` = 2, `3` = 3, `3 vehicles` = 3, `4` = 4, `4 vehicles` = 4, `5` = 5, `5 vehicles` = 5, `6` = 6, 
                                          `6 vehicles` = 6, `7` = 7, `7 vehicles` = 7, `8` = 8, `8 vehicles` = 8, `9` = 9)

#create a separate object with trips per person, averaged for # of days in study. 

trips_pp <- data %>% group_by(person_dim_id)  %>%
  summarize( trips=n(), days = max(daynum), daily_trips = trips/days)

head(trips_pp)
summary(trips_pp)



#basic stats. 
mean_daily_trips = summary(trips_pp$daily_trips)[4]
sd_daily_trips = sd(trips_pp$daily_trips, na.rm = TRUE)

tab = table(data$mode_simple)
mode_split = prop.table(tab)
drive_pct = mode_split["Drive"]*100

#join daily_trips to main data to prep to make predictions. 
data = left_join(data, trips_pp, by = "person_dim_id")

#what about missing data?
No_dist = data[is.na(data$trip_path_distance)==TRUE,]
No_dist
sum(is.na(data$trip_path_distance)) #50 trips missing distance
sum(is.na(data$final_home_puma10))

data[is.na(data$o_puma10)==TRUE,]
sum(is.na(data$o_puma10))
sum(is.na(data$d_puma10))
sum(is.na(data$trip_path_distance))

table(data$employment)

#relevel variables to help with models
data$age = relevel(data$age, ref = "25-34 years") #chose this age because it's a large group and probably more reliable reporters of trips than younger ages.
data$employment = relevel(data$employment, ref = "Employed")



#create tree_data df for decision tree on mode choice. 
#clean data
tree_data = data[!is.na(data$mode_simple),]
tree_data = tree_data[!is.na(tree_data$trip_path_distance),]
tree_data = tree_data[(tree_data$mode_simple != "Home"),]
tree_data$mode_simple = droplevels(tree_data$mode_simple)
tree_data = tree_data[!is.na(tree_data$o_puma10),]
tree_data = tree_data[!is.na(tree_data$d_puma10),]

sum(!complete.cases(tree_data))

tree_data$license = as.factor(tree_data$license)

summary(tree_data$mode_simple)



#create binary variable to help undersample the drive trips to re-try cluster algorithm. 

tree_data$drive = ifelse(tree_data$mode_simple == "Drive", 1, 0)

#clean NAs from license.
tree_data$license = dplyr::recode(tree_data$license, 
                                  `Yes, has a learners permit` = "Permit",          `No, does not have a license or permit` = "None", 
                                  `Missing: Skip logic` = "Child", `Missing: Skip Logic` = "Child", `Yes, has an intermediate or unrestricted license` = "License")

#now isolate vars about the trip itself. 

trip_data = (tree_data %>% dplyr::select(c(o_puma10, d_puma10, trip_path_distance, final_home_puma10, google_duration, speed_mph))) 


#isolate vars relevant to a decision tree about travel choices that influence mode choice. This code removes irrelevant vars. 
choice_data = (tree_data %>% dplyr::select(-c(survey_year, daynum, trips, days, trip_id, person_dim_id, hhid, mode_freq_1, speed_mph, google_duration, benefits_3))) #add more here
choice_data$education = as.factor(choice_data$education)

  
mode_cluster = (choice_data %>% dplyr::select(-c(mode_simple, drive))) #remove target var for classification. must specify package for select function because MASS is also loaded.


#create new df for supervised clustering of car_share variable

carshare_data = fulldata %>% select(c(car_share, hhincome_detailed, seattle_home, final_home_uvnum, vehicle_count))

#recode carshare_data variables to numeric for kmeans
carshare_data$final_home_uvnum = dplyr::recode(carshare_data$final_home_uvnum,
                                              `(None)` = "No UV",                        `Madison-Miller`  = "Residential",                   `Pike/Pine` = "Urban Center",        `Mt Baker` = "Hub",                
                                              `First Hill` = "Urban Center",                        `Belltown` = "Urban Center",                        `Roosevelt`  = "Residential",                       
                                              `Fremont`= "Hub",                          `South Lake Union` = "Urban Center",                 `Capitol Hill` = "Urban Center",                    
                                              `Northgate` = "Urban Center",           `Ballard-Interbay-Northend` = "Industrial",       `Lake City` = "Hub",                      
                                              `University District Northwest` = "Urban Center",    `Uptown` = "Urban Center",                           `Commercial Core` = "Urban Center",                 
                                              `23rd & Union-Jackson` = "Residential",            `Greater Duwamish` = "Industrial",                `Eastlake` = "Residential",                         
                                              `Wallingford` = "Residential",                       `Admiral` = "Residential",                           `Bitter Lake Village` = "Hub",            
                                              `Green Lake` = "Residential",                        `Upper Queen Anne`  = "Residential",                 `12th Avenue` = "Urban Center",                     
                                              `Chinatown-International District`= "Urban Center",  `Crown Hill`  = "Residential",                       `Ravenna`  = "Residential",                         
                                              `Denny Triangle`= "Urban Center",                   `Greenwood-Phinney Ridge`  = "Residential",          `Columbia City` = "Residential",                  
                                              `Ballard` = "Hub",                          `Pioneer Square` = "Urban Center",                 `West Seattle Junction`= "Hub",          
                                              `Aurora-Licton Springs` = "Residential",             `North Beacon Hill` = "Residential",                 `North Rainier` = "Hub",                  
                                              `South Park`= "Industrial",                       `University Campus`= "Urban Center",                 `Othello`  = "Residential",                         
                                              `Rainier Beach`  = "Residential",                    `Morgan Junction`  = "Residential",                  `Westwood-Highland Park` = "Residential")   

carshare_data$final_home_uvnum = replace_na(carshare_data$final_home_uvnum,"No UV")

carshare_data$urban_center = dplyr::recode(carshare_data$final_home_uvnum, 
                                         `Hub` = 0, `Industrial` = 0, `No UV` = 0, `Residential` = 0, `Urban Center` = 1)

carshare_data$seattle_home = ifelse(carshare_data$seattle_home == "Home in Seattle",1,0)

carshare_data$vehicle_count = dplyr::recode(carshare_data$vehicle_count,
                                   `0 (no vehicles)` = 0, `1` = 1, `1 vehicle` = 1, `10 or more vehicles` = 10, `2` = 2, 
                                   `2 vehicles` = 2, `3` = 3, `3 vehicles` = 3, `4` = 4, `4 vehicles` = 4, `5` = 5, `5 vehicles` = 5, `6` = 6, 
                                   `6 vehicles` = 6, `7` = 7, `7 vehicles` = 7, `8` = 8, `8 vehicles` = 8, `9` = 9)


#add continuous income variable using midpoint using distribution as a guide. Make midpoints off center to reflect where the mean within the group is likely to fall. 
carshare_data$hhincome_num = carshare_data$hhincome_detailed #first make a copy to modify
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "Under $10,000", 8000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$10,000-$24,999", 19000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$25,000-$34,999", 32000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$35,000-$49,999", 45000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$50,000-$74,999", 60000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$75,000-$99,999", 85000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$100,000-$149,999", 120000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$150,000-$199,999", 170000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$200,000-$249,999", 220000)
carshare_data$hhincome_num = replace(carshare_data$hhincome_num, carshare_data$hhincome_num == "$250,000 or more", 260000)



carshare_data = carshare_data[carshare_data$hhincome_detailed != "Prefer not to answer",]
carshare_data$hhincome_num = as.numeric(carshare_data$hhincome_num)

carshare_data$car_share = as.factor(carshare_data$car_share)
carshare_data = (carshare_data %>% select(-c(hhincome_detailed, final_home_uvnum)))

#prepare data for neural net

data$employed = ifelse(data$employment=="Employed", 1, 0)
data$homemaker = ifelse(data$employment == "Homemaker",1,0)
data$female = ifelse(data$gender == "Female",1,0)

#make a numeric age variable
data$age_numeric = dplyr::recode(data$age,
                                      `25-34 years` =29.5, `12-15 years`=13.5, `16-17 years`=16.5, `18-24 years` =21, 
                                      `35-44 years`=39.5, `45-54 years` = 49.5, `5-11 years` = 8, `55-64 years` = 59.5, `65-74 years` = 69.5,
                                 `75-84 years` = 79.5, `85 or years older` = 85, `Under 5 years old` = 4) 

table(data$age_numeric)

#reorder ages to help plots
table(data$age)
ageorder = c("Under 5 years old", "5-11 years", "12-15 years", "16-17 years", "18-24 years", "25-34 years", "35-44 years", 
             "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 or years older")
data$age <- factor(data$age, levels = ageorder) 



save.image('Project_prep.RData') #save workspace


