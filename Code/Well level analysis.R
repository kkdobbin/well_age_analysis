#well level analysis

#Load libraries
library(tidyverse)
library(tidycensus)

#Load data
Wells <- readRDS("Data/dryWells_long.rds")

#Reduce data frame to only needed variables
Wells <- Wells[,c(1,3:5,8:9,19:24,30,42)]

#get census data
#census_api_key("84059d7faf015832a99ef159379684476b2ec4a7", overwrite = TRUE, install = TRUE)
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

Demographics_blockg <- get_acs(geography = "block group", variables = c("B19013_001E", "B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_007E", "B03002_012E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2020, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% rename(Median.hh.income = B19013_001E, Race.estimate.total = B03002_001E, Race.white.alone = B03002_003E, Race.black.alone = B03002_004E, Race.native.alone = B03002_005E, Race.asian.alone = B03002_006E, Race.PI.alone = B03002_007E, Race.hispanicorlatino = B03002_012E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

#Get rid of MOE columns
Demographics_blockg <- Demographics_blockg[,-c(4,6,8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 27)]

#make percentages for block group SES variables
Demographics_blockg$pct.whitealone <- (Demographics_blockg$Race.white.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.hispanicorlatino <- (Demographics_blockg$Race.hispanicorlatino)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.blackalone <- (Demographics_blockg$Race.black.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.nativealone <- (Demographics_blockg$Race.native.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.asianalone <- (Demographics_blockg$Race.asian.alone)/ (Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.PIalone <- (Demographics_blockg$Race.PI.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.owner <- (Demographics_blockg$Tenure.owner)/(Demographics_blockg$Tenure.estimate.total)

Demographics_blockg$pct.renter <- (Demographics_blockg$Tenure.renter)/(Demographics_blockg$Tenure.estimate.total)

#create education variable (requires aggregating multiple census variables)
##Get percent with HS diploma over age 25
ca_ed <- get_acs(
  geography = "block group",
  variables = paste0("B15003_0", 17:25),  # hs diploma and above variables
  summary_var = "B15003_001",             # pop 25 years and older - denominator
  state = "CA"
)

ed_sum <- ca_ed %>% 
  group_by(GEOID, NAME) %>% 
  summarize(
    n_hs_above = sum(estimate),
    n_hs_above_moe = moe_sum(moe, estimate),
    n_pop_over_25 = summary_est[1],
    n_pop_over_25_moe = summary_moe[1]
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_hs_above = n_hs_above / n_pop_over_25,
    pct_hs_above_moe = moe_prop(n_pop_over_25, n_pop_over_25,
                                n_hs_above_moe, n_pop_over_25_moe)
  )

#calculate percent without HS diploma over 25
ed_sum$pct_no_hs <- ((1) - (ed_sum$pct_hs_above))


# Use GEOID20 to join with ACS five year estimates for SES variables

#make not a spatial object
Demographics_blockg <- as.data.frame(Demographics_blockg)
Wells <- as.data.frame(Wells)

Wells <- left_join(Wells, Demographics_blockg, by = c("GEOID20" = "GEOID"))

ed_sum <- ed_sum[,-c(2:6,8)]
Wells <- left_join(Wells, ed_sum, by = c("GEOID20" = "GEOID"))


#make summary tables to compare dry and active wells
Wells$YEAR <- as.factor(Wells$YEAR)
Wells$tcddry <- as.factor(Wells$tcddry)

#Summarize each variable for dry and active wells for each year scenario (so looks like a table with 1994, 1972 and 1952 in columns, rows for each variable with percents in the cells. Could add in whole region as a comparison point by summarizing each variable for all wells)

Table <- Wells %>% group_by(YEAR, tcddry) %>% summarise("Mean MHI" = mean(na.omit(Median.hh.income)), "Mean % white" = mean(na.omit(pct.whitealone)), "Mean % Black" = mean(na.omit(pct.blackalone)), "Mean % Native" = mean(na.omit(pct.nativealone)), "Mean % Asian" = mean(na.omit(pct.asianalone)), "Mean % PI" = mean(na.omit(pct.PIalone)), "Mean % renter" = mean(na.omit(pct.renter)), "Mean % no highschool diploma" = mean(na.omit(pct_no_hs)))

tmp <- Wells %>% 
  group_by(YEAR) %>% 
  summarise("Mean MHI" = mean(na.omit(Median.hh.income)), 
            "Mean % white" = mean(na.omit(pct.whitealone)), 
            "Mean % Black" = mean(na.omit(pct.blackalone)), 
            "Mean % Native" = mean(na.omit(pct.nativealone)), 
            "Mean % Asian" = mean(na.omit(pct.asianalone)), 
            "Mean % PI" = mean(na.omit(pct.PIalone)), 
            "Mean % renter" = mean(na.omit(pct.renter)), 
            "Mean % no highschool diploma" = mean(na.omit(pct_no_hs))) %>%
  mutate(tcddry = "All") %>%
  rbind(Table)



#Still interested in doing significance tests but maybe stop year and do the block group analysis first because this is so imprecise I'm not even sure these descriptive tell us anything? 

#How to add in an all wells column here and how to format this table so it is more intuitive?


#Exploration of associations and significance
#significance tests between (difference of means, chi-square?)

###DOODLING  

#association between well age and depth?
#create well age variable
Wells$wellage <- 2020 - (Wells$year)

Test <- lm(Wells$TOTALCOMPLETEDDEPTH ~ Wells$wellage)
summary(Test) #yes there is an association. Can confirm this with Rich's writ eup and deep dive on OSCWR database

#What about between well age and SES?
Test2 <- lm(Wells$Median.hh.income ~ Wells$wellage)
summary(Test2) #interesting there is a positive association here but this aligns with what the summary table shows. 


Test3 <- lm(Wells$pct_no_hs ~ Wells$wellage)
summary(Test3)

#Dry column has active (at MTs) TCDdry and topdry as impacts for each row which is a well for a specific year