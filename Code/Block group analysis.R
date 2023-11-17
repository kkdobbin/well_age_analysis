#Block group analysis

#Load libraries
library(tidyverse)

#load data
LONG <- readRDS("Data/drystats_by_bg_LONG.rds")
WIDE <- readRDS("Data/drystats_by_bg_WIDE.rds")

#1994 versus 1952 assumption

#filter dataset and remove NAs (indicates there is no wells for that Block Group)
Fiftytwo <- WIDE
Fiftytwo <- Fiftytwo[ ,c(1,19,22,24,27)]
Fiftytwo <- na.omit(Fiftytwo)

#make DV for change in percent impacted 

Fiftytwo$change.impacted.1994.1952 <- (Fiftytwo$perc_impacted_1952- Fiftytwo$perc_impacted_1994)
summary(Fiftytwo$change.impacted.1994.1952)
  
Fiftytwo$change.fullydew.1994.1952 <- (Fiftytwo$perc_fullydew_1952 - Fiftytwo$perc_fullydew_1994)


#add census
library(tidycensus)

#get census data
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

Fiftytwo <- left_join(Fiftytwo, Demographics_blockg, by = c("GEOID20" = "GEOID"))

ed_sum <- ed_sum[,-c(2:6,8)]
Fiftytwo <- left_join(Fiftytwo, ed_sum, by = c("GEOID20" = "GEOID"))


#assess multicolinearity
Vars <- Fiftytwo[,c(9, 22:27, 29, 31)]
Vars <- na.omit(Vars)
Varscor <- cor(x = Vars)

library(psych)
Varsval <- corr.test(Vars, adjust = "none")$p

library(corrplot)
corrplot(Varscor, type = "upper", diag = T, p.mat = Varsval, insig = "p-value", sig.level = 0)

#see what a combined model looks like
Combinedregression <- lm(change.impacted.1994.1952 ~ Median.hh.income + pct.hispanicorlatino + pct.blackalone + pct.asianalone + pct.renter + pct_no_hs, data = Fiftytwo)
summary(Combinedregression)

#is it different for fully dew?
Combinedregression2 <- lm(change.fullydew.1994.1952 ~ Median.hh.income + pct.hispanicorlatino + pct.blackalone + pct.asianalone + pct.renter + pct_no_hs, data = Fiftytwo)
summary(Combinedregression2)

#pairwise regressions
Pair1 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$Median.hh.income)
summary(Pair1)

Pair2 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$pct.hispanicorlatino)
summary(Pair2)

Pair3 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$pct.whitealone)
summary(Pair3)

Pair4 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$pct.blackalone)
summary(Pair4)

Pair5 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$pct.renter)
summary(Pair5)

Pair6 <- lm(Fiftytwo$change.impacted.1994.1952 ~ Fiftytwo$pct_no_hs)
summary(Pair6)

#pairwise regressions with fully dew
Pair1 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$Median.hh.income)
summary(Pair1)

Pair2 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$pct.hispanicorlatino)
summary(Pair2)

Pair3 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$pct.whitealone)
summary(Pair3)

Pair4 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$pct.blackalone)
summary(Pair4)

Pair5 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$pct.renter)
summary(Pair5)

Pair6 <- lm(Fiftytwo$change.fullydew.1994.1952 ~ Fiftytwo$pct_no_hs)
summary(Pair6)





#Next steps
##Think about the DV. is there a better way. What about relative change? 
##think if there needs to be any controls
##think about implications of removing NAs. 
##pairwise regression with all the SES variables? 
#what descriptive stats can go along with this analysis? 
#what figures? maybe map of change or relative change (Darcy?) like the EJ tree canopy paper has
#do same analysis for a more moderate retirement age assumption? 


