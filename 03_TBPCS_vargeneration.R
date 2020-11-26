setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- "C:/Users/Takuya"
  }
  base::setwd(dir)
}

setwd()

setwd("./Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data")

exclude <-  function(blah) {
  "excluded block"
}


# package 
library("dataMeta") # build_linker and build_dict functions to create data dictionary 
library("DataCombine") # FindRepalce function to replace data into english at once
library("ggplot2")
library("dplyr")
library("epiR")
library("htmlTable")
library("reshape2")
library("gridExtra")
library("xlsx")
library("data.table")


################################################################################################################
# Load cleaned data
################################################################################################################
load("./TBPCS_iso3_cleaned.RData")

################################################################################################################
# Inocme
################################################################################################################
# Checked if no income data and no asset data. > variable names for asset need to be changed based on the questionnaire for each country
# This example is based on TZA

all$no_income <- 0

all$no_income <- ifelse(is.na(all$income_hh_pre) & is.na(all$electricity) &  #is.na(all$solar) &
                        is.na(all$radio) & is.na(all$tv) & is.na(all$mobile) & is.na(all$fridge) &
                        #is.na(all$computer) & 
                          is.na(all$bicycle) & is.na(all$motorcycle) & is.na(all$car),1,0)

all$no_income

#How many people did not report personal income? /*47 with missing income , 148 with zero income*/
all$income_pre
all$income_pre_re <- all$income_pre # keep reported hh income for tabulation

#How many people did not report household income? /*3 with missing income , 68 with zero income*/
all$income_hh_pre
all$income_hh_pre_re <- all$income_hh_pre # keep reported hh income for tabulation

#if no household income we use household assets based prediction. 
# preditions with regression model
library(caret)
library(MASS)
# Fit the full model 
full.model <- lm(income_hh_pre~electricity+
                 sew+
                 radio+
                 tv+
                 mobile+
                 fridge+
                # computer+
                 bicycle+
                 motorcycle+
                 car+
                 water+
                 toilet+
                 watch+
                 bank, all)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
all$income_hh_pred <-predict(step.model, new = all)

detach("package:MASS", unload=T)

all$income_imputed <- 0
all$income_imputed <- ifelse((is.na(all$income_hh_pre) | all$income_hh_pre==0) & !is.na(all$income_hh_pred),1,0)
all$income_hh_pre  <- ifelse((is.na(all$income_hh_pre) | all$income_hh_pre==0) & !is.na(all$income_hh_pred), all$income_hh_pred, all$income_hh_pre)
all$income_hh_pre  <- ifelse(all$income_hh_pre<0, 1, all$income_hh_pre)

all$income_hh_pre

#Calculates mean of personal income/household income for those with data (mean is 50.38%)
all <- all %>% mutate(m_income_pre = income_pre/income_hh_pre_re)
all$m_income_pre  <- ifelse(is.na(all$m_income_pre),0, all$m_income_pre)
all$mn_income_pre <- mean(all$m_income_pre)

#if only household earnings and not personal income reported then assume a percentage of household
all <- all %>% mutate(income_pre=replace(income_pre, 
                                         is.na(income_pre) & !is.na(income_hh_pre) & income_hh_pre !=0, income_hh_pre * mn_income_pre))

#If individual patient income is higher than household, we replace households income with the higher individual income
#percentage of HH income **THIS SHOULD BE DONE IN STEP 2
all$income_hh_pre <- ifelse((all$income_pre > all$income_hh_pre) & (is.na(all$income_pre) & is.na(all$income_hh_pre)), 
                            all$income_pre, all$income_hh_pre)

#We create wealth quintiles based on household income pre disease
find_quintile <- quantile(all$income_hh_pre, seq(0, 1, 0.2)) # find quintiles
all$hh_quintile <- findInterval(all$income_hh_pre, find_quintile, all.inside = TRUE)

#annualize monthly income by multiplying by 12
all$income_hh_pre_annual <- all$income_hh_pre *12
all$income_hh_now_annual <- all$income_hh_now *12
all$income_pre_annual <- all$income_pre *12
all$income_now_annual <- all$income_now *12

## Time valuation: HOURLY WAGE FOR Human Capital APPROACH */
  
#assuming 4.33 weeks per month. Income_pre is monthly and hours_worked is asked weekly
#gen hourly_wage = income_pre1/(hours_worked_pre*4.33) if income_pre1 !=. & hours_worked_pre !=.
all <- all %>% mutate(hourly_wage = ifelse(!is.na(income_pre) & !is.na(hours_worked_pre) & hours_worked_pre!=0, income_pre/(hours_worked_pre*4.33), NA))
all$hourly_wage
                      
#if blank use 2000 hours per year or 160 hrs per month
all <- all %>% mutate(hourly_wage = replace(hourly_wage, (is.na(hours_worked_pre)|hours_worked_pre==0) & (!is.na(income_pre)&income_pre!=0), income_pre/160))

#Replace hourly_wage iwth minimum wage if desired
all <- all %>% mutate(hourly_wage = replace(hourly_wage, income_pre==0 & hourly_wage==0, 1))

############################
# TREATMENT DURATION  in months: 8 months MDR intensive and 12 months MDR continuation#
###############################
#if treat duration is missing use guidelines of 6 months DSTB 20 months MDR
# all$tb_type <- as.character(all$tb_type)
# all$duration_int <- ifelse(all$treat_duration==6|all$treat_duration==8, 2,
#                             ifelse(all$treat_duration==9,4,
#                                    ifelse(all$treat_duration>9|all$treat_duration<6, 2,2)))
# all$duration_int <- ifelse(is.na(all$treat_duration), 2, all$duration_int)

# all$duration_cont <- ifelse(all$treat_duration==6, 4,
#                             ifelse(all$treat_duration==8, 6,
#                                    ifelse(all$treat_duration==9, 5,
#                                           ifelse((all$treat_duration>9|all$treat_duration<6|is.na(all$treat_duration)) & (all$tb_type=="tx_type1"|all$tb_type=="tx_type3"), 4,6))))
# 
# all$duration_cont <- ifelse(is.na(all$treat_duration), 4, all$duration_cont)


all$treat_duration <- all$duration_int + all$duration_cont
# replace treat_duration=6 if treat_duration==. & mdr==0
# replace treat_duration=20 if treat_duration==. & mdr==1

#long care seeking is defined as more than 4 weeks (15.38% of respondents)
all$delay <- NA 
all$delay <- ifelse(all$weeks_before_tx<=4& !is.na(all$weeks_before_tx), 0, all$delay)
all$delay <- ifelse(all$weeks_before_tx>4 & !is.na(all$weeks_before_tx), 1, all$delay)

#phase duration in days works under the assumptions that DSTB is split 2 months intensive,
# 4 months continuation (or 6 months for those with total duration of 8) and MDR
# is 8 months intensive, 12 months continuation. For those with a treatment duration of 8 months 
# we assume 2 months intensive 6 months continuation
all$phase <- as.character(all$phase)
all$phase_duration <- NA
all$phase_duration <- ifelse(all$phase=="phase1", all$duration_int*30, all$phase_duration)  # & mdr=="no"
all$phase_duration <- ifelse(all$phase=="phase2", all$duration_cont*30, all$phase_duration)  # & mdr=="no"

#Here we are assuming MDR intensive is 6 months and MDR continuation is 14 months, so 6x30 days=180 days and 14X30days=420 days
#replace phase_duration=180 if phase=="phase1" & mdr==1
#replace phase_duration=420 if phase=="phase2" & mdr==1
#*/
  
#scalar should be the inverse of the percent of the phase completed. 
#For example if you are half done with the continuation phase, the scalar should be 2
all$scalar <- all$phase_duration/all$phase_days
all$scalar <- ifelse(all$scalar<1, 1, all$scalar)

#*************************
#  EXPENDITURE
#*************************/
library("gdata")
all <- rename.vars(all, c("ex_total","ex_other_s"), c("tot_ex","other_ex"))


all$expend_hh_dis     <- rowSums(all[, grep("ex_", names(all))], na.rm=T)
all$tot_ex <- ifelse(is.na(all$tot_ex),0,all$tot_ex)

setDT(all)[, expend_hh:=pmax(expend_hh_dis, tot_ex)]

all$expend_hh_annual <- all$expend_hh*12
all$expend_hh_annual <- ifelse(all$expend_hh_annual==0,1,all$expend_hh_annual)

#*****************************************************************
#  PART III- RELAPSE PATIENTS ONLY (COSTS OF PREVIOUS EPISODES)
# We have limited information on previous episodes (# of hospital days)
#  We scale up costs based on number of days in hospital in previous 
#  visits.
#  *****************************************************************/
#gen n_prev_treat=tb_tx_times
  
#Length of stay: add variables that are the length of hospitalizations in part 5
#egen los_pre=rowtotal(hosp_dur_*), missing /*9 cases were hospitalised in previous episodes*/
    
#**************************************************************
# PART IV- NEW IN INTENSIVE PHASE ONLY (COSTS BEFORE DIAGNOSIS)
  
#  No need to scale it up here as patients will report all info 
#  as all of this happens prior to the interview.
#  ***************************************************************/
#number of visits are any row with some value for at least one visit time
#egen n_visits_before=rownonmiss(r_*visit_before) if phase=="phase1"
  
#travel and visit times in hours (added across all visits) before TB diagnosis
#time component of cost before diagnosis (new intensive phase )
colnames(all)[colnames(all)=="KEY"] <- "PARENT_KEY"

#bring necessary vars from main df to tx df
tx <- merge(x=tx, y=all[, c("PARENT_KEY",
                            "hourly_wage")], by = "PARENT_KEY", all.x=T)


tx$t_travel_before <- rowSums(tx[, grep("travel_time1.", names(tx))], na.rm=T)
tx$t_visit_before <- rowSums(tx[, grep("visit_time1.", names(tx))], na.rm=T)
tx$t_before <- rowSums(tx[, grep("^t_.*before$", names(tx))], na.rm=T) # rowsums column name starting with "t_" and ending with "before"

#putting a cost on that time this using the Human capital approach
tx$c_traveltime_before <- tx$t_travel_before * tx$hourly_wage
tx$c_visittime_before <- tx$t_visit_before * tx$hourly_wage
tx$c_time_before <- tx$t_before * tx$hourly_wage
  
#medical costs before TB diagnosis
tx$c_med_before_room     <- rowSums(tx[, grep("day1.", names(tx))], na.rm=T)
tx$c_med_before_cons     <- rowSums(tx[, grep("doctors1.", names(tx))], na.rm=T)
tx$c_med_before_radio    <- rowSums(tx[, grep("radio1.", names(tx))], na.rm=T)
tx$c_med_before_lab      <- rowSums(tx[, grep("lab1.", names(tx))], na.rm=T)
tx$c_med_before_proc     <- rowSums(tx[, grep("proc1.", names(tx))], na.rm=T)
tx$c_med_before_medicine <- rowSums(tx[, grep("medicine1.", names(tx))], na.rm=T)
tx$c_med_before_other    <- rowSums(tx[, grep("other1.", names(tx))], na.rm=T)
tx$c_med_before_dis      <- rowSums(tx[, grep("c_med_before_", names(tx))], na.rm=T)

#non-medical costs before TB diagnosis
tx$c_nmed_before_travel   <- rowSums(tx[, grep("travel1.", names(tx))], na.rm=T)
tx$c_nmed_before_food     <- rowSums(tx[, grep("food1.", names(tx))], na.rm=T)
#tx$c_nmed_before_nutri    <- rowSums(tx[, grep("nutri1.", names(tx))], na.rm=T)
tx$c_nmed_before_accommodation     <- rowSums(tx[, grep("accomodation1.", names(tx))], na.rm=T)
tx$c_nmed_before_dis      <- rowSums(tx[, grep("c_nmed_before_", names(tx))], na.rm=T)


tx$reimburse_before <- rowSums(tx[, grep("reimburse1.", names(tx))], na.rm=T)
tx$reimburse_before <- ifelse(is.na(tx$reimburse_before), 0, tx$reimburse_before)
  
# for combined visit costs choose maximum of reported total or sum of disaggregated costs
tx$totalmed <- rowSums(tx[, grep("totalmed1.", names(tx))], na.rm=T)
tx$totalnmed <- rowSums(tx[, grep("totalnmed1.", names(tx))], na.rm=T)

#medical costs before diagnosis
setDT(tx)[, c_med_before:=pmax(c_med_before_dis, totalmed)]

tx$c_med_before <- tx$c_med_before - tx$reimburse_before
tx$c_med_before <- ifelse(tx$c_med_before<0 & !is.na(tx$c_med_before), 0, tx$c_med_before)

#non-medical costs before diagnosis
setDT(tx)[, c_nmed_before:=pmax(c_nmed_before_dis, totalnmed)]

#Signaling if total non-medical costs sum of disaggregated are lower than the non-disaggregated total (i.e. totalnmed) /*52 real changes made*/
tx$test_flag2 <- 0
tx$test_flag2 <- ifelse(tx$c_nmed_before_dis<tx$totalnmed & !is.na(tx$totalnmed), 1, tx$test_flag2) 
tx$test_flag2 <- ifelse(is.na(tx$c_nmed_before_dis) & !is.na(tx$totalnmed), 1, tx$test_flag2)
  
# must breakdown nmed categories for 5 patients who use totalnmed 
#collapse (sum) c_nmed_before_travel c_nmed_before_food c_nmed_before_accomodation
#Results show absolute amounts for the 3 components so we calculated the shares shown below for example country. 
#Replace these numbers with your shares.
#29.6% travel , 51.9% food, 10.0% nutri, 8.4% other/accom
# replace c_nmed_before_travel= .296  * totalnmed if test_flag2==1
# replace c_nmed_before_food= .519 * totalnmed if test_flag2==1
# replace c_nmed_before_nutri= .10 * totalnmed if test_flag2==1
# replace c_nmed_before_accomodation= .084 * totalnmed if test_flag2==1

#direct costs before diagnosis
tx <- tx %>% 
  mutate(c_direct_before = rowSums(dplyr::select(., c_med_before, c_nmed_before), na.rm = TRUE))

#costs before diagnosis (HC approach)
tx <- tx %>% 
  mutate(c_before = rowSums(dplyr::select(., c_time_before, c_direct_before), na.rm = TRUE))

all <- merge(x=all, y=tx[, c("PARENT_KEY",
                             "t_travel_before",             "t_visit_before"   ,          
                             "t_before"        ,            "c_traveltime_before",        
                             "c_visittime_before",          "c_time_before"       ,       
                             "c_med_before_room"  ,         "c_med_before_cons"    ,      
                             "c_med_before_radio"  ,        "c_med_before_lab"      ,     
                             "c_med_before_proc"   ,        "c_med_before_medicine"  ,    
                             "c_med_before_other"  ,        "c_med_before_dis"        ,   
                             "c_nmed_before_travel" ,       "c_nmed_before_food"     ,    
                             "c_nmed_before_accommodation", "c_nmed_before_dis"       ,   
                             "reimburse_before"    ,        "totalmed"                ,   
                             "totalnmed"           ,        "c_med_before"           ,    
                             "c_nmed_before"        ,                        
                             "c_direct_before"      ,       "c_before"  
)], by = "PARENT_KEY", all.x=T)


#******************************************************************
#    PART V - ALL PATIENTS (COSTS DURING CURRENT EPISODE)
#*****************************************************************/
#bring necessary vars from main df to hp df
hp <- merge(x=hp, y=all[, c("PARENT_KEY",
                            "hourly_wage",
                            "hours_worked_pre",
                            "scalar")], by = "PARENT_KEY", all.x=T)

hp$t_travel_current <- rowSums(hp[, grep("hosp_travel.", names(hp))], na.rm=T)
hp$t_stay_current <- rowSums(hp[, grep("hosp_los.", names(hp))], na.rm=T)
hp$s_t_stay_current <- hp$t_stay_current * hp$hourly_wage

#convert lost days to hours
hp$t_stay_current_hrs <- hp$t_stay_current*hp$hours_worked_pre/7 

# working hours pre-disease (if missing, imputed with standard working hours per day (8hours) should be used)
# it was 24hours in the generic code 2017  
hp <- hp %>% 
  mutate(t_current = rowSums(dplyr::select(., t_travel_current, t_stay_current_hrs), na.rm = TRUE))

hp$s_t_current=hp$t_current * hp$scalar
  
#putting a cost on that time (Human capital approach)
hp$c_traveltime_current <- hp$t_travel_current * hp$hourly_wage
hp$c_staytime_current <- hp$t_stay_current * hp$hourly_wage
hp$c_time_current <- hp$t_current * hp$hourly_wage
  
#cost of current episode hospital stays
#Hospital medical costs
hp$c_med_current_day   <- rowSums(hp[, grep("hosp_day.", names(hp))], na.rm=T)
hp$c_med_current_cons  <- rowSums(hp[, grep("hosp_cons.", names(hp))], na.rm=T)
hp$c_med_current_radio <- rowSums(hp[, grep("hosp_radio.", names(hp))], na.rm=T)
hp$c_med_current_lab   <- rowSums(hp[, grep("hosp_lab.", names(hp))], na.rm=T)
hp$c_med_current_proc  <- rowSums(hp[, grep("hosp_proc.", names(hp))], na.rm=T)
hp$c_med_current_med   <- rowSums(hp[, grep("hosp_med.", names(hp))], na.rm=T)
hp$c_med_current_oth   <- rowSums(hp[, grep("hosp_other.", names(hp))], na.rm=T)

#Hospital non medical costs
hp$c_nmed_current_travel_cost   <- rowSums(hp[, grep("hosp_travel_cost.", names(hp))], na.rm=T)
hp$c_nmed_current_food          <- rowSums(hp[, grep("hosp_food.", names(hp))], na.rm=T)
# hp$c_nmed_current_nutri         <- rowSums(hp[, grep("hosp_nutri.", names(hp))], na.rm=T)
hp$c_nmed_current_other_nmed    <- rowSums(hp[, grep("hosp_other_nmed.", names(hp))], na.rm=T)

# Total medical and non medical costs
hp <- hp %>% 
  mutate(c_med_current_dis = rowSums(dplyr::select(., c_med_current_day, c_med_current_cons, c_med_current_radio, c_med_current_lab, c_med_current_proc, c_med_current_med, c_med_current_oth), na.rm = TRUE))

hp <- hp %>% 
  mutate(c_nmed_current_dis = rowSums(dplyr::select(., c_nmed_current_travel_cost, c_nmed_current_food, #/*c_nmed_current_nutri*/, 
                                                    c_nmed_current_other_nmed), na.rm = TRUE))

#Hospital reimbursements
hp$reimburse_current   <- rowSums(hp[, grep("hosp_reimburse.", names(hp))], na.rm=T)
hp$s_reimburse_current <- hp$reimburse_current*hp$scalar
  
# for combined hospital stay costs choose maximum of reported total or sum of disaggregated costs
hp$hosp_tot_med   <- rowSums(hp[, grep("hosp_tot_med.", names(hp))], na.rm=T)
hp$hosp_tot_nmed  <- rowSums(hp[, grep("hosp_tot_nmed.", names(hp))], na.rm=T)

setDT(hp)[, c_med_current:=pmax(c_med_current_dis, hosp_tot_med)]
hp$c_med_current <- hp$c_med_current - hp$reimburse_current

setDT(hp)[, c_nmed_current:=pmax(c_nmed_current_dis, hosp_tot_nmed)]

hp$test_flag1 <- 0
hp$test_flag1 <- ifelse(hp$c_nmed_current_dis < hp$hosp_tot_nmed & !is.na(hp$hosp_tot_nmed),1,hp$test_flag1)
hp$test_flag1 <- ifelse(is.na(hp$c_nmed_current_dis) & !is.na(hp$hosp_tot_nmed),1,hp$test_flag1)

hp <- hp %>% 
  mutate(c_direct_current = rowSums(dplyr::select(., c_med_current, c_nmed_current), na.rm = TRUE))

hp$hosp_total_all <- rowSums(hp[, grep("hosp_total_all.", names(hp))], na.rm=T)
hp$hosp_total_all <- hp$hosp_total_all- hp$reimburse_current
  
#If the total is larger than the sum of the medical and non-medical amounts, use the total
hp$test_flag4 <- 0
hp$c_direct_current2 <- 0
hp$c_direct_current2 <- ifelse(hp$c_direct_current < hp$hosp_total_all & !is.na(hp$hosp_total_all), hp$hosp_total_all, hp$c_direct_current2)

hp$test_flag4 <- ifelse(hp$c_direct_current < hp$hosp_total_all & !is.na(hp$hosp_total_all), 1, hp$test_flag4)
hp$test_flag4 <- ifelse(is.na(hp$c_direct_current) & !is.na(hp$hosp_total_all), 1, hp$test_flag4)

hp$c_direct_current <- ifelse(hp$test_flag4==1, hp$c_direct_current2, hp$c_direct_current)
  
#Using the commented collapse command below, we split teh total into parts 
#collapse (sum) c_nmed_current c_med_current c_direct_current 
#c_nonmed_current is 38.2%, c_med_current is 61.8%
#also replace non non-medical total if totall all is greater than parts
#  replace c_nmed_current=.382 * c_direct_current if test_flag4==1 
  
hp <- select (hp,-c(c_direct_current2)) 

y <- c("day", "cons", "radio", "lab", "proc", "med", "oth")

for (i in y) {
  hp[paste("s_c_med_current_", i, sep="")] <- hp[paste("c_med_current_",i, sep="")] * hp$scalar
}

y <- c("travel_cost", "food", #/*nutri*/,
       "other_nmed")

for (i in y) {
  hp[paste("s_c_nmed_current_", i, sep="")] <- hp[paste("c_nmed_current_",i, sep="")] * hp$scalar
}

hp$s_c_time_current <- hp$c_time_current * hp$scalar
hp$s_c_med_current  <- hp$c_med_current * hp$scalar
hp$s_c_nmed_current <- hp$c_nmed_current * hp$scalar
hp$s_c_direct_current <- hp$c_direct_current * hp$scalar
    
hp <- hp %>% 
  mutate(c_current = rowSums(dplyr::select(., c_time_current, c_direct_current), na.rm = TRUE))



all <- merge(x=all, y=hp[, c("PARENT_KEY",
                             "t_travel_current"  ,           "t_stay_current"              ,
                             "t_stay_current_hrs",           "t_current"                   ,
                             "s_t_current"        ,          "c_traveltime_current"        ,
                             "s_t_stay_current",
                             "c_staytime_current"  ,         "c_time_current"              ,
                             "c_med_current_day"  ,          "c_med_current_cons"          ,
                             "c_med_current_radio",          "c_med_current_lab"           ,
                             "c_med_current_proc" ,          "c_med_current_med"           ,
                             "c_med_current_oth"  ,          "c_nmed_current_travel_cost"  ,
                             "c_nmed_current_food",          "c_nmed_current_other_nmed"   ,
                             "c_med_current_dis"  ,          "c_nmed_current_dis"          ,
                             "reimburse_current"  ,          "s_reimburse_current"         ,
                             "hosp_tot_med"       ,          "hosp_tot_nmed"               ,
                             "c_med_current"      ,          "c_nmed_current"              ,
                             "test_flag1"         ,          "c_direct_current"            ,
                             "hosp_total_all"     ,          "test_flag4"                  ,
                             "s_c_med_current_day"  ,        "s_c_med_current_cons"        ,
                             "s_c_med_current_radio",        "s_c_med_current_lab"         ,
                             "s_c_med_current_proc" ,        "s_c_med_current_med"         ,
                             "s_c_med_current_oth"  ,        "s_c_nmed_current_travel_cost",
                             "s_c_nmed_current_food",        "s_c_nmed_current_other_nmed" ,
                             "s_c_time_current"     ,        "s_c_med_current"             ,
                             "s_c_nmed_current"     ,        "s_c_direct_current"          ,
                             "c_current" 
)], by = "PARENT_KEY", all.x=T)



#/***************************************************************
#    Costs for DOT and food costs during ambulatory care
#    
#    We will not need to use other patient info here to extrapolate
#    future behavior. We can use treatement duration and DOT or not DOT
#    to estimate number of DOT visits and costs remain constant per DOT visit.
#    
#***************************************************************/
#Transform time loss in hours instead of minutes therefore we don't divide by 60
# Time loss is valued following human capital approach
all$t_dot_per  <- all$dot_prov_time
all$c_dot_time <- all$t_dot_per * all$hourly_wage
all <- all %>% 
  mutate(c_direct_dot_per = rowSums(dplyr::select(., c_dot_food, c_travel_dot, c_dot_fee), na.rm = TRUE))

#Estimating number of visits in DOT throughout TB or MDR episode 
# Number of times going to DOT 

## OPTION 1: Simple extrapolation from current frequency (Option 2 or 3 is preferred!)
all$self_admin <- as.character(all$self_admin)
all$n_dot_visits_1 <- NA #/* self-administered in both phases */
# all$dot_times_week <- 3 #/* This is because of misdesign of questionnaire for SLB
# all$dot_times_week_int <- 5 #/* This is because of misdesign of questionnaire for SLB
# all$self_admin_int <- all$self_admin

all$n_dot_visits_1 <- ifelse(all$self_admin=="dot2" | all$self_admin=="dot3", all$treat_duration*4.33*all$dot_times_week, all$n_dot_visits_1)  #/* DOT in both phases*/
      
## OPTION 2: current frequency and NTP protocol
#for patients in intensive: Current frequency (int) + NTP procol (cont)  
all$n_dot_visits_int_2 <- NA
all$n_dot_visits_int_2 <- ifelse(all$phase=="phase1" & (all$self_admin=="dot2" | all$self_admin=="dot3"), 
                                 all$duration_int*4.33*all$dot_times_week, all$n_dot_visits_int_2)
all$n_dot_visits_cont_2 <- NA
all$n_dot_visits_cont_2 <- ifelse(all$phase=="phase1" & (all$self_admin=="dot2" | all$self_admin=="dot3"), 
                                 all$duration_int*4.33*5, all$n_dot_visits_cont_2)

#for patients in continuation: past frequency (int) + current frequency (cont)
all$n_dot_visits_int_2 <- ifelse(all$phase=="phase2" & (all$self_admin_int=="dot2" | all$self_admin_int=="dot3"),
                                all$duration_int*4.33*all$dot_times_week_int, all$n_dot_visits_int_2) 
all$n_dot_visits_cont_2 <- ifelse(all$phase=="phase2" & (all$self_admin=="dot2" | all$self_admin=="dot3"),
                                  all$duration_cont*4.33*all$dot_times_week, all$n_dot_visits_cont_2)

#total number of visits
all <- all %>% 
  mutate(n_dot_visits_2 = ifelse(!is.na(n_dot_visits_int_2)|!is.na(n_dot_visits_cont_2),
                 rowSums(dplyr::select(., n_dot_visits_int_2, n_dot_visits_cont_2), na.rm = T), NA))

## OPTION 3: Estimating frequency of DOTS visits for continuation phase
#for patients in intensive: Current frequency (int) + NTP procol (cont)  
all$n_dot_visits_int_3 <- NA
all$n_dot_visits_int_3 <- ifelse(all$phase=="phase1" & (all$self_admin=="dot2" | all$self_admin=="dot3"),
                                 all$duration_int*4.33*all$dot_times_week, all$n_dot_visits_int_3)  
    
all$n_dot_visits_cont_3 <- NA
all$e_dot_times_week <- setDT(all)[, .(e_dot_times_week = mean(dot_times_week[phase=="phase2"], na.rm = T))]
all$n_dot_visits_cont_3 <- ifelse(all$phase=="phase1" & (all$self_admin=="dot2" | all$self_admin=="dot3"),
                                  all$duration_cont*4.33*all$e_dot_times_week, all$n_dot_visits_cont_3)
    
#for patients in continuation: past frequency (int) + current frequency (cont)
all$n_dot_visits_int_3 <- ifelse(all$phase=="phase2" & (all$self_admin_int=="dot2" | all$self_admin_int=="dot3"),
                                all$duration_int*4.33*all$dot_times_week_int, all$n_dot_visits_int_3)
all$n_dot_visits_cont_3 <- ifelse(all$phase=="phase2" & (all$self_admin_int=="dot2" | all$self_admin_int=="dot3"),
                                  all$duration_cont*4.33*all$dot_times_week, all$n_dot_visits_cont_3)
    
#total number of visits
all <- all %>% 
  mutate(n_dot_visits_3 = ifelse(!is.na(n_dot_visits_int_3)|!is.na(n_dot_visits_cont_3),
                                 rowSums(dplyr::select(., n_dot_visits_int_3, n_dot_visits_cont_3), na.rm = T), NA))

all$n_dot_visits_1
all$n_dot_visits_2
all$n_dot_visits_3
    
##scripts go with OPTION 3
all$n_dot_visits <- all$n_dot_visits_3
    
#Estimating time loss for all ambulatory care visits (in minutes, transformed dot_prov_time above)
all$t_dot <- all$t_dot_per * all$n_dot_visits
all$t_dot <- ifelse(all$t_dot_per==0 | is.na(all$t_dot_per), 0, all$t_dot)
all$t_dot <- ifelse(all$n_dot_visits==0 | is.na(all$n_dot_visits), 0, all$t_dot)

#Estimating medical and non medical ambulatory care costs
all$c_med_dot         <- all$c_dot_fee*all$n_dot_visits
all$c_nmed_dot_travel <- all$c_travel_dot*all$n_dot_visits
all$c_nmed_dot_food   <- all$c_dot_food*all$n_dot_visits
all$c_direct_dot      <- all$c_direct_dot_per*all$n_dot_visits
    
#Estimating total indirect costs using Human Capital approach
all$c_indirect_dot <- all$c_dot_time *all$n_dot_visits
    
# Total for DOT costs
all <- all %>% 
  mutate(c_dot = ifelse(!is.na(c_indirect_dot)|!is.na(c_direct_dot),
                                 rowSums(dplyr::select(., c_direct_dot, c_indirect_dot), na.rm = T), NA))

#****************************************************************
#      Costs of picking up drugs and food costs during ambulatory care
    
#    This section does not require any extrapolation as the questions
#    already assume a consistent utilization throughout phases.
#    ****************************************************************/
## OPTION 1: Simple extrapolation from current frequency
all$drug_pickup <- as.character(all$drug_pickup)
all$drug_pickup_n <- as.character(all$drug_pickup_n)
all$drug_pickup_n <- ifelse(all$drug_pickup_n=="drug_often1",1,
                            ifelse(all$drug_pickup_n=="drug_often2",0.5,
                                   ifelse(all$drug_pickup_n=="drug_often2",0.25,
                                          ifelse(all$drug_pickup_n=="other",1/(all$treat_duration*4.33),NA))))


all$n_pickup_total_1 <- NA
all$n_pickup_total_1 <- ifelse(all$drug_pickup=="yes", all$treat_duration*4.33*all$drug_pickup_n, all$n_pickup_total_1)
    
## OPTION 2: current frequency and NTP protocol (e.g. 1 per week for int and 1 per month for cont)
#for patients in intensive: Current frequency (int) + NTP procol (cont)  
all$n_pickup_int_2 <- NA
all$n_pickup_int_2 <- ifelse(all$phase=="phase1" & all$drug_pickup=="yes", all$duration_int*4.33*all$drug_pickup_n, all$n_pickup_int_2)
    
all$n_pickup_cont_2 <- NA
all$n_pickup_cont_2 <- ifelse(all$phase=="phase1" & all$drug_pickup=="yes", all$duration_cont*1, all$n_pickup_cont_2)  #frequency needs to be changed based on NTP protocol
    
#for patients in continuation: past frequency (int) + current frequency (cont)
all$n_pickup_int_2 <- ifelse(all$phase=="phase2" & all$drug_pickup=="yes", all$duration_int*4.33, all$n_pickup_int_2)
all$n_pickup_cont_2 <- ifelse(all$phase=="phase2" & all$drug_pickup=="yes", all$duration_cont*4.33*all$drug_pickup_n, all$n_pickup_cont_2) 
    
#total number of visits
all <- all %>% 
  mutate(n_pickup_total_2 = ifelse(!is.na(n_pickup_int_2)|!is.na(n_pickup_cont_2),
                        rowSums(dplyr::select(., n_pickup_int_2, n_pickup_cont_2), na.rm = T), NA))

## OPTION 3: Estimating frequency of DOTS visits for continuation phase
#for patients in intensive: Current frequency (int) + NTP procol (cont)  
all$n_pickup_int_3 <- NA
all$n_pickup_int_3 <- ifelse(all$phase=="phase1" & all$drug_pickup=="yes", all$duration_int*4.33*all$drug_pickup_n, all$n_pickup_int_3) 
    
all$n_pickup_cont_3 <- NA
all$e_pickup_n_cont <- setDT(all)[, .(e_pickup_n_cont = mean(drug_pickup_n[phase=="phase2"& drug_pickup=="yes"], na.rm = T))]
all$n_pickup_cont_3 <- ifelse(all$phase=="phase1" & all$drug_pickup=="yes", all$duration_cont*4.33*all$e_pickup_n_cont, all$n_pickup_cont_3)
    
#for patients in continuation: past frequency (int) + current frequency (cont)
all$e_pickup_n_int <- setDT(all)[, .(e_pickup_n_int = mean(drug_pickup_n[phase=="phase1"& drug_pickup=="yes"], na.rm = T))]
all$n_pickup_int_3 <- ifelse(all$phase=="phase2" & all$drug_pickup=="yes", all$duration_int*4.33*all$e_pickup_n_int, all$n_pickup_int_3)

all$n_pickup_cont_3 = ifelse(all$phase=="phase2" & all$drug_pickup=="yes", all$duration_cont*4.33*all$drug_pickup_n, all$n_pickup_cont_3)
    
#total number of visits
all <- all %>% 
  mutate(n_pickup_total_3 = ifelse(!is.na(n_pickup_int_3)|!is.na(n_pickup_cont_3),
                                   rowSums(dplyr::select(., n_pickup_int_3, n_pickup_cont_3), na.rm = T), NA))

all$n_pickup_total_1
all$n_pickup_total_2
all$n_pickup_total_3

##scripts go with OPTION 3
all$n_pickup_total <- all$n_pickup_total_3
    
# Pick up drug: direct non medical cost calculation (transport + food)
all <- all %>% 
  mutate(c_pickup_nmed_per = rowSums(dplyr::select(., c_lodge_drug, drug_pickup_cost, drug_pickup_food), na.rm = T))

# Pick up drug: direct medical cost calculation (fee)
all$c_pickup_med_per <- all$drugpick_fee_amount
all$c_pickup_med_per <- ifelse(is.na(all$c_pickup_med_per),0,all$c_pickup_med_per)
    
# Pick up drug: DM+DNM 
all$c_pickup_nmed = all$c_pickup_nmed_per * all$n_pickup_total
all$c_pickup_med = all$c_pickup_med_per * all$n_pickup_total
    
all <- all %>% 
  mutate(c_pickup_direct = ifelse(!is.na(c_pickup_med)|!is.na(c_pickup_nmed),
                                   rowSums(dplyr::select(., c_pickup_med, c_pickup_nmed), na.rm = T), NA))

# Pick up drug:for more detailed non-medical cost categories
all$c_pickup_nmed_accommodation <- all$c_lodge_drug * all$n_pickup_total
all$c_pickup_nmed_travel <- all$drug_pickup_cost * all$n_pickup_total
all$c_pickup_nmed_food <- all$drug_pickup_food * all$n_pickup_total
    
#Indirect cost estimate (time) for pickup, using Human Capital Approach
all$t_pickup <- NA
all$t_pickup <- all$drug_pickup_time * all$n_pickup_total
all$t_pickup <- ifelse(all$drug_pickup_time==0 | is.na(all$drug_pickup_time), 0, all$t_pickup)
all$t_pickup <- ifelse(all$n_pickup_total==0 | is.na(all$n_pickup_total), 0, all$t_pickup)

all$c_pickup_indirect <- all$t_pickup * all$hourly_wage
    
# Pick up drug: total cost, direct plus indirect (human capital approach)
## Total for pickup*/
all <- all %>% 
  mutate(c_pickup = ifelse(!is.na(c_pickup_direct)|!is.na(c_pickup_indirect),
                                  rowSums(dplyr::select(., c_pickup_direct, c_pickup_indirect), na.rm = T), NA))

#*******************************************************************************************
#      Cost during outpatient visits for medical follow-up (see the doctor or nurse, have tests)
    
#*******************************************************************************************/
#per = per one visit
all$fu <- ifelse(all$fu<0, NA, all$fu) 
all$fu <- ifelse(is.na(all$fu), 0, all$fu) 
# all$fu <- ifelse(all$fu==100, 1, all$fu) #

#s = scaled up for phase and estimate the number of FU visit per month
all$s_fu_per <- NA
all$s_fu_per <- ifelse(all$phase=="phase1", all$fu * all$scalar / all$duration_int, all$s_fu_per)
all$s_fu_per <- ifelse(all$phase=="phase2", all$fu * all$scalar / all$duration_cont, all$s_fu_per)

##Scripts for estimating the number of FU visits in the other phase or NTP protocol*/
##OPTION 1. Simple estimation 
    
all$s_fu_1 = all$s_fu_per * all$treat_duration
    
##OPTION 2. current frequency and NTP protocol (e.g. 2 per month for int and 1 per month for cont)
#for patients in intensive: Current frequency (int) + NTP procol (cont)  
all$s_fu_int_2 <- NA
all$s_fu_int_2 <- ifelse(all$phase=="phase1", all$s_fu_per * all$duration_int, all$s_fu_int_2) 
all$s_fu_cont_2 <- NA
all$s_fu_cont_2 <- ifelse(all$phase=="phase1", 1 * all$duration_cont, all$s_fu_cont_2)  #frequency needs to be changed based on NTP protocol
    
#for patients in continuation: past frequency (int) + current frequency (cont)
all$s_fu_int_2 <- ifelse(all$phase=="phase2", 2 * all$duration_int, all$s_fu_int_2)
all$s_fu_cont_2 <- ifelse(all$phase=="phase2", all$s_fu_per * all$duration_int, all$s_fu_int_2)

#total number of visits
all <- all %>% 
  mutate(s_fu_2 = ifelse(!is.na(s_fu_int_2)|!is.na(s_fu_cont_2),
                           rowSums(dplyr::select(., s_fu_int_2, s_fu_cont_2), na.rm = T), NA))


##OPTION 3: Estimating frequency of DOTS visits for the other phase
#for patients in intensive: Current frequency (int) + from patient in cont (cont) 
all$s_fu_int_3 <- NA
all$s_fu_int_3 = ifelse(all$phase=="phase1", all$s_fu_per * all$duration_int, all$s_fu_int_3) 
    
all$s_fu_cont_3 <- NA
all$e_fu_per_cont <- setDT(all)[, .(e_fu_per_cont = mean(s_fu_per[phase=="phase2"], na.rm = T))]
all$s_fu_cont_3 <- ifelse(all$phase=="phase1", all$e_fu_per_cont * all$duration_cont, all$s_fu_cont_3)
    
#for patients in continuation: mean from patients in int(int) + Current frequency (cont) 
all$e_fu_per_int <- setDT(all)[, .(e_fu_per_int = mean(s_fu_per[phase=="phase1"], na.rm = T))]
all$s_fu_int_3 <- ifelse(all$phase=="phase2", all$e_fu_per_int * all$duration_int, all$s_fu_int_3)
    
all$s_fu_cont_3 = ifelse(all$phase=="phase2", all$s_fu_per * all$duration_cont, all$s_fu_cont_3)
    
#total number of visits
all <- all %>% 
  mutate(s_fu_3 = ifelse(!is.na(s_fu_int_3)|!is.na(s_fu_cont_3),
                         rowSums(dplyr::select(., s_fu_int_3, s_fu_cont_3), na.rm = T), NA))

all$s_fu_1
all$s_fu_2
all$s_fu_3

##scripts go with OPTION 3
all$s_fu <- all$s_fu_3

all <- all %>% mutate(c_fu_nmed_per = rowSums(dplyr::select(., c_travel_fu, c_accom_fu), na.rm = T))
all <- all %>% 
  mutate(c_fu_med_per = rowSums(dplyr::select(., c_fees_fu, c_radio_fu, c_tests_fu, c_proc_fu, c_med_fu, c_oth_med_fu, c_oth_fu), na.rm = T))

#more detailed medical and non-medical cost categories
all$c_fu_nmed_travel <- all$c_travel_fu * all$s_fu
all$c_fu_nmed_accom <- all$c_accom_fu * all$s_fu
all$c_fu_nmed <- all$c_fu_nmed_per * all$s_fu
all$c_fu_med <- all$c_fu_med_per * all$s_fu
    
#Direct medical and non-medical costs
all <- all %>% mutate(c_fu_direct = rowSums(dplyr::select(., c_fu_med, c_fu_nmed), na.rm = T))

all$t_fu <- all$travel_dur_fu * all$s_fu
all$t_fu <- ifelse(all$fu==0 | is.na(all$fu), 0, all$t_fu)  
all$t_fu <- ifelse(all$travel_dur_fu==0 | is.na(all$travel_dur_fu), 0, all$t_fu)  

all$c_fu_indirect <- all$t_fu* all$hourly_wage
    
# Total for follow-up*/
all <- all %>% mutate(c_fu = rowSums(dplyr::select(., c_fu_direct, c_fu_indirect), na.rm = T))


#******************************************
#     Costs for nutritional/food supplements 
#******************************************/
#/*OPTION 1: crude scaling*/
#assume 4.33 weeks per month. Treat duration is in months and amount reported per week
all$c_supp_1 <- (all$c_food_supp*4.33)*all$treat_duration
all$c_extra_1 <- (all$c_food_extra*4.33)*all$treat_duration

all <- all %>% 
  mutate(c_food_1 = ifelse(!is.na(c_supp_1)|!is.na(c_extra_1),
                         rowSums(dplyr::select(., c_supp_1, c_extra_1), na.rm = T), NA))


# OPTION 2: Scaling within the current phase and imputation for the other phase (median)*/
#supp
all$c_supp_int_2 <- NA
all$c_supp_int_2 <- ifelse(all$phase=="phase1", (all$c_food_supp*4.33)*all$duration_int, all$c_supp_int_2)

all$c_supp_cont_2 <- NA
all$c_supp_cont_2 <- ifelse(all$phase=="phase2", (all$c_food_supp*4.33)*all$duration_cont, all$c_supp_cont_2)

all$e_c_supp_int_2 <- setDT(all)[, .(e_c_supp_int_2 = median(c_supp_int_2[phase=="phase1"], na.rm = T))]
all$c_supp_int_2 <- ifelse(all$phase=="phase2" & !is.na(all$c_supp_cont), all$e_c_supp_int_2, all$c_supp_int_2)

all$e_c_supp_cont_2 <- setDT(all)[, .(e_c_supp_cont_2 = median(c_supp_cont_2[phase=="phase2"], na.rm = T))]
all$c_supp_cont_2 <- ifelse(all$phase=="phase1" & !is.na(all$c_supp_int), all$e_c_supp_cont_2, all$c_supp_cont_2)

all <- all %>% 
  mutate(c_supp_2 = ifelse(!is.na(c_supp_int_2)|!is.na(c_supp_cont_2),
                           rowSums(dplyr::select(., c_supp_int_2, c_supp_cont_2), na.rm = T), NA))

# extra
# exclude({
  
all$c_extra_int_2 <- NA
all$c_extra_int_2 <- ifelse(all$phase=="phase1", (all$c_food_extra*4.33)*all$duration_int, all$c_extra_int_2)

all$c_extra_cont_2 <- NA
all$c_extra_cont_2 <- ifelse(all$phase=="phase2", (all$c_food_extra*4.33)*all$duration_cont, all$c_extra_cont_2)

all$e_c_extra_int_2 <- setDT(all)[, .(e_c_extra_int_2 = median(c_extra_int_2[phase=="phase1"], na.rm = T))]
all$c_extra_int_2 <- ifelse(all$phase=="phase2" & !is.na(all$c_extra_cont), all$e_c_extra_int_2, all$c_extra_int_2)

all$e_c_extra_cont_2 <- setDT(all)[, .(e_c_extra_cont_2 = median(c_extra_cont_2[phase=="phase2"], na.rm = T))]
all$c_extra_cont_2 <- ifelse(all$phase=="phase1" & !is.na(all$c_extra_int), all$e_c_extra_cont_2, all$c_extra_cont_2)

all <- all %>% 
  mutate(c_extra_2 = ifelse(!is.na(c_extra_int_2)|!is.na(c_extra_cont_2),
                           rowSums(dplyr::select(., c_extra_int_2, c_extra_cont_2), na.rm = T), NA))

# })

# all$c_extra_2 <- 0

#total
all <- all %>% 
  mutate(c_food_2 = ifelse(!is.na(c_supp_2)|!is.na(c_extra_2),
                           rowSums(dplyr::select(., c_supp_2, c_extra_2), na.rm = T), NA))


##scripts go with OPTION 1
all$c_supp <- all$c_supp_1
all$c_extra <- all$c_extra_1

all$c_food <- all$c_food_1
    
#****************************************************************************
#  Costs for guardians - Only consider them if they lost income.
#****************************************************************************/
#number of people * number of occasions * number of hours * hourly wage rate (human capital approach)
# Estimating caregiver wage. Option 1 is to give everyone minimum wage $8 SBD per hour
# https://www.solomontimes.com/news/minimum-wage-doubled/9262
#Option 1: Replace caregiver hourly_wage with minimum wage if desired

all$caregiver_wage_1 <- NA
all$caregiver_wage_1 <- 250 #40000 per month (250 per hour)? in Tanzania as of 2013
    
#Option 2: Give caregiver an equal proportion of household income after deducting patient income
all$caregiver_wage_2 <- NA

all$caregiver_wage_2 <- ifelse(all$hhsize_a > 1, (all$income_hh_pre - all$income_pre)/(all$hhsize_a - 1), all$caregiver_wage_2)

#curently in wage per month
all$caregiver_wage_2 <- all$caregiver_wage_2/160  #/*now in hourly wage*/
all$caregiver_wage_2 <- ifelse(all$caregiver_wage_2 < 0 & !is.na(all$caregiver_wage_2), NA, all$caregiver_wage_2)

all$e_caregiver_wage_2 <- setDT(all)[, .(e_caregiver_wage_2  = median(caregiver_wage_2[!is.na(all$caregiver_wage_2)], na.rm = T))]

all$caregiver_wage_2 <- ifelse(is.na(all$caregiver_wage_2),all$e_caregiver_wage_2,all$caregiver_wage_2)
                               
#script continues with OPTION 2
all$caregiver_wage <- all$caregiver_wage_2
    
#Caregiver time loss count in hours.

#********************
# DOT
#********************
#DOT visits: time loss (hours)and time valuation (human capital approach)
all$guard_dot_yes <- ifelse(all$guard_dot_n=="yes",1,NA)

all$t_guard_dot <- all$guard_dot_yes * all$n_dot_visits * all$t_dot_per
all$t_guard_dot <- ifelse(!is.na(all$n_dot_visits) & is.na(all$t_guard_dot), 0, all$t_guard_dot)

#all$t_guard_dot <- ifelse(!is.na(all$guard_loi), all$guard_loi * all$n_dot_visits * all$t_dot_per, all$t_guard_dot)
all$c_guard_dot <- ifelse(all$guard_dot_n=="yes", all$t_guard_dot* all$caregiver_wage, NA)
    
#********************
# pickup
#********************
#Drug Pickups: time loss (hours) and time valuation (human capital approach)
all$guard_drug_yes <- ifelse(all$guard_drug_n=="yes",1,NA)

all$t_guard_pickup <- all$guard_drug_yes * (all$t_pickup)
all$t_guard_pickup <- ifelse(!is.na(all$drug_pickup_n) & is.na(all$t_guard_pickup), 0, all$t_guard_pickup)

#all$t_guard_pickup <- ifelse(!is.na(all$guard_loi), all$guard_loi  * (all$t_pickup), all$t_guard_pickup)
all$c_guard_pickup <- ifelse(all$guard_drug_n=="yes", all$t_guard_pickup * all$caregiver_wage, NA) 

#********************
# follow-up
#********************
# Follow-up visits: time loss (hours) and time valuation (human capital approach)
all$guard_fu_yes <- ifelse(all$guard_fu_n=="yes",1,NA)

all$t_guard_fu <- all$guard_fu_yes * all$s_fu * (all$travel_dur_fu)
all$t_guard_fu <- ifelse(!is.na(all$s_fu) & is.na(all$t_guard_fu), 0, all$t_guard_fu)

#all$t_guard_fu <- ifelse(!is.na(all$guard_loi), all$guard_loi  * (all$s_fu), all$travel_dur_fu)
all$c_guard_fu <- ifelse(all$guard_fu_n=="yes", all$t_guard_fu * all$caregiver_wage, NA) 
    
#********************
# hospitalization
#********************
# Hospitalizations: time loss and time valuation. Used scaled hospitalizations.
all$guard_hosp_yes <- ifelse(all$guard_hosp_n=="yes",1,NA)

all$t_guard_hosp <- all$guard_hosp_yes * all$s_t_current
# all$t_guard_hosp <- ifelse(!is.na(all$guard_loi), all$guard_loi * all$s_t_current,  all$t_guard_hosp)
all$c_guard_hosp <- ifelse(all$guard_hosp_n==1, all$t_guard_hosp * all$caregiver_wage, NA) 
    
#Total Caregiver time and indirect cost: dot, pick up, follow-up and hospitalisation
all <- all %>% 
  mutate(t_guard_tot = ifelse(!is.na(t_guard_dot)|!is.na(t_guard_pickup)|!is.na(t_guard_fu)|!is.na(t_guard_hosp),
                           rowSums(dplyr::select(., t_before, t_guard_dot, t_guard_pickup, t_guard_fu, t_guard_hosp), na.rm = T), NA))

all <- all %>% 
  mutate(c_guard_tot = ifelse(!is.na(c_guard_dot)|!is.na(c_guard_pickup)|!is.na(c_guard_fu)|!is.na(c_guard_hosp),
                              rowSums(dplyr::select(., c_guard_dot, c_guard_pickup, c_guard_fu, c_guard_hosp), na.rm = T), NA))

#replace t_guard_tot=0 if t_guard_tot==.
#replace c_guard_tot=0 if c_guard_tot==.
    
#***********************************************
#   Dissaving
#***********************************************/
#add all types of dissaving
exclude({
all$dissavings <- as.character(all$dissavings)
all <- all %>% 
  mutate(dissaving_tot = ifelse(all$dissavings=="yes",
                              rowSums(dplyr::select(., dissavings1, dissavings2, dissavings3, dissavings4), na.rm = T), NA))
})

all$borrow <- as.character(all$borrow)
all <- all %>% 
  mutate(borrow_tot = ifelse(all$borrow=="yes",
                                rowSums(dplyr::select(., borrow4), na.rm = T), NA))

all$assetsale <- as.character(all$assetsale)
all <- all %>% 
  mutate(asset_proceeds = ifelse(all$assetsale=="yes",
                                rowSums(dplyr::select(., asset_proceeds1), na.rm = T), NA))

all <- all %>% 
  mutate(coping_amount = ifelse(#!is.na(dissaving_tot)|
    !is.na(borrow_tot)|!is.na(asset_proceeds),
                              rowSums(dplyr::select(., #dissaving_tot, 
                                                    borrow_tot, asset_proceeds), na.rm = T), NA))

#coping will be a dummy if any type of dissaving happens
all$coping <- "no"
all$coping <- ifelse(#all$dissavings=="yes"|
  all$borrow=="yes"|all$assetsale=="yes", "yes", all$coping)

#**********************
#  SOCIAL EFFECTS
#**********************/
# manage multiple answers in one column for social effect
library("tidyverse")
soc <- all %>% select(PARENT_KEY,social_effect)

soc <- soc %>% 
  separate(social_effect, paste0("v", 1:6), remove=F) %>% 
  gather(q2, val, v1:v6) %>% 
  na.exclude %>% 
  mutate(val=paste0("social_effect.", val), q2=1) %>% 
  distinct() %>%
  spread(val, q2)

soc[is.na(soc)] <- 0
names(soc) <- sub(".*\\.", "", names(soc))
soc <- subset(soc, select = -c(social_effect, social))

all <- merge(x=all, y=soc[, c("PARENT_KEY",
                              "dropout",
                              "divorce",
                              "exclusion",
                              "food",
                              "insec",
                              "lossofjob",
                              "reloc",
                              "no")], by = "PARENT_KEY", all.x=T)

#any social effect
all <- all %>% 
  mutate(any_socialeffect = rowSums(dplyr::select(.,divorce, dropout, exclusion, food, insec, lossofjob, reloc), na.rm = T), NA)
all$any_socialeffect <- ifelse(all$any_socialeffect>1, 1, 0) 
    
################################################################################################################
# Save R data
################################################################################################################
save.image("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/TBPCS_iso3_noimp.RData")

