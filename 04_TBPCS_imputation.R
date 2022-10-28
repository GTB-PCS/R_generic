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
load("./TBPCS_iso3_noimp.RData")

#*******************************************************************************
#  EXTRAPOLATION

#The plan is to scale up each patient's resource utilization in the current 
#treatment phase based on their reported usage. This is done above using the scalar
#and the s_ variables.

#The other phase will be estimated based on average (m_) utilization of other 
#patients in the other phase (not by facility...yet)
#*******************************************************************************/ 
#// PART 4 is only completed by intensive phase (phase1) patients. We use their averages to extrapolate
#// for continuous phase (phase2) patients before diagnosis activities.

#//Medians for part 4 major categories
all$tb_type <- as.character(all$tb_type) #this is special stratification for SLB *no MDR, no HIV positive cases

all$mdr <- as.character(all$mdr)
all <- all %>% mutate (m_med_before = median(c_med_before[mdr=="no"&phase=="phase1"&!is.na(c_med_before)], na.rm = T))
all <- all %>% mutate (m_nmed_before = median(c_nmed_before[mdr=="no"&phase=="phase1"&!is.na(c_nmed_before)], na.rm = T))
all <- all %>% mutate (m_direct_before = median(c_nmed_before[mdr=="no"&phase=="phase1"&!is.na(c_direct_before)], na.rm = T))

all <- all %>% mutate (m_med_before_mdr = median(c_med_before[mdr=="yes"&phase=="phase1"&!is.na(c_med_before)], na.rm = T))
all <- all %>% mutate (m_nmed_before_mdr = median(c_nmed_before[mdr=="yes"&phase=="phase1"&!is.na(c_nmed_before)], na.rm = T))
all <- all %>% mutate (m_direct_before_mdr = median(c_direct_before[mdr=="yes"&phase=="phase1"&!is.na(c_direct_before)], na.rm = T))

#//time and cost of time
all <- all %>% mutate (m_t_before = median(t_before[mdr=="no"&phase=="phase1"&!is.na(t_before)], na.rm = T))
all <- all %>% mutate (m_c_time_before = median(c_time_before[mdr=="no"&phase=="phase1"&!is.na(c_time_before)], na.rm = T))

all <- all %>% mutate (m_t_before_mdr = median(t_before[mdr=="yes"&phase=="phase1"&!is.na(t_before)], na.rm = T))
all <- all %>% mutate (m_c_time_before_mdr = median(c_time_before[mdr=="yes"&phase=="phase1"&!is.na(c_time_before)], na.rm = T))

#***************************************************************
#DS- Patients -MEDIAN ALL SURVEY PARTICIPANTS
#****************************************************************/
#//CURRENT DS-TB TREATMENT DIRECT COSTS- INTENSIVE PHASE AVERAGES
all <- all %>% mutate (m_med_current_int = median(s_c_med_current[mdr=="no"&phase=="phase1"&!is.na(s_c_med_current)], na.rm = T))
all <- all %>% mutate (m_nmed_current_int = median(s_c_nmed_current[mdr=="no"&phase=="phase1"&!is.na(s_c_nmed_current)], na.rm = T))
all <- all %>% mutate (m_direct_current_int = median(s_c_direct_current[mdr=="no"&phase=="phase1"&!is.na(s_c_direct_current)], na.rm = T))

#//sub-categories
all <- all %>% mutate (m_direct_current_travel_cost_int = median(s_c_nmed_current_travel_cost[mdr=="no"&phase=="phase1"&!is.na(s_c_nmed_current_travel_cost)], na.rm = T))
all <- all %>% mutate (m_direct_current_food_int = median(s_c_nmed_current_travel_cost[mdr=="no"&phase=="phase1"&!is.na(s_c_nmed_current_food)], na.rm = T))
all <- all %>% mutate (m_direct_current_other_nmed_int = median(s_c_nmed_current_other_nmed[mdr=="no"&phase=="phase1"&!is.na(s_c_nmed_current_other_nmed)], na.rm = T))

#//CURRENT TREATMENT DIRECT COSTS- CONTINUATION PHASE AVERAGES
all <- all %>% mutate (m_med_current_con = median(s_c_med_current[mdr=="no"&phase=="phase2"&!is.na(s_c_med_current)], na.rm = T))
all <- all %>% mutate (m_nmed_current_con = median(s_c_nmed_current[mdr=="no"&phase=="phase2"&!is.na(s_c_nmed_current)], na.rm = T))
all <- all %>% mutate (m_direct_current_con = median(s_c_direct_current[mdr=="no"&phase=="phase2"&!is.na(s_c_direct_current)], na.rm = T))

#//sub-categories
all <- all %>% mutate (m_direct_current_travel_cost_con = median(s_c_nmed_current_travel_cost[mdr=="no"&phase=="phase2"&!is.na(s_c_nmed_current_travel_cost)], na.rm = T))
all <- all %>% mutate (m_direct_current_food_con = median(s_c_nmed_current_travel_cost[mdr=="no"&phase=="phase2"&!is.na(s_c_nmed_current_food)], na.rm = T))
all <- all %>% mutate (m_direct_current_other_nmed_con = median(s_c_nmed_current_other_nmed[mdr=="no"&phase=="phase2"&!is.na(s_c_nmed_current_other_nmed)], na.rm = T))

#//CURRENT TREATMENT - TIME - INTENSIVE PHASE AVERAGES and time valuation (human capital approach)
all <- all %>% mutate (m_t_current_int = median(s_t_current[mdr=="no"&phase=="phase1"&!is.na(s_t_current)], na.rm = T))
all <- all %>% mutate (m_c_time_current_int = median(s_c_time_current[mdr=="no"&phase=="phase1"&!is.na(s_c_time_current)], na.rm = T))

#//CURRENT TREATMENT - TIME - CONTINUATION PHASE AVERAGES and time valuation (human capital approach)
all <- all %>% mutate (m_t_current_con = median(s_t_current[mdr=="no"&phase=="phase2"&!is.na(s_t_current)], na.rm = T))
all <- all %>% mutate (m_c_time_current_con = median(s_c_time_current[mdr=="no"&phase=="phase2"&!is.na(s_c_time_current)], na.rm = T))

#/***************************************************************
#MDR- Patients - MEDIAN ALL SURVEY PARTICIPANTS
#******************************************************************/
#//CURRENT DR-TB TREATMENT DIRECT COSTS- INTENSIVE PHASE AVERAGES
all <- all %>% mutate (m_med_current_int_mdr = median(s_c_med_current[mdr=="yes"&phase=="phase1"&!is.na(s_c_med_current)], na.rm = T))
all <- all %>% mutate (m_nmed_current_int_mdr = median(s_c_nmed_current[mdr=="yes"&phase=="phase1"&!is.na(s_c_nmed_current)], na.rm = T))
all <- all %>% mutate (m_direct_current_int_mdr = median(s_c_direct_current[mdr=="yes"&phase=="phase1"&!is.na(s_c_direct_current)], na.rm = T))

#//sub-categories
all <- all %>% mutate (m_d_current_tra_cost_int_mdr = median(s_c_nmed_current_travel_cost[mdr=="yes"&phase=="phase1"&!is.na(s_c_nmed_current_travel_cost)], na.rm = T))
all <- all %>% mutate (m_direct_current_food_int_mdr = median(s_c_nmed_current_travel_cost[mdr=="yes"&phase=="phase1"&!is.na(s_c_nmed_current_food)], na.rm = T))
all <- all %>% mutate (m_d_current_other_nmed_int_mdr = median(s_c_nmed_current_other_nmed[mdr=="yes"&phase=="phase1"&!is.na(s_c_nmed_current_other_nmed)], na.rm = T))

#//CURRENT TREATMENT DIRECT COSTS- CONTINUATION PHASE AVERAGES
all <- all %>% mutate (m_med_current_con_mdr = median(s_c_med_current[mdr=="yes"&phase=="phase2"&!is.na(s_c_med_current)], na.rm = T))
all <- all %>% mutate (m_nmed_current_con_mdr = median(s_c_nmed_current[mdr=="yes"&phase=="phase2"&!is.na(s_c_nmed_current)], na.rm = T))
all <- all %>% mutate (m_direct_current_con_mdr = median(s_c_direct_current[mdr=="yes"&phase=="phase2"&!is.na(s_c_direct_current)], na.rm = T))

#//sub-categories
all <- all %>% mutate (m_d_current_tra_cost_con_mdr = median(s_c_nmed_current_travel_cost[mdr=="yes"&phase=="phase2"&!is.na(s_c_nmed_current_travel_cost)], na.rm = T))
all <- all %>% mutate (m_direct_current_food_con_mdr = median(s_c_nmed_current_travel_cost[mdr=="yes"&phase=="phase2"&!is.na(s_c_nmed_current_food)], na.rm = T))
all <- all %>% mutate (m_d_current_other_nmed_con_mdr = median(s_c_nmed_current_other_nmed[mdr=="yes"&phase=="phase2"&!is.na(s_c_nmed_current_other_nmed)], na.rm = T))

#//CURRENT TREATMENT - TIME - INTENSIVE PHASE AVERAGES and time valuation (human capital approach)
all <- all %>% mutate (m_t_current_int_mdr = median(s_t_current[mdr=="yes"&phase=="phase1"&!is.na(s_t_current)], na.rm = T))
all <- all %>% mutate (m_c_time_current_int_mdr = median(s_c_time_current[mdr=="yes"&phase=="phase1"&!is.na(s_c_time_current)], na.rm = T))

#//CURRENT TREATMENT - TIME - CONTINUATION PHASE AVERAGES and time valuation (human capital approach)
all <- all %>% mutate (m_t_current_con_mdr = median(s_t_current[mdr=="yes"&phase=="phase2"&!is.na(s_t_current)], na.rm = T))
all <- all %>% mutate (m_c_time_current_con_mdr = median(s_c_time_current[mdr=="yes"&phase=="phase2"&!is.na(s_c_time_current)], na.rm = T))

#*******************************************************************************
#CALCULATE TOTAL COSTS - Disaggregated by type     
#*******************************************************************************/
#/*With HOSPITALIZATION*/
#//DSTB patients in intensive phase costs and time
all <- all %>% mutate(c_medical1 = rowSums(dplyr::select(., c_med_before, s_c_med_current, m_med_current_con, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before1 <- all$c_med_before
all <- all %>% mutate(c_med_hosp1 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con), na.rm = TRUE))
all <- all %>% mutate(c_nmed_hosp1 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_con), na.rm = TRUE))
all <- all %>% mutate(t_hosp1 = rowSums(dplyr::select(., s_t_current, m_t_current_con), na.rm = TRUE))
all <- all %>% mutate(c_medical_after1 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical1 = rowSums(dplyr::select(., c_nmed_before, s_c_nmed_current, m_nmed_current_con, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before1 <- all$c_nmed_before
all <- all %>% mutate(c_nmedical_after1 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_con, c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect1 = rowSums(dplyr::select(., c_time_before, s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect1 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_con), all$c_indirect1 + (all$hourly_wage * all$m_t_current_con), all$c_indirect1)
all$c_indirect_before1 <- all$c_time_before
all <- all %>% mutate(c_indirect_after1 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_after1 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_con), all$c_indirect_after1 + (all$hourly_wage * all$m_t_current_con), all$c_indirect_after1)

#//DSTB patients in continuation phase costs and time
all <- all %>% mutate(c_medical2 = rowSums(dplyr::select(., m_med_before, s_c_med_current, m_med_current_int, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before2 <- all$m_med_before
all <- all %>% mutate(c_med_hosp2 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int), na.rm = TRUE))
all <- all %>% mutate(c_nmed_hosp2 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_int), na.rm = TRUE))
all <- all %>% mutate(t_hosp2 = rowSums(dplyr::select(., s_t_current, m_t_current_int), na.rm = TRUE))
all <- all %>% mutate(c_medical_after2 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical2 = rowSums(dplyr::select(., m_nmed_before, s_c_nmed_current, m_nmed_current_int, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before2 <- all$m_nmed_before
all <- all %>% mutate(c_nmedical_after2 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_int, c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect2 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect2 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_int), all$c_indirect2 + (all$hourly_wage * (all$m_t_current_int+all$m_t_before)), all$c_indirect2)
all$c_indirect_before2 <- all$m_t_before * all$hourly_wage
all <- all %>% mutate(c_indirect_after2 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_after2 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_int), all$c_indirect_after2 + (all$hourly_wage * all$m_t_current_int), all$c_indirect_after2)


#//MDR patients in intensive phase costs and time
all <- all %>% mutate(c_medical3 = rowSums(dplyr::select(., c_med_before, s_c_med_current, m_med_current_con_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before3 <- all$c_med_before
all <- all %>% mutate(c_med_hosp3 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con_mdr), na.rm = TRUE))
all <- all %>% mutate(c_nmed_hosp3 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_con_mdr), na.rm = TRUE))
all <- all %>% mutate(t_hosp3 = rowSums(dplyr::select(., s_t_current, m_t_current_con_mdr), na.rm = TRUE))
all <- all %>% mutate(c_medical_after3 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical3 = rowSums(dplyr::select(., c_nmed_before, s_c_nmed_current, m_nmed_current_con_mdr, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before3 <- all$c_nmed_before
all <- all %>% mutate(c_nmedical_after3 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_con_mdr, c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect3 = rowSums(dplyr::select(., c_time_before, s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect3 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_con_mdr), all$c_indirect3 + (all$hourly_wage * all$m_t_current_con_mdr), all$c_indirect3)
all$c_indirect_before3 <- all$c_time_before
all <- all %>% mutate(c_indirect_after3 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_after3 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_con_mdr), all$c_indirect_after3 + (all$hourly_wage * all$m_t_current_con_mdr), all$c_indirect_after3)

#//MDR patients in continuation phase costs and time
all <- all %>% mutate(c_medical4 = rowSums(dplyr::select(., m_med_before_mdr, s_c_med_current, m_med_current_int_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before4 <- all$m_med_before
all <- all %>% mutate(c_med_hosp4 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int_mdr), na.rm = TRUE))
all <- all %>% mutate(c_nmed_hosp4 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_int_mdr), na.rm = TRUE))
all <- all %>% mutate(t_hosp4 = rowSums(dplyr::select(., s_t_current, m_t_current_int_mdr), na.rm = TRUE))
all <- all %>% mutate(c_medical_after4 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical4 = rowSums(dplyr::select(., m_nmed_before_mdr, s_c_nmed_current, m_nmed_current_int_mdr, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before4 <- all$m_nmed_before
all <- all %>% mutate(c_nmedical_after4 = rowSums(dplyr::select(., s_c_nmed_current, m_nmed_current_int_mdr, c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect4 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect4 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_int_mdr), all$c_indirect4 + (all$hourly_wage * (all$m_t_current_int_mdr+all$m_t_before)), all$c_indirect4)
all$c_indirect_before4 <- all$m_t_before_mdr * all$hourly_wage
all <- all %>% mutate(c_indirect_after4 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_after4 <- ifelse(!is.na(all$hourly_wage) & !is.na(all$m_t_current_int_mdr), all$c_indirect_after4 + (all$hourly_wage * all$m_t_current_int_mdr), all$c_indirect_after4)

#/*Without HOSPITALIZATION*/
#//DSTB patients in intensive phase costs and time
all <- all %>% mutate(c_medical5 = rowSums(dplyr::select(., c_med_before, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before5 <- all$c_med_before
all <- all %>% mutate(c_medical_after5 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical5 = rowSums(dplyr::select(., c_nmed_before, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before5 <- all$c_nmed_before
all <- all %>% mutate(c_nmedical_after5 = rowSums(dplyr::select(., c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect5 = rowSums(dplyr::select(., c_time_before, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_before5 <- all$c_time_before
all <- all %>% mutate(c_indirect_after5 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))

#//DSTB patients in continuation phase costs and time
all <- all %>% mutate(c_medical6 = rowSums(dplyr::select(., m_med_before, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before6 <- all$m_med_before
all <- all %>% mutate(c_medical_after6 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical6 = rowSums(dplyr::select(., m_nmed_before, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before6 <- all$m_nmed_before
all <- all %>% mutate(c_nmedical_after6 = rowSums(dplyr::select(., c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect6 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect6 <- ifelse(!is.na(all$hourly_wage), all$c_indirect6 + (all$hourly_wage * (all$m_t_before)), all$c_indirect6)
all$c_indirect_before6 <- all$m_t_before * all$hourly_wage
all <- all %>% mutate(c_indirect_after6 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))


#//MDR patients in intensive phase costs and time
all <- all %>% mutate(c_medical7 = rowSums(dplyr::select(., c_med_before, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before7 <- all$c_med_before
all <- all %>% mutate(c_medical_after7 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical7 = rowSums(dplyr::select(., c_nmed_before, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before7 <- all$c_nmed_before
all <- all %>% mutate(c_nmedical_after7 = rowSums(dplyr::select(., c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect7 = rowSums(dplyr::select(., c_time_before, c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect_before7 <- all$c_time_before
all <- all %>% mutate(c_indirect_after7 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))

#//MDR patients in continuation phase costs and time
all <- all %>% mutate(c_medical8 = rowSums(dplyr::select(., m_med_before_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all$c_medical_before8 <- all$m_med_before
all <- all %>% mutate(c_medical_after8 = rowSums(dplyr::select(., s_c_med_current, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(c_nmedical8 = rowSums(dplyr::select(., m_nmed_before_mdr, c_nmed_dot_travel, c_nmed_dot_food, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all$c_nmedical_before8 <- all$m_nmed_before
all <- all %>% mutate(c_nmedical_after8 = rowSums(dplyr::select(., c_nmed_dot_food, c_nmed_dot_travel, c_pickup_nmed, c_fu_nmed, c_food), na.rm = TRUE))
all <- all %>% mutate(c_indirect8 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))
all$c_indirect8 <- ifelse(!is.na(all$hourly_wage), all$c_indirect8 + (all$hourly_wage * (all$m_t_before_mdr)), all$c_indirect8)
all$c_indirect_before8 <- all$m_t_before_mdr * all$hourly_wage
all <- all %>% mutate(c_indirect_after8 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect, c_guard_tot), na.rm = TRUE))

all$c_medical  <- NA
all$c_med_hosp  <- NA
all$c_nmed_hosp  <- NA
all$t_hosp  <- NA
all$c_medical_before  <- NA
all$c_medical_after  <- NA
all$c_nmedical  <- NA
all$c_nmedical_before  <- NA
all$c_nmedical_after  <- NA
all$c_indirect <- NA
all$c_indirect_before <- NA
all$c_indirect_after <- NA

all$current_hosp <- as.character(all$current_hosp)
all$prev_hosp <- as.character(all$prev_hosp)

i <- "c_med_hosp"
y1 <- c("c_medical", "c_med_hosp", "c_nmed_hosp", "t_hosp", "c_medical_before", "c_medical_after", "c_nmedical", "c_nmedical_before", "c_nmedical_after", "c_indirect", "c_indirect_before", "c_indirect_after")
for (i in y1) {
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="no" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 1, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="no" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 2, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="yes" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 3, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="yes" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 4, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(is.na(all[,paste(i)]), 0, all[,paste(i)])
}

y2 <- c("c_medical", "c_medical_before", "c_medical_after", "c_nmedical", "c_nmedical_before", "c_nmedical_after", "c_indirect", "c_indirect_before", "c_indirect_after")
for (i in y2) {
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="no" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 5, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="no" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 6, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="yes" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 7, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="yes" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 8, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(is.na(all[,paste(i)]), 0, all[,paste(i)])
}

#/*********************************
#Make cost categories for pie chart 
#*********************************/
#/*with HOSPITALIZATION*/
all <- all %>% mutate(cat_current_med1 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos1 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con), na.rm = TRUE))
all <- all %>% mutate(cat_current_travel1 = rowSums(dplyr::select(., s_c_nmed_current_travel_cost, m_direct_current_travel_cost_con, c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation1 = rowSums(dplyr::select(., s_c_nmed_current_other_nmed, m_direct_current_other_nmed_con, c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food1 = rowSums(dplyr::select(., s_c_nmed_current_food, m_direct_current_food_con, c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri1 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect1 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))
all$cat_current_indirect1 <- all$cat_current_indirect1 + (all$hourly_wage * all$m_t_current_con)

#//DSTB patients in continuation phase costs and time
all <- all %>% mutate(cat_current_med2 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos2 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int), na.rm = TRUE))
all <- all %>% mutate(cat_current_travel2 = rowSums(dplyr::select(., s_c_nmed_current_travel_cost, m_direct_current_travel_cost_int, c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation2 = rowSums(dplyr::select(., s_c_nmed_current_other_nmed, m_direct_current_other_nmed_int, c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food2 = rowSums(dplyr::select(., s_c_nmed_current_food, m_direct_current_food_int, c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri2 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect2 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))
all$cat_current_indirect2 <- all$cat_current_indirect2 + (all$hourly_wage * all$m_t_current_int)

#//MDR patients in intensive phase costs and time
all <- all %>% mutate(cat_current_med3 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos3 = rowSums(dplyr::select(., s_c_med_current, m_med_current_con_mdr), na.rm = TRUE))
all <- all %>% mutate(cat_current_travel3 = rowSums(dplyr::select(., s_c_nmed_current_travel_cost, m_d_current_tra_cost_con_mdr, c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation3 = rowSums(dplyr::select(., s_c_nmed_current_other_nmed, m_d_current_other_nmed_con_mdr, c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food3 = rowSums(dplyr::select(., s_c_nmed_current_food, m_direct_current_food_con_mdr, c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri3 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect3 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))
all$cat_current_indirect3 <- all$cat_current_indirect3 + (all$hourly_wage * all$m_t_current_con_mdr)

#//MDR patients in continuation phase costs and time
all <- all %>% mutate(cat_current_med4 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int_mdr, c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos4 = rowSums(dplyr::select(., s_c_med_current, m_med_current_int_mdr), na.rm = TRUE))
all <- all %>% mutate(cat_current_travel4 = rowSums(dplyr::select(., s_c_nmed_current_travel_cost, m_d_current_tra_cost_int_mdr, c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation4 = rowSums(dplyr::select(., s_c_nmed_current_other_nmed, m_d_current_other_nmed_int_mdr, c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food4 = rowSums(dplyr::select(., s_c_nmed_current_food, m_direct_current_food_int_mdr, c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri4 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect4 = rowSums(dplyr::select(., s_c_time_current, c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))
all$cat_current_indirect4 <- all$cat_current_indirect4 + (all$hourly_wage * all$m_t_current_int_mdr)

#/*without HOSPITALIZATION*/
#//DSTB patients in intensive phase costs and time
all <- all %>% mutate(cat_current_med5 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos5 = 0, na.rm = TRUE)
all <- all %>% mutate(cat_current_travel5 = rowSums(dplyr::select(., c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation5 = rowSums(dplyr::select(., c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food5 = rowSums(dplyr::select(., c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri5 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect5 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))

#//DSTB patients in continuation phase costs and time
all <- all %>% mutate(cat_current_med6 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos6 = 0, na.rm = TRUE)
all <- all %>% mutate(cat_current_travel6 = rowSums(dplyr::select(., c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation6 = rowSums(dplyr::select(.,  c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food6 = rowSums(dplyr::select(.,  c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri6 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect6 = rowSums(dplyr::select(.,  c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))

#//MDR patients in intensive phase costs and time
all <- all %>% mutate(cat_current_med7 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos7 = 0, na.rm = TRUE)
all <- all %>% mutate(cat_current_travel7 = rowSums(dplyr::select(.,  c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation7 = rowSums(dplyr::select(.,  c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food7 = rowSums(dplyr::select(.,  c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri7 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect7 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))

#//MDR patients in continuation phase costs and time
all <- all %>% mutate(cat_current_med8 = rowSums(dplyr::select(., c_med_dot, c_pickup_med, c_fu_med), na.rm = TRUE))
all <- all %>% mutate(cat_current_hos8 = 0, na.rm = TRUE)
all <- all %>% mutate(cat_current_travel8 = rowSums(dplyr::select(., c_nmed_dot_travel, c_pickup_nmed_travel, c_fu_nmed_travel), na.rm = TRUE))
all <- all %>% mutate(cat_current_accommodation8 = rowSums(dplyr::select(., c_fu_nmed_accom), na.rm = TRUE))
all <- all %>% mutate(cat_current_food8 = rowSums(dplyr::select(., c_nmed_dot_food, c_pickup_nmed_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_nutri8 = rowSums(dplyr::select(., c_food), na.rm = TRUE))
all <- all %>% mutate(cat_current_indirect8 = rowSums(dplyr::select(., c_indirect_dot, c_pickup_indirect, c_fu_indirect), na.rm = TRUE))

all$cat_before_med <- all$c_medical_before
all$cat_before_nmed <- all$c_nmedical_before
all$cat_before_indirect <- all$c_indirect_before

all$cat_current_med <- NA
all$cat_current_hos <- NA
all$cat_current_travel <- NA
all$cat_current_accommodation <- NA
all$cat_current_food <- NA
all$cat_current_nutri <- NA
all$cat_current_indirect <- NA

all$cat_current_dot <- all$c_med_dot
all$cat_current_pu  <- all$c_pickup_med
all$cat_current_fu  <- all$c_fu_med


i <- "cat_current_med"
y <- c("cat_current_med", "cat_current_hos", "cat_current_travel", "cat_current_accommodation", "cat_current_food", "cat_current_nutri", "cat_current_indirect")
for (i in y) {
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="no" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 1, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="no" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 2, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="yes" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 3, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="yes" &
                             (all$current_hosp=="yes" | all$prev_hosp=="yes"), all[,paste(i, 4, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="no" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 5, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="no" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 6, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase1" & all$mdr=="yes" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 7, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="yes" &
                             (all$current_hosp=="no" & all$prev_hosp=="no"), all[,paste(i, 8, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(is.na(all[,paste(i)]), 0, all[,paste(i)])
}


all$cat_caregiver <- all$c_guard_tot

all <- all[, -grep("^cat_current.*1$", colnames(all))]
all <- all[, -grep("^cat_current.*2$", colnames(all))]
all <- all[, -grep("^cat_current.*3$", colnames(all))]
all <- all[, -grep("^cat_current.*4$", colnames(all))]
all <- all[, -grep("^cat_current.*5$", colnames(all))]
all <- all[, -grep("^cat_current.*6$", colnames(all))]
all <- all[, -grep("^cat_current.*7$", colnames(all))]
all <- all[, -grep("^cat_current.*8$", colnames(all))]

all <- all %>% mutate(cat_med = rowSums(dplyr::select(., cat_before_med, cat_current_hos, cat_current_dot, cat_current_pu, cat_current_fu), na.rm = TRUE))
all <- all %>% mutate(cat_nmed = rowSums(dplyr::select(., cat_before_nmed, cat_current_travel, cat_current_accommodation, cat_current_food, cat_current_nutri), na.rm = TRUE))

all <- all %>% mutate(cat_direct = rowSums(dplyr::select(., cat_before_med, cat_before_nmed, cat_current_med, cat_current_travel, cat_current_accommodation, cat_current_food, cat_current_nutri), na.rm = TRUE))
all <- all %>% mutate(cat_indirect = rowSums(dplyr::select(., cat_before_indirect, cat_current_indirect), na.rm = TRUE))
all <- all %>% mutate(total_cost_hc = rowSums(dplyr::select(., cat_direct, cat_indirect), na.rm = TRUE))


#//MULTIPLY TOTAL COST IF PATIENTS FROM SAME HOUSEHOLD ARE SURVEYED.
#//one other person in house means costs go up 50%, more than one oteher person doubles costs
#//tab  house_tb_n

#*****************************************************************************
#CATASTROPHIC COST

#Definition 1 (cc1) uses the output based approach:
#HH Income(PRE-TB) - HH Income(POST-TB) + DIRECT COSTS
#------------------------------------------------------  > THRESHOLD
#HH Income(PRE-TB)			

#Definition 2 (cc2) uses the human capital approach, which involves utilizing 
#an hourly wage for each respondent:
#INDIRECT COSTS + DIRECT COSTS
#------------------------------------------------------  > THRESHOLD
#HH Income(PRE-TB)			
#*****************************************************************************/

all$threshold <-0.2

#//Using annualized hh income to calculate the post-pre annual income
all$income_diff_time <- all$treat_duration
all$income_diff_time <- ifelse(all$income_diff_time > 12 & !is.na(all$income_diff_time), 12,all$income_diff_time)
all$income_lost1 <- (all$income_hh_pre-all$income_hh_now)*all$income_diff_time

#//Optional : add the drop in income from onset of symptoms to diagnosis
#//must convert time into months

#all$income_lost2 <- (all$income_hh_diag - all$income_hh_pre) * (all$weeks_before_tx/4.33)

all <- all %>% mutate(income_diff = rowSums(dplyr::select(., income_lost1#, /*income_lost2*/
                                                          ), na.rm = TRUE))

#//if households happen to earn more after TB then change income loss to 0
all$income_diff <- ifelse(all$income_diff<0, 0, all$income_diff)

#//Numerator (pct1_num) for indicator-definition 1
all <- all %>% mutate(pct1_num = rowSums(dplyr::select(., income_diff, cat_direct), na.rm = TRUE))

#//Indicator definition 1
all$pct1 <- all$pct1_num / all$income_hh_pre_annual

#//Indicator-definition 2
all$pct2 <- all$total_cost_hc/all$income_hh_pre_annual

#//Indicator-definition 3
all$pct3 <- all$total_cost_hc/all$expend_hh_annual

#Generating dichotomic results below threshold not catastrophic vs above catastrophic
# output approach
all$cc1 <- NA
all$cc1 <- ifelse(all$pct1 <= all$threshold & !is.na(all$pct1), 0, all$cc1)
all$cc1 <- ifelse(all$pct1 > all$threshold & !is.na(all$pct1), 1, all$cc1)

# hc approach
all$cc2 <- NA
all$cc2 <- ifelse(all$pct2 <= all$threshold & !is.na(all$pct2), 0, all$cc2)
all$cc2 <- ifelse(all$pct2 > all$threshold & !is.na(all$pct2), 1, all$cc2)

# expenditure approach
all$cc3 <- NA
all$cc3 <- ifelse(all$pct3 <= all$threshold & !is.na(all$pct3), 0, all$cc3)
all$cc3 <- ifelse(all$pct3 > all$threshold & !is.na(all$pct3), 1, all$cc3)

#Indicator definition 4- Households dissaving yes/no (dichotomic)
all$cc4 <- NA
all$cc4 <- ifelse(all$coping=="no", 0, all$cc4)
all$cc4 <- ifelse(all$coping=="yes", 1, all$cc4)

#Indicator 5 where only direct medical and non-medical are considered (excluding indirect cost estimates)
all$pct_conservative <- all$cat_direct/all$income_hh_pre_annual
all$cc_conservative <- NA
all$cc_conservative <- ifelse(all$pct_conservative <= all$threshold & !is.na(all$pct_conservative), 0, all$cc_conservative)
all$cc_conservative <- ifelse(all$pct_conservative > all$threshold & !is.na(all$pct_conservative), 1, all$cc_conservative)



#*************************************************
#  Impoverishment
#***************************************************/
# IMPOVERISHMENT INCIDENCE using the PPP$ 1.90/DAY (2011) POVERTY LINE. 
#Variables: 
#  income_hh_pre = monthly household income pre-TB
#hh_members = number of members in the patient's household
#total_cost_output = total costs using output-based approach (not human capital) for indirect costs and direct costs

#*Step 1: Generating poverty threshold
all$hhsize <- all$hhsize_a + all$hhsize_c
all$hhsize <- ifelse(is.na(all$hhsize), median(all$hhsize[!is.na(all$hhsize)], na.rm = T), all$hhsize)

#// Current USD poverty cutoff //
# definition e.g. for Brazil
# 1.9 * PPP conversion factor in 2011 (e.g. 1.47BRL/USD) * Consumer price index in 2021 (=187.1) / Consumer price index in 2011 (=106.6) * household size (from survey data)?
# find the values from https://data.worldbank.org/indicator/FP.CPI.TOTL and https://data.worldbank.org/indicator/PA.NUS.PPP
all$poverty_threshold_month_PPP <- 1.9 * 30.41 * all$hhsize * 896.07*187.4/112.7 #2011 PPP converter (896.07), * 2019 PCI (187.4)/2011 PCI (112.7) 2011&2019 CPI
all$poverty_threshold_year_PPP <- all$poverty_threshold_month_PPP*365

#*Step 2: Number of patients below poverty line
all$below_poverty <- NA
all$below_poverty <- ifelse(all$income_hh_pre_re < all$poverty_threshold_month_PPP & !is.na(all$income_hh_pre_re), 1, all$below_poverty)
all$below_poverty <- ifelse(all$income_hh_pre_re >= all$poverty_threshold_month_PPP & !is.na(all$income_hh_pre_re), 0, all$below_poverty)

#*Step 3: Pushed below poverty line after TB
all$income_hh_pre_annual_reported <- all$income_hh_pre_re*12
all$below_poverty_after <- NA
all$below_poverty_after <- ifelse((all$income_hh_pre_annual_reported - all$pct1_num) < all$poverty_threshold_year_PPP & !is.na(all$income_hh_pre_annual_reported), 1, all$below_poverty_after)
all$below_poverty_after <- ifelse((all$income_hh_pre_annual_reported - all$pct1_num) >= all$poverty_threshold_year_PPP & !is.na(all$income_hh_pre_annual_reported), 0, all$below_poverty_after)


#*******************************************************************
#CONVERT TO USD (vietnam example)
#Using period average of X between July 24th 2016 and 14 October 2016
#Costs are converted to United States Dollars (US$) using the average annual 
#exchange rate during enrollment of US$1 = X.Country units (oanda.com).
#********************************************************************/
#delimit ;
i <- "income_pre_re"
moneyvars <- c("income_pre_re",	"income_hh_pre_re",	"income_hh_pred",	"income_imputed",	"m_income_pre",	"mn_income_pre", "income_hh_pre",	"income_hh_pre_annual",	
               "income_hh_now_annual",	"income_pre_annual",	"income_now_annual",	"c_time_before",	"c_med_before",	"c_nmed_before",
               "c_direct_before",	"s_c_nmed_current_travel_cost",	"s_c_nmed_current_food",	"s_c_nmed_current_other_nmed",	"s_c_time_current",	
               "s_c_med_current",	"s_c_nmed_current",	"s_c_direct_current",	"c_dot_time",	"c_direct_dot_per",	"c_med_dot",	"c_nmed_dot_travel",	
               "c_nmed_dot_food",	"c_direct_dot",	"c_indirect_dot",	"c_dot",	"c_pickup_nmed_per",	"c_pickup_med_per",	"c_pickup_nmed",	
               "c_pickup_med",	"c_pickup_direct",	"c_pickup_nmed_accommodation",	"c_pickup_nmed_travel",	"c_pickup_nmed_food",	"c_pickup_indirect",
               "c_pickup",	"c_fu_nmed_per",	"c_fu_med_per",	"c_fu_nmed_travel",	"c_fu_nmed_accom",	"c_fu_nmed",	"c_fu_med",	"c_fu_direct",
               "c_fu_indirect",	"c_fu",	"c_supp",	"c_extra",	"c_food",	"caregiver_wage_1",	"caregiver_wage_2",	"caregiver_wage",	"c_guard_dot",	
               "c_guard_pickup",	"c_guard_fu",	"c_guard_hosp",	"c_guard_tot",	"coping_amount",	"m_med_before",	"m_nmed_before",	"m_direct_before",
               "m_med_before_mdr",	"m_nmed_before_mdr",	"m_direct_before_mdr",	"m_c_time_before",	"m_c_time_before_mdr",	"m_med_current_int",
               "m_nmed_current_int",	"m_direct_current_int",	"m_direct_current_travel_cost_int",	"m_direct_current_other_nmed_int",	
               "m_direct_current_food_int",	"m_med_current_con",	"m_nmed_current_con",	"m_direct_current_con",	"m_direct_current_travel_cost_con",
               "m_direct_current_other_nmed_con",	"m_direct_current_food_con",	"m_c_time_current_int",	"m_c_time_current_con",
               "m_med_current_int_mdr",	"m_nmed_current_int_mdr",	"m_direct_current_int_mdr",	"m_d_current_tra_cost_int_mdr",	
               "m_d_current_other_nmed_int_mdr",	"m_direct_current_food_int_mdr",	"m_med_current_con_mdr",	"m_nmed_current_con_mdr",
               "m_direct_current_con_mdr",	"m_d_current_tra_cost_con_mdr",	"m_d_current_other_nmed_con_mdr",	"m_direct_current_food_con_mdr",
               "m_c_time_current_int_mdr",	"m_c_time_current_con_mdr",	"c_medical", "c_med_hosp", "c_nmed_hosp",	"c_medical_before",	"c_medical_after",	"c_nmedical",
               "c_nmedical_before",	"c_nmedical_after",	"c_indirect",	"c_indirect_before",	"c_indirect_after",	"cat_before_med",	"cat_before_nmed",
               "cat_before_indirect",	"cat_current_med", "cat_current_hos", "cat_current_dot", "cat_current_pu", "cat_current_fu",	"cat_current_travel",	"cat_current_accommodation",	"cat_current_food",	"cat_current_nutri",
               "cat_current_indirect",	"cat_caregiver", "cat_med", "cat_nmed",	"cat_direct",	"cat_indirect",	"total_cost_hc",	"income_lost1",	"income_diff",	"pct1_num",	
               "income_pre",	"income_now", "expend_hh", "expend_hh_annual"
)


#//convert to USD. replace 1 with exchange rate of local currency to USD
for (i in moneyvars) {
  all[,paste(i)] <- all[,paste(i)]/2307 # use the average value of UNORE: https://treasury.un.org/operationalrates/OperationalRates.php
}

#//Identify outliers
#foreach var of varlist cat_* {    
#quietly summarize `var'    
#cap g Z_`var'= (`var' > 6*r(sd)) if `var' < .      
#list `var' Z_`var' if Z_`var' == 1
#}


################################################################################################################
# Save R data
################################################################################################################
save.image("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/TBPCS_iso3_imputed.RData")

# write.xlsx(all, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/main_imp.xlsx",
#            sheetName = "main", append = FALSE)
# 
