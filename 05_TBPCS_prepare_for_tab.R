#  
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
library("htmlTable") 
library("boot") 
library("table1")
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
# Load imputed data
################################################################################################################
load("./TBPCS_iso3_imputed.Rdata")

######################################
# Working hours lost
######################################
# patients
all$t_before
all$m_t_before
all$t_hosp
all$t_dot
all$t_pickup
all$t_fu

all <- all %>% mutate(t_total1 = rowSums(dplyr::select(., t_before, t_hosp, t_dot, t_pickup, t_fu), na.rm = T))
all <- all %>% mutate(t_total2 = rowSums(dplyr::select(., m_t_before, t_hosp, t_dot, t_pickup, t_fu), na.rm = T))
all <- all %>% mutate(t_total3 = rowSums(dplyr::select(., m_t_before_mdr, t_hosp, t_dot, t_pickup, t_fu), na.rm = T))
all$t_total <- NA

i <- "t_total"
y <- c("t_total")
for (i in y) {
  all[,paste(i)] <- ifelse(all$phase=="phase1", all[,paste(i, 1, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="no", all[,paste(i, 2, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(all$phase=="phase2" & all$mdr=="yes", all[,paste(i, 3, sep="")], all[,paste(i, sep="")])
  all[,paste(i)] <- ifelse(is.na(all[,paste(i)]), 0, all[,paste(i)])
}

# hh members
all$t_guard_hosp	
all$t_guard_dot	
all$t_guard_pickup	
all$t_guard_fu	
all$t_guard_tot

######################################
# Demographic information 
######################################
all$sex <- as.character(all$sex)

all$hiv <- as.character(all$hiv)
all$hiv <- ifelse(all$hiv=="hiv_pos", "Positive",
                  ifelse(all$hiv=="hiv_neg", "Negative", "Unknown"))
all$insurance <- as.character(all$insurance)
all$insurance <- ifelse(all$insurance=="no", "No insurance","With insurance")

exclude({ #please arrange according to each country 
all$educ_level <- as.character(all$educ_level)
all$educ_level <- ifelse(all$educ_level=="educ1"|all$educ_level=="educ2"|all$educ_level=="educ3", "no education",
                         ifelse(all$educ_level=="educ4"|all$educ_level=="educ5", "Pre- or primary school",
                                ifelse(all$educ_level=="educ6", "secondary school",
                                       ifelse(all$educ_level=="educ7"|all$educ_level=="educ8"|all$educ_level=="other", "university/vocational/other", NA))))
})

all$mdr <- ifelse(all$mdr=="no", "DS-TB","DR-TB")

exclude({ #please arrange according to each country 
  
all$employ_before <- ifelse(all$employ_before=="employ_any1", "Employed",
                                 ifelse(all$employ_type_before=="employ_any6", "Unemployed", "Student/Retired/Household work/Other"))
all$employ_before<-factor(all$employ_type_before,levels=c("Employed","Unemployed","Student/Retired/Household work/Other"))

all$employ_type_before <- as.character(all$employ_type_before)
all$employ_type_before <- ifelse(all$employ_type_before=="employ_any1", "Employed",
                                 ifelse(all$employ_type_before=="employ_any6", "Unemployed", "Student/Retired/Household work/Other"))
all$employ_type_before<-factor(all$employ_type_before,levels=c("Employed","Unemployed","Student/Retired/Household work/Other"))
})

######################################
# Clinical information 
######################################
exclude({ #please arrange according to each country 
  
all$facility_cat <- as.character(all$facility_cat)
all$facility_cat <- ifelse(all$facility_cat=="psi", "Faith based health center or hospital",
                           ifelse(all$facility_cat=="pubphcf", "Government primary health care facility",
                                  ifelse(all$facility_cat=="pubhosp", "Government hospital", "Other")))
all$facility_cat<-factor(all$facility_cat,levels=c("Government primary health care facility","Government hospital","Faith based health center or hospital", "Other"))


all$tb_type <- as.character(all$tb_type)
all$diag_mode <- ifelse(all$tb_type=="tb_type1"|all$tb_type=="tb_type3", "Bacteriologically confirmed", "Clinically diagnosed")

all$dst <- as.character(all$dst)
all$dst <- ifelse(all$dst=="yes", "DST done", "DST not done")
})


all$phase <- as.character(all$phase)
all$phase <- ifelse(all$phase=="phase1", "Intensive phase", "Continuation phase")
all$phase<-factor(all$phase,levels=c("Intensive phase","Continuation phase"))

all$duration_int
all$duration_cont

all$treat_group_nmdr <- as.character(all$treat_group_nmdr)
all$treat_group_nmdr <- ifelse(all$treat_group_nmdr=="tx_gp_nmdr1", "New",
                               ifelse(all$treat_group_nmdr=="tx_gp_nmdr2", "Relapse",
                                      ifelse(all$treat_group_nmdr=="tx_gp_nmdr3"|all$treat_group_nmdr=="tx_gp_nmdr4", "Retreatment", "Unknown")))
# all$treat_group_nmdr<-factor(all$treat_group_nmdr,levels=c("New", "Relapse", "Retreatment"))

all$treat_group_mdr <- as.character(all$treat_group_mdr)
all$treat_group_mdr <- ifelse(all$treat_group_mdr=="tx_gp_mdr1", "New",
                               ifelse(all$treat_group_mdr=="tx_gp_mdr2", "Relapse",
                                      ifelse(all$treat_group_mdr=="tx_gp_mdr3"|all$treat_group_nmdr=="tx_gp_mdr4", "Retreatment", "Unknown")))
# all$treat_group_mdr<-factor(all$treat_group_mdr,levels=c("New", "Relapse", "Retreatment","Other"))

all$treat_group <- ifelse(all$mdr=="DS-TB", all$treat_group_nmdr,
                          ifelse(all$mdr=="DR-TB", all$treat_group_mdr, NA))

all$treat_group <- ifelse(is.na(all$treat_group), "Unknown", all$treat_group)

all$treat_group <- factor(all$treat_group,levels=c("New", "Relapse", "Retreatment","Unknown"))


######################################
# mode of care
######################################
#hospitalization
all$current_hosp <- ifelse(all$current_hosp!="yes", "no", all$current_hosp)
all$prev_hosp <- ifelse(all$prev_hosp!="yes", "no", all$prev_hosp)

all$s_t_stay_current

#ambulatory care
all$amb <- NA
all$sx_times <- all$visit_before
all$n_dot_visits
all$n_pickup_total
all$s_fu

all <- all %>% 
  mutate(visit_tot = rowSums(dplyr::select(., sx_times, n_dot_visits, n_pickup_total, s_fu), na.rm = T))
all$visit_tot

#DOT in the current phase
all$self_admin
all$self_admin_current_int <- ifelse(all$phase=="Continuation phase",NA,
                                     ifelse(all$current_hosp=="yes","Hospitalized",
                                     ifelse(all$current_hosp=="no"&all$self_admin=="dot1","Self-administered",
                                            ifelse(all$current_hosp=="no"&all$self_admin=="dot2","Facility-based DOT",
                                                   ifelse(all$current_hosp=="no"&all$self_admin=="dot3","Community-based DOT",
                                                          ifelse(all$current_hosp=="no"&all$self_admin=="","Unknown",0
))))))

all$self_admin_current_con <- ifelse(all$phase=="Intensive phase",NA,
                                     ifelse(all$current_hosp=="yes","Hospitalized",
                                            ifelse(all$current_hosp=="no"&all$self_admin=="dot1","Self-administered",
                                                   ifelse(all$current_hosp=="no"&all$self_admin=="dot2","Facility-based DOT",
                                                          ifelse(all$current_hosp=="no"&all$self_admin=="dot3","Community-based DOT",
                                                                 ifelse(all$current_hosp=="no"&all$self_admin=="","Unknown",0
                                                                 ))))))




#delay
all$weeks_before_tx
all$delay <- as.character(all$delay)
all$delay <- ifelse(all$delay=="1", "Yes", "No")


######################################
# coping mechanism
######################################
all$dissavings <- ifelse(all$dissavings!="yes", "no", all$dissavings)
all$borrow <- ifelse(all$borrow!="yes", "no", all$borrow)
all$assetsale <- ifelse(all$assetsale!="yes", "no", all$assetsale)
all$coping <- ifelse(all$coping!="yes", "no", all$coping)

######################################
# impact & consequenses
######################################
all$cope_impact <- as.character(all$cope_impact)
all$cope_impact <- ifelse(all$cope_impact=="cope_impact1", "no impact",
                          ifelse(all$cope_impact=="cope_impact2", "little impact",
                                 ifelse(all$cope_impact=="cope_impact3", "moderate impact",
                                        ifelse(all$cope_impact=="cope_impact4", "serious impact", "very serious impact"))))

all$cope_impact<-factor(all$cope_impact,levels=c(#"no impact",
  "little impact","moderate impact","serious impact"#, "very serious impact"
  ))

all$dropout <- as.character(all$dropout)
all$divorce <- as.character(all$divorce)
all$insec <- as.character(all$insec)
all$exclusion <- as.character(all$exclusion)
all$lossofjob <- as.character(all$lossofjob)
all$any_socialeffect <- as.character(all$any_socialeffect)

######################################
# impoverishment
######################################
all$below_poverty <- as.character(all$below_poverty)
all$below_poverty_after <- as.character(all$below_poverty_after)

######################################
# SP and voucher
######################################
all$sp <- as.character(all$sp)
all$sp <- ifelse(all$sp!="yes", "no", all$sp)

exclude({
all$sp_type <- ifelse(all$sp_type=="sp_mp", "from member of Parliament", 
                      ifelse(all$sp_type=="other", "from family/relatives", NA))
})

all$vouchers <- as.character(all$vouchers)
all$vouchers <- ifelse(all$vouchers!="yes", "no", all$vouchers)

exclude({
all$voucher_type <- ifelse(all$voucher_type=="voucher_type1", "travel voucher",
                           ifelse(all$voucher_type=="voucher_type2", "food support",
                                  ifelse(all$voucher_type=="other", "other", NA)))
})

all$voucher_type <- ifelse(all$vouchers=="yes"&is.na(all$voucher_type), "unknown",all$voucher_type)
                           

######################################
# CC figure
######################################
## simple version from Nobu's generic
# TB patient costs
# all sites
library("Rmisc") # for ci calc

all$cat <- ifelse(all$mdr=="DS-TB", 1, 2)

cc_all <- CI(all$cc1)*100
cc_ds  <- group.CI(cc1~cat, all)*100
cc_ds <- melt(cc_ds, id.vars = c("cat"))
cc_ds <- dcast(cc_ds, variable ~ cat, value.var = "value")
cc_ds <- subset(cc_ds, select=-c(variable))
colnames(cc_ds)[colnames(cc_ds)==100] <- "DS-TB"
colnames(cc_ds)[colnames(cc_ds)==200] <- "DR-TB"

# all merged
# TB
cc <- data.frame(cc_all, cc_ds)
cc = t(cc) #flip row and column
grp <- c("All", "DS-TB", "DR-TB")
cc <- data.frame(cc, grp)

## Lao version with 2 axis
c_all  <- median(all$pct1_num, na.rm = T)
c_dstb  <- median(all$pct1_num[all$mdr=="DS-TB"], na.rm = T)
c_drtb <- median(all$pct1_num[all$mdr=="DR-TB"], na.rm = T)
cost   <- rbind.data.frame(c_all, c_dstb, c_drtb)
colnames(cost) <- "cost"

cc  <- cbind.data.frame(cc, cost)


########################
# cost driver
########################
# TB costs
library("Hmisc") # for describe
all$med_prop <- ifelse(is.na(all$cat_med / all$pct1_num), 0, all$cat_med / all$pct1_num)
all$nmed_prop <- ifelse(is.na(all$cat_nmed / all$pct1_num), 0, all$cat_nmed / all$pct1_num) 
all$loss_prop <- ifelse(is.na(all$income_diff / all$pct1_num), 0, all$income_diff / all$pct1_num)

i <- "cat_before_med"
y <- c("cat_before_med", "cat_current_hos", "cat_current_dot", "cat_current_pu", "cat_current_fu")
for (i in y) {
  all[,paste(i)] <- ifelse(is.na(all[,paste(i)]),0,all[,paste(i)])
}
  
# all$before_prop <- ifelse(is.na(all$cat_before_med / all$cat_med), NA, all$cat_before_med / all$cat_med) 
# all$hos_prop    <- ifelse(is.na(all$cat_current_hos / all$cat_med), NA, all$cat_current_hos / all$cat_med) 
# all$dot_prop    <- ifelse(is.na(all$cat_current_dot / all$cat_med), NA, all$cat_current_dot / all$cat_med) 
# all$pu_prop     <- ifelse(is.na(all$cat_current_pu / all$cat_med), NA, all$cat_current_pu / all$cat_med) 
# all$fu_prop     <- ifelse(is.na(all$cat_current_fu / all$cat_med), NA, all$cat_current_fu / all$cat_med) 

all$before_prop <- ifelse(all$cat_med==0, NA, all$cat_before_med / all$cat_med) 
all$hos_prop    <- ifelse(all$cat_med==0, NA, all$cat_current_hos / all$cat_med) 
all$dot_prop    <- ifelse(all$cat_med==0, NA, all$cat_current_dot / all$cat_med) 
all$pu_prop     <- ifelse(all$cat_med==0, NA, all$cat_current_pu / all$cat_med) 
all$fu_prop     <- ifelse(all$cat_med==0, NA, all$cat_current_fu / all$cat_med) 

describe(all$before_prop)
describe(all$hos_prop)
describe(all$dot_prop)
describe(all$pu_prop)
describe(all$fu_prop)



all$before_prop <- ifelse(is.na(all$cat_before_nmed / all$cat_nmed), 0, all$cat_before_nmed / all$cat_nmed) 
all$nutri_prop <- ifelse(is.na(all$cat_current_nutri / all$cat_nmed), 0, all$cat_current_nutri / all$cat_nmed) 
all$travel_prop <- ifelse(is.na(all$cat_current_travel / all$cat_nmed), 0, all$cat_current_travel / all$cat_nmed) 
all$accom_prop <- ifelse(is.na(all$cat_current_accommodation / all$cat_nmed), 0, all$cat_current_accommodation / all$cat_nmed) 
all$food_prop <- ifelse(is.na(all$cat_current_food / all$cat_nmed), 0, all$cat_current_food / all$cat_nmed) 
describe(all$nutri_prop)
describe(all$before_prop)
describe(all$travel_prop)
describe(all$food_prop)
describe(all$accom_prop)


med <- mean(all$med_prop)*100
nmed <- mean(all$nmed_prop)*100
loss <- mean(all$loss_prop)*100
driver <- data.frame(med, nmed, loss)

med_ds  <- group.CI(med_prop~cat, all)*100
med_ds <- subset(med_ds, select=-c(cat,med_prop.upper, med_prop.lower))
nmed_ds  <- group.CI(nmed_prop~cat, all)*100
nmed_ds <- subset(nmed_ds, select=-c(cat,nmed_prop.upper, nmed_prop.lower))
loss_ds  <- group.CI(loss_prop~cat, all)*100
loss_ds <- subset(loss_ds, select=-c(cat,loss_prop.upper, loss_prop.lower))

driver_ds <- cbind.data.frame(med_ds, nmed_ds, loss_ds)

colnames(driver_ds)[colnames(driver_ds)=="med_prop.mean"] <- "med"
colnames(driver_ds)[colnames(driver_ds)=="nmed_prop.mean"] <- "nmed"
colnames(driver_ds)[colnames(driver_ds)=="loss_prop.mean"] <- "loss"

driver <- rbind.data.frame(driver, driver_ds)
grp <- c("All", "DS-TB", "DR-TB")
driver <- cbind.data.frame(driver, grp)
driver$grp = factor(driver$grp, levels=c('DS-TB','DR-TB','All')) # define order as All < DS-TB < DR-TB
driver <- melt(driver, id.vars = c("grp"))

# final adjustment for data label in barchart
detach("package:Rmisc", unload=T)
detach("package:reshape2", unload=T)
detach("package:plyr", unload=T)
library("dplyr")
driver <- driver %>% group_by(grp) %>% mutate(cum = cumsum(value)-(value*0.5))

########################
# changes of employment status by river plot
########################
exclude({
library(riverplot)
library(RColorBrewer)

all$employ <- as.character(all$employ)
all$employ <- ifelse(all$employ=="employ_any1", "Employed",
                                 ifelse(all$employ=="employ_any6", "Unemployed", "Student/Retired/Household work/Other"))
all$employ<-factor(all$employ,levels=c("Employed","Unemployed","Student/Retired/Household work/Other"))

all$count <- 1

emp <- all %>%
  select(count, employ, employ_type_before)

emp$employ_before <- ifelse(emp$employ_type_before=="Unemployed", "unemp1", 
                     ifelse(emp$employ_type_before=="Employed", "emp1","oth1"))

emp$employ_after <- ifelse(emp$employ=="Unemployed", "unemp2", 
                           ifelse(emp$employ=="Employed", "emp2","oth2"))

#defining the edges
edges <- aggregate(count ~ employ_before+employ_after, data=emp, sum)

colnames(edges) <- c(
    "N1",
    "N2",
    "Value")

#defining the nodes
nodes <- data.frame(ID = c("emp1", "unemp1", "oth1", "emp2", "unemp2", "oth2"))

nodes$x = c(1,1,1,2,2,2)

nodes$y = c(1,2,3,1,2,3)

nodes$labels = c("Employed","Unemployed", "Other","Employed","Unemployed", "Other")

#picking colours
palette = paste0(brewer.pal(6, "Set1"), "70")

#plot styles
styles = lapply(nodes$y, function(n) {
  list(col = palette[n], lty = 0, textcol = "black")
})

#matching nodes to names
names(styles) = nodes$ID

#defining the river
r <- makeRiver( nodes, edges,
                node_styles = styles)

# for Solomon not useful
#plot(r, plot_area = 0.7, srt=0, yscale=0.00315, nsteps = 30, default_style=list(srt=0, textcol="grey20"))

})

########################
### variable manupulations ####
########################
library("forcats")
all <- all %>% 
  #replace_na(list(visit_before=0, n_dot_visits=0, n_pickup_total=0, s_fu=0)) %>% 
  #mutate(total_visits=rowSums(dplyr::select(., visit_before, n_dot_visits, n_pickup_total, s_fu), na.rm = TRUE)) %>% 
  mutate(age_c = cut(age,breaks=c(0,15,25,35,45,55,65,Inf),right=FALSE,
                     labels = c("0-14","15-24","25-34","35-44","45-54","55-64","65+"))) %>% 
  # mutate(age_c2 = cut(age,breaks=c(0,15,25,35,45,55,Inf),right=FALSE,
  #                    labels = c("0-14","15-24","25-34","35-44","45-54","55+"))) %>% 
  mutate(hhsize_cat = cut(hhsize,breaks=c(0,7,Inf),right=FALSE,
                     labels = c("0-7","8+"))) %>% 
  # mutate(mdr=factor(mdr,labels=c("DS-TB","DR-TB"))) %>% 
  #mutate(hiv=factor(hiv,labels=c("Negative","Positive","Unknown"))) %>% 
  #mutate(self_admin=factor(self_admin,labels=c("Self-administration","Home-based DOT", "Facility-based DOT"))) %>% 
  mutate(cc1_f=factor(cc1, labels=c('No','Yes'))) %>% 
  #mutate(educ_level=factor(educ_level,labels=c(" ","No education","Pre-primary","Primary education",
  #                                             "Post-primary training",'Secondary "O" level',
  #                                             'Post-secondary "O" level training',
  #                                             'Secondary "A" level',
  #                                             "Diploma","University","Don't Know"))) %>% 
  # mutate(below_poverty=factor(below_poverty)) %>% 
  #mutate(below_poverty_tshs=factor(below_poverty_tshs,labels=c("No","Yes"))) %>% 
  mutate(hh_quintile=factor(hh_quintile, 
                            labels=c("Poorest","Second","Third","Fourth","Wealthiest"))) %>% 
  mutate(hh_quintile_r=fct_rev(hh_quintile)) %>% 
  mutate(current_hosp=factor(current_hosp, labels=c('No','Yes'))) %>% 
  mutate(prev_hosp=factor(prev_hosp, labels=c('No','Yes'))) %>% 
  mutate(mdr=factor(mdr, levels=c('DS-TB','DR-TB'))) %>% 
  mutate(sex=factor(sex, labels=c('Female','Male'))) %>%
  mutate(self_admin_current_int=factor(self_admin_current_int, levels=c('Hospitalized','Self-administered','Community-based DOT','Facility-based DOT'))) %>%
  mutate(self_admin_current_con=factor(self_admin_current_con, levels=c('Hospitalized','Self-administered','Community-based DOT','Facility-based DOT'))) %>%
  # mutate(hh_decile=factor(hh_decile, labels=c('Poorest','Second','Third','Fourth'))) %>%
  # mutate(yr_tb_f=factor(year_register, labels=c('2017','2018','2019'))) %>%
  mutate(emp_main=factor(emp_main, labels=c('yes','no','equal','no_income'))) %>%
  #mutate(all__1=factor(1,labels=c("Total"))) %>% 
  #mutate(all__2=factor(1,labels=c("Total"))) %>% 
  #mutate(t_before=replace_na(t_before,0),  # replace na in summary time variables 
  #       t_current=replace_na(t_current,0)) %>% 
  #mutate(delay=factor(replace_na(delay,0),labels=c("No delay","Delayed"))) %>% 
  #mutate(insurance=factor(insurance,labels=c("No","Yes"))) %>% 
  mutate(ur_ru=factor(ur_ru,labels=c('Rural','Semi-urban','Urban')))

#  mutate(___=factor(___, levels=c('__','__'))) %>% 
# mutate(cc1=factor(cc1, labels=c('No','Yes'))) %>% 


################################################################################################################
# Rearranging var categorization for Odds Ratio
################################################################################################################
# all <- all %>% 
#   mutate(hh_quintile_r2=ifelse(hh_quintile_r=="Poorest"|hh_quintile_r=="Second"|hh_quintile_r=="Third",3,hh_quintile_r)) %>%
#   mutate(hh_quintile_r2=factor(hh_quintile_r2, 
#                             labels=c("Wealthiest","Fourth","Poorest/Second/Third"))) %>%
#   mutate(educ_level_r=fct_rev(educ_level)) %>% 
#   mutate(educ_level_r2=ifelse(educ_level_r=="no education"|educ_level_r=="Pre- or primary school",3,educ_level_r)) %>%
#   mutate(educ_level_r2=factor(educ_level_r2, 
#                              labels=c("university/vocational/other","secondary school","no education or pre/primary school")))
#   
  
library("labelled")
var_label(all) <- list(
  age            = "Age",
  age_c          = "Age group",
  # age_c2          = "Age group",
  sex            = "Sex",
  tb_type        = "Type of TB",
  phase          = "Treatment phase",
  current_hosp   = "Hospitalized at time of interview",
  prev_hosp      = "Hospitalized during current phase",
  mdr            = "Drug resistance status",
  hiv            = "Recorded HIV status",
  self_admin     = "Treatment modality",
  self_admin_current_int     = "Mode of supervision for TB treatment: in intensive phase",
  self_admin_current_con     = "Mode of supervision for TB treatment: in continuation phase",
  hh_quintile    = "Income quintile",
  hh_quintile_r  = "Income quintile",
  # hh_quintile_r2  = "Income quintile",
  educ_level     = "Education level",
  # educ_level_r2     = "Education level",
  #t_stay_current = "Days hospitalized during current phase",
  #total_visits   = "Number of visits per episode: Total",
  #visit_before   = "Number of visits: Pre-diagnosis",
  #n_dot_visits   = "Number of visits: DOT",
  #s_fu           = "Number of visits: follow-up",
  #n_pickup_total = "Number of visits: Drug pick-up",
  weeks_before_tx= "Treatment delay (week)",
  duration_int   = "Treatment duration for intensive phase",
  duration_cont  = "Treatment duration for continuation phase",
  income_hh_pre_re = "Reported monthly household income (prediagnosis)",
  income_hh_pre_annual = "Annual household income (prediagnosis)",
  expend_hh_annual = "Annual household expediture",
  expend_hh = "Monthly household expediture",
  #below_poverty_tshs  = "Below poverty line",
  below_poverty  = "Below poverty line",
  facility_type  = "Type of health facility",
  duration_int   = "Intensive phase (month)",
  duration_cont  = "Continuation phase (month)",
  delay          = "Delay before diagnosis",
  insurance      = "Insurance",
  hhsize         = "Household size",
  hhsize_cat        = "Household size",
  ur_ru          = "Urban/rural"
  #all__1         = "Total"
)


################################################################################################################
# Save R data
################################################################################################################
save.image("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/TBPCS_iso3_complete.RData")

