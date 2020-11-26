setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- "C:/Users/Takuya"
  }
  base::setwd(dir)
}

setwd()

setwd("./Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/")

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
library("readxl")


################################################################################################################
# Load appended and renamed data
################################################################################################################
load("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/pcs_xxx_reshaped.RData")

################################################################################################################
# data cleaning: character to numeric * this is tanzania specific arrangement due to the questionnaire design
################################################################################################################
raw_all <- all

# replace "unknown" and remove comma
chr_vars <- c("income_pre","income_hh_pre","income_now","income_hh_now",
              "ex_educ","ex_health","ex_farm","ex_food","ex_cloth","ex_utility",
              "ex_fuel","ex_house","ex_capital","ex_rent","ex_tax","ex_drink",
              "ex_debt","ex_transport","ex_repair","ex_stationery","ex_cosme",
              "ex_deposit","ex_loss","ex_gamble","ex_donation","ex_legal",
              "ex_personal","ex_other","borrow4","asset_proceeds1"
              )

## replacing "unknown"
for (i in chr_vars) {
  all[,paste(i)] <- gsub(".*(The |Unknown|Not working|Uknown|unknown|Ukown|Uknwn|Known|O|P).*", NA, all[,paste("income_pre")])
}

## removing comma
for (i in chr_vars) {
  all[,paste(i)] <- as.numeric(gsub(",", "", gsub("\\.", "", all[,paste(i)])))
}


################################################################################################################
# check number of samples by province
################################################################################################################
# all$count<-1
# subs <- all %>%
#   select(province, count, phase) 
# subs <- aggregate(. ~ province+phase, data=subs, sum)
# subs <- dcast(subs, province ~ phase, value.var = "count")
# subs <- replace(subs,is.na(subs),0)
# subs <- subs %>% mutate(total = phase1 + phase2)
# names(subs) <- c("province", "actual int", "actual cont", "total actual")
# 
# target <- read_excel("C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/target_samplesize.xlsx", sheet = "Sheet1")
# 
# subs <- merge(x=subs, y=target[, c("province",
#                                    "target int",
#                                    "target cont",
#                                    "total target")], by = "province", all.x=T)
# subs <- subs %>% mutate(diff = `total actual`- `total target`) 
# 
# 

################################################################################################################
# validation of hh income
################################################################################################################
# hh income pre-disease
# inc_pre<-all[(!is.na(all$income_pre)&!is.na(all$income_hh_pre)&all$income_pre>all$income_hh_pre)|
#                (!is.na(all$income_pre)&is.na(all$income_hh_pre)), 
#              c("pt_idnum","date_int","interviewer","income_pre","income_hh_pre")]
# 
# # hh income now
# inc_now<-all[(!is.na(all$income_now)&!is.na(all$income_hh_now)&all$income_now>all$income_hh_now)|
#                (!is.na(all$income_now)&is.na(all$income_hh_now)), 
#              c("pt_idnum","date_int","interviewer","income_now","income_hh_now")]
# 
# ################################################################################################################
# # For PIs data validation
# ################################################################################################################
# v1<-subset(raw_all, is.na(raw_all$phase_days))
# v2<-subset(raw_all, ((!is.na(raw_all$income_pre)&!is.na(raw_all$income_hh_pre))&raw_all$income_pre>raw_all$income_hh_pre)|
#              (!is.na(raw_all$income_pre)&is.na(raw_all$income_hh_pre)))
# v3<-subset(raw_all, ((!is.na(raw_all$income_now)&!is.na(raw_all$income_hh_now))&raw_all$income_now>raw_all$income_hh_now)|
#              (!is.na(raw_all$income_now)&is.na(raw_all$income_hh_now)))
# v <- rbind.data.frame(v1, v2, v3)
# 
# write.xlsx(v, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/slb_validation_required.xlsx",
#            sheetName = "main", append = FALSE)
# 

################################################################################################################
# Save R data
################################################################################################################
save.image("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/TBPCS_iso3_cleaned.RData")
