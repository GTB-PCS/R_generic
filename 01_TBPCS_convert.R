setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- "C:/Users/Takuya"
  }
  base::setwd(dir)
}

setwd()
setwd("./Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/pull/output")

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

################################################################################################################
# Importing raw dataset
################################################################################################################
# enrolment forms
# version 1_5

all    <- read.csv("tanzania7.csv")
tx <- read.csv("tanzania7_g3_repeat_4.csv") # sub df for previous care seeking
hp <- read.csv("tanzania7_g4_repeat5.csv") # sub df for hospitalization

################################################################################################################
# Rename
################################################################################################################
library("readxl")

rename <- function(x) {
 names(x) <- sub("g1.*\\.", "", names(x))
 # names(x) <- sub("g1b.*\\.", "", names(x))
 names(x) <- sub("g2.*\\.", "", names(x))
 names(x) <- sub("g2a.*\\.", "", names(x))
 names(x) <- sub("g2b.*\\.", "", names(x))
 names(x) <- sub("g3.*\\.", "", names(x))
 names(x) <- sub("g4.*\\.", "", names(x))
 names(x) <- sub("g5.*\\.", "", names(x))
 names(x) <- sub("g6.*\\.", "", names(x))
 names(x) <- sub("g7.*\\.", "", names(x))
 }

names(all) <- rename(all)


# ################################################################################################################
# # Appending all the versions by rbind
# ################################################################################################################
# library(plyr)
# 
# # merge forms
# all <- rbind.fill(all_v15, all_v22, all_v25)
# do <- rbind.fill(do_v15, do_v22, do_v25)
# hs <- rbind.fill(hs_v15, hs_v22, hs_v25)
# tx <- rbind.fill(tx_v15, tx_v22, tx_v25)
# hp <- rbind.fill(hp_v15, hp_v22, hp_v25)
# 
# # drop empty data in subdatasets: this is due to misdesign of questionnaire form!
# do<-subset(do, !is.na(do$dropout_age))
# hs<-subset(hs, !is.na(hs$tb_tx_year))
# hp<-subset(hp, hp$hosp_type!="")
# 
# # drop forms with version number
# drop <- c("all_v15", "all_v22", "all_v25",
#           "do_v15", "do_v22", "do_v25",
#           "hp_v15", "hp_v22", "hp_v25",
#           "hs_v15", "hs_v22", "hs_v25",
#           "rnm_15", "rnm_22", "rnm_25",
#           "tx_v15", "tx_v22", "tx_v25")
# 
# rm(list=drop)

# ################################################################################################################
# # Export raw data
# ################################################################################################################
# 
# # export main data.frames as excel for checking missing submission and rename
# # main data
# write.xlsx(all, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/main.xlsx",
#            sheetName = "main", append = FALSE)
# # interrupted schooling
# write.xlsx(do, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/do.xlsx",
#            sheetName = "do", append = FALSE)
# # hospitalization
# write.xlsx(hp, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/hp.xlsx",
#            sheetName = "hp", append = FALSE)
# # past TB history
# write.xlsx(hs, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/hs.xlsx",
#            sheetName = "hs", append = FALSE)
# # Pre-disease visits
# write.xlsx(tx, file = "C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/1.pull/checker/tx.xlsx",
#            sheetName = "tx", append = FALSE)
# 
################################################################################################################
# # Save R data
# ################################################################################################################
# save.image("C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/pcs_slb_renamed.RData")
# load("C:/Users/Takuya/Dropbox/11.STB_Docs/STC_Yamanaka_T/SLB_PCS/pcs_slb_renamed.RData")

################################################################################################################
# Reshaping long to wide for sub-data.frames
################################################################################################################
# Reshape from long to wide 

hp$PARENT_KEY <- as.character(hp$PARENT_KEY)
hp$num <- sequence(rle(hp$PARENT_KEY)$lengths)
hp <- reshape(hp, idvar='PARENT_KEY', timevar='num', direction='wide')

tx$PARENT_KEY <- as.character(tx$PARENT_KEY)
tx$num <- sequence(rle(tx$PARENT_KEY)$lengths)
tx <- reshape(tx, idvar='PARENT_KEY', timevar='num', direction='wide')


################################################################################################################
# Drop note and autmatically generated vars
################################################################################################################
library(tidyverse)

#y <- list(ec_hp, e_tx,  mc_hp, ec_ic, mc_ic, i_dm,  i_do,  e_do,  ec_dm, ec_do,
#       mc_dm, mc_do, mc_all,i_all, i_hh,  i_hp,  e_hh,  i_ic,  ec_hh,
#       mc_hh, e_ib,  e_ic,  ec_all,e_all)

remove <- function(x) {
   x %>% select(-contains("_note"))
}

all <- remove(all)
# do <- remove(do)
hp <- remove(hp)
# hs <- remove(hs)
tx <- remove(tx)

################################################################################################################
# Save R data
################################################################################################################
save.image("C:/Users/Takuya/Dropbox/10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/R_data/pcs_xxx_reshaped.RData")
