library(tidyverse)
library(lubridate)
library(stringr)

in_path = '/global/project/queens-mma/scene-csv/sample03/raw/'
out_path = '/global/project/queens-mma/scene-csv/sample03/clean/'



# iwd_time

iwd_time <- read_csv(paste(in_path, 'iwd_time.csv', sep=""))

# remove 'a.' from all column names
names(iwd_time) <- substring(names(iwd_time), 3)

write_csv(iwd_time, paste(out_path, 'iwd_time.csv', sep=""))





# scene_pt_tp_dim

scene_pt_tp_dim <- read_csv(paste(in_path, 'scene_pt_tp_dim.csv', sep=""), quote = "\"")

# define transaction type 1 as issuance, redemption or reversal

for (i in 1:nrow(scene_pt_tp_dim)) {
  if (grepl("redem", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_1"] <- "redemption"
  } else if (grepl("revers", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_1"] <- "reversal"  
  } else if (grepl("deactivation", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_1"] <- "deactivation"  
  } else
    scene_pt_tp_dim[i, "txn_tp_1"] <- "issuance"  
}


for (i in 1:nrow(scene_pt_tp_dim)) {
  if (grepl("deactivation", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "deactivation"
  } else if(grepl("enrol", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("open", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("new cust", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("activation", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("promo", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "promobonus"
  } else if(grepl("campaign", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "promobonus"  
  } else if(grepl("survey", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "surveybonus"  
  } else if(grepl("bonus", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_2"] <- "otherbonus" 
  } else
    scene_pt_tp_dim[i, "txn_tp_2"] <- "regular"  
}

# define transaction type 3 as scene vs non-scene transactions
for (i in 1:nrow(scene_pt_tp_dim)) {
  if (grepl("non cineplex", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "non_cin"
  } else if (grepl("other place", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "non_cin"  
  } else if (grepl("cineplex", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"
  } else if (grepl("famous", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"
  } else if (grepl("galax", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("silver city", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin" 
  } else if (grepl("colossus", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("clossus", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("coliseum", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("cloiseum", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("cinema", str_to_lower(scene_pt_tp_dim$desc[i])) == TRUE) {
    scene_pt_tp_dim[i, "txn_tp_3"] <- "cin"  
  } else
    scene_pt_tp_dim[i, "txn_tp_3"] <- "unknown"  
}

write_csv(scene_pt_tp_dim, paste(out_path, 'scene_pt_tp_dim.csv', sep=""))




# scene_mbr_acct_dim

scene_mbr_acct_dim <- read_csv(paste(in_path, 'scene_mbr_acct_dim.csv', sep=""))

scene_mbr_acct_dim <- scene_mbr_acct_dim %>%
  filter(prim_scene_mbr_key != -1)

scene_mbr_acct_dim$cncl_dt[scene_mbr_acct_dim$cncl_dt == "null"]<-NA
scene_mbr_acct_dim$cncl_dt <- ymd_hms(scene_mbr_acct_dim$cncl_dt)

scene_mbr_acct_dim$scene_src_enrollment_dt <- ymd_hms(scene_mbr_acct_dim$scene_src_enrollment_dt)

scene_mbr_acct_dim$acct_eff_from_tmstamp <- ymd_hms(scene_mbr_acct_dim$acct_eff_from_tmstamp)

scene_mbr_acct_dim["date_lag"] = 
  difftime(scene_mbr_acct_dim$acct_eff_from_tmstamp, scene_mbr_acct_dim$scene_src_enrollment_dt, units = c("days"))

scene_mbr_acct_dim$acct_eff_to_tmstamp <- ymd_hms(scene_mbr_acct_dim$acct_eff_to_tmstamp)
scene_mbr_acct_dim = subset(scene_mbr_acct_dim, select = -c(acct_eff_to_tmstamp))

write_csv(scene_mbr_acct_dim, paste(out_path, 'scene_mbr_acct_dim.csv', sep=""))


