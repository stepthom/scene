library(tidyverse)
library(lubridate)
library(stringr)

in_path = '/global/project/queens-mma/scene-csv/full/raw/'
out_path = '/global/project/queens-mma/scene-csv/full/clean/'

iwd_time <- read_csv(paste(in_path, 'iwd_time.csv', sep=""))
names(iwd_time) <- substring(names(iwd_time), 3)

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

# define transaction type 2 as regular or bonus points
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


scene_pt_fact <- read_csv(paste(in_path, 'scene_pt_fact.csv', sep=""))

scene_pt_fact <-
  left_join(scene_pt_fact, scene_pt_tp_dim, by="scene_pt_tp_key") 

txn_mth_ref <-
  iwd_time %>%
  filter(time_lvl == "Month") %>%
  select(time_key, anul_clndr_code, anul_fncl_code, mo_clndr_code, time_lvl_st_dt, time_lvl_end_dt)

scene_pt_fact <-
  left_join(scene_pt_fact, txn_mth_ref, by = c("mth_tm_key"="time_key"))

scene_pt_fact$txn_amt <- parse_double(scene_pt_fact$txn_amt)

scene_pt_fact = subset(scene_pt_fact, select = -c(scene_mbr_acct_acty_key))

scene_pt_fact$mo_clndr_code <- 
  factor(scene_pt_fact$mo_clndr_code, levels = month.name, ordered = TRUE)


write_csv(scene_pt_fact, paste(out_path, 'scene_pt_fact.csv', sep=""))

