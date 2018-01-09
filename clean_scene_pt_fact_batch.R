library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(data.table)

in_path = '/global/project/queens-mma/scene-csv/sample03/raw/'
out_path = '/global/project/queens-mma/scene-csv/sample03/clean/'

doit = function(df, pt_tp_dim, iwd_time) {

    print("Drop column")
    df = select(df, -scene_mbr_acct_acty_key)

    print("Join pt_tp_dim")
    df <- left_join(df, pt_tp_dim, by="scene_pt_tp_key") 

    print("Join iwd_time")
    df <- left_join(df, iwd_time, by = c("mth_tm_key"="mo_fncl_key"))

    print("as_double")
    df <- mutate(df, txn_amt = as.double(txn_amt))

    print("factor")
    df$mo_clndr_code <- factor(df$mo_clndr_code, levels = month.name, ordered = TRUE)

  return(df)
}



scene_pt_tp_dim_R <- read_csv(paste(in_path, 'scene_pt_tp_dim.csv', sep=""), quote = "\"")

# define transaction type 1 as issuance, redemption or reversal
for (i in 1:nrow(scene_pt_tp_dim_R)) {
  if (grepl("redem", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_1"] <- "redemption"
  } else if (grepl("revers", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_1"] <- "reversal"  
  } else if (grepl("deactivation", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_1"] <- "deactivation"  
  } else
    scene_pt_tp_dim_R[i, "txn_tp_1"] <- "issuance"  
}

# define transaction type 2 as regular or bonus points
for (i in 1:nrow(scene_pt_tp_dim_R)) {
  if (grepl("deactivation", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "deactivation"
  } else if(grepl("enrol", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("open", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("new cust", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("activation", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "enrollbonus"
  } else if (grepl("promo", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "promobonus"
  } else if(grepl("campaign", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "promobonus"  
  } else if(grepl("survey", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "surveybonus"  
  } else if(grepl("bonus", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "otherbonus" 
  } else
    scene_pt_tp_dim_R[i, "txn_tp_2"] <- "regular"  
}

# define transaction type 3 as scene vs non-scene transactions
for (i in 1:nrow(scene_pt_tp_dim_R)) {
  if (grepl("non cineplex", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "non_cin"
  } else if (grepl("other place", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "non_cin"  
  } else if (grepl("cineplex", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"
  } else if (grepl("famous", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"
  } else if (grepl("galax", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("silver city", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin" 
  } else if (grepl("colossus", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("clossus", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("coliseum", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("cloiseum", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else if (grepl("cinema", str_to_lower(scene_pt_tp_dim_R$desc[i])) == TRUE) {
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "cin"  
  } else
    scene_pt_tp_dim_R[i, "txn_tp_3"] <- "unknown"  
}
        scene_pt_tp_dim_R <- mutate(scene_pt_tp_dim_R, scene_pt_tp_dim_R = as.integer(scene_pt_tp_key))


iwd_time <- read_csv(paste(in_path, 'iwd_time.csv', sep=""))
names(iwd_time) <- substring(names(iwd_time), 3)
iwd_time <- iwd_time %>% filter(time_lvl == "Month") %>% select(time_key, anul_clndr_code, anul_fncl_code, mo_clndr_code, time_lvl_st_dt, time_lvl_end_dt)
iwd_time <- mutate(iwd_time, time_key = as.integer(time_key))

#tbls = c("xaa", "xab", "xac", "xad", "xae", "xaf", "xag", "xah", "xai", "xaj", "xak", "xal", "xam", "xan", "xao", "xap", "xaq", "xar", "xas", "xat", "xau", "xav", "xaw", "xax", "xay", "xaz", "xba", "xbb", "xbc", "xbd", "xbe", "xbf", "xbg", "xbh", "xbi", "xbj", "xbk", "xbl", "xbm", "xbn", "xbo", "xbp", "xbq", "xbr", "xbs", "xbt", "xbu", "xbv", "xbw", "xbx", "xby", "xbz", "xca", "xcb", "xcc", "xcd", "xce", "xcf", "xcg", "xch", "xci", "xcj", "xck", "xcl", "xcm", "xcn", "xco", "xcp", "xcq", "xcr", "xcs", "xct", "xcu", "xcv", "xcw", "xcx", "xcy", "xcz", "xda", "xdb", "xdc", "xdd", "xde", "xdf", "xdg", "xdh", "xdi", "xdj", "xdk", "xdl", "xdm", "xdn", "xdo", "xdp", "xdq", "xdr", "xds", "xdt", "xdu", "xdv", "xdw")

tbls = c("scene_pt_fact.csv")

first = TRUE
for (tbl in tbls) {
      print(paste("Reading table ", tbl, sep=""))
      hdr = FALSE
      if (first == TRUE) {
        hdr = TRUE
        first = FALSE
      }
      df <- fread(paste(in_path, tbl, sep=""), sep = ",", header=hdr)
      colnames(df) = c('pt','txn_amt','scene_pt_tp_key','scene_mbr_acct_key','scene_mbr_acct_acty_key','scene_mbr_key','mth_tm_key')
      
      
      print(paste("Doing it for table ", tbl, sep=""))

        # Convert to a characgter to avoid an error in xam
        print("as.integer")
        df <- mutate(df, scene_pt_tp_key = as.integer(scene_pt_tp_key))
        df <- mutate(df, mth_tm_key = as.integer(mth_tm_key))

        print("Drop column")
        df = select(df, -scene_mbr_acct_acty_key)
    
        print("Join pt_tp_dim")
        df <- left_join(df, scene_pt_tp_dim_R, by="scene_pt_tp_key") 
    
        print("Join iwd_time")
        df <- left_join(df, iwd_time, by = c("mth_tm_key"="time_key"))

        print("as_double")
        df <- mutate(df, txn_amt = as.double(txn_amt))

        print("factor")
        df$mo_clndr_code <- factor(df$mo_clndr_code, levels = month.name, ordered = TRUE)
      
      
      print(paste("Writing table ", tbl, sep=""))
      fwrite(file=paste(out_path, tbl, sep=""), x=df, sep=",", row.names = FALSE, col.names=hdr)
      rm(df)
      gc()
}

