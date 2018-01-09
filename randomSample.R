library(tidyr)
library(dplyr)
library(sparklyr)
library(magrittr) # To give use the cool %<>% operator
library(data.table)

in_path = '/global/project/queens-mma/scene-csv/sample003/clean/'
out_path = '/global/project/queens-mma/scene-csv/sample0003/clean/'

tbl = 'scene_mbr_dim.csv'
takeSample = FALSE
if (takeSample == TRUE) {
    mbr_dim = fread(paste(in_path, tbl, sep=""), sep=",", header=TRUE)
    mbr_dim2 = mbr_dim %>% sample_frac(.1, replace=FALSE)
    rm(mbr_dim)
    gc()
    fwrite(x=mbr_dim2, file=paste(out_path, tbl, sep=""), sep=",", row.names=FALSE, col.names=TRUE)
} else {
    mbr_dim2 = fread(paste(out_path, tbl, sep=""), sep=",", header=TRUE)
}

keys = unique(mbr_dim2$scene_mbr_key)
acct_keys = unique(mbr_dim2$scene_mbr_acct_key)

rm(mbr_dim2)
gc()

tbl = 'scene_pt_fact.csv'
pt_fact = fread(paste(in_path, tbl, sep=""), sep=",", header=TRUE)
pt_fact2 = pt_fact %>% filter(scene_mbr_key %in% keys)
rm(pt_fact)
gc()
fwrite(x=pt_fact2, file=paste(out_path, tbl, sep=""), sep=",", row.names=FALSE, col.names=TRUE)
rm(pt_fact2)
gc()



tbls = c("xaa", "xab", "xac", "xad", "xae", "xaf", "xag", "xah", "xai", "xaj", "xak", "xal", "xam", "xan", "xao", "xap", "xaq", "xar", "xas", "xat", "xau", "xav", "xaw", "xax", "xay", "xaz", "xba", "xbb")

first=TRUE
for (tbl in tbls) {
      hdr = FALSE
      if (first == TRUE) {
        hdr = TRUE
        first = FALSE
      }
      
      print(paste("Reading table ", tbl, sep=""))
      pt_fact = fread(paste(in_path, tbl, sep=""), sep=",", header=hdr)
      colnames(pt_fact) = c('pt','txn_amt','scene_pt_tp_key','scene_mbr_acct_key','scene_mbr_acct_acty_key','scene_mbr_key','mth_tm_key')

      print("Filter")
      pt_fact2 = pt_fact %>% filter(scene_mbr_key %in% keys)
      rm(pt_fact)
      gc()
      
      print("Writing")
      fwrite(x=pt_fact2, file=paste(out_path, tbl, sep=""), sep=",", row.names=FALSE, col.names=hdr)
      rm(pt_fact2)
      gc()
}

tbl = 'scene_mbr_acct_dim.csv'
mbr_acct_dim = fread(paste(in_path, tbl, sep=""), sep=",")
mbr_acct_dim2 = mbr_acct_dim %>% filter(scene_mbr_acct_key %in% acct_keys)
fwrite(x=mbr_acct_dim2, file=paste(out_path, tbl,sep=""), sep=",", row.names=FALSE, col.names=TRUE)

rm(mbr_acct_dim)
rm(mbr_acct_dim2)
gc()
