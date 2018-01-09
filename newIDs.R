library(tidyr)
library(dplyr)
library(sparklyr)
library(magrittr) # To give use the cool %<>% operator
library(data.table)

in_path = '/global/project/queens-mma/scene-csv/sample0003/clean/'
out_path = '/global/project/queens-mma/scene-csv/sample0003/clean_newkeys/'

tbl = 'scene_mbr_dim.csv'
mbr_dim = fread(paste(in_path, tbl, sep=""), sep=",", header=TRUE)
colnames(mbr_dim)

names(mbr_dim)[names(mbr_dim) == 'scene_mbr_key'] <- 'scene_mbr_key_orig'
names(mbr_dim)[names(mbr_dim) == 'scene_mbr_acct_key'] <- 'scene_mbr_acct_key_orig'

mbr_dim$scene_mbr_key <- as.integer(unclass(factor(mbr_dim$scene_mbr_key_orig)))
mbr_dim$scene_mbr_acct_key <- as.integer(unclass(factor(mbr_dim$scene_mbr_acct_key_orig)))

scene_mbr_key_map = unique(mbr_dim %>%
  select(scene_mbr_key_orig, scene_mbr_key))

scene_mbr_acct_key_map = unique(mbr_dim %>%
  select(scene_mbr_acct_key_orig, scene_mbr_acct_key))

scene_mbr_key_map
scene_mbr_acct_key_map

fwrite(x=scene_mbr_key_map, 
       file=paste(out_path, 'scene_mbr_key_map.csv', sep=""), 
       sep=",", row.names=FALSE, col.names=TRUE)
fwrite(x=scene_mbr_acct_key_map, 
       file=paste(out_path, 'scene_mbr_acct_key_map.csv', sep=""), 
       sep=",", row.names=FALSE, col.names=TRUE)


mbr_dim2 = subset(mbr_dim, select=-c(scene_mbr_key_orig,scene_mbr_acct_key_orig))

mbr_dim2 = mbr_dim2 %>%
  select(scene_mbr_key, everything())

fwrite(x=mbr_dim2, file=paste(out_path, tbl, sep=""), sep=",", row.names=FALSE, col.names=TRUE)






tbl = 'scene_mbr_acct_dim.csv'
mbr_acct_dim = fread(paste(in_path, tbl, sep=""), sep=",")

mbr_acct_dim2 = mbr_acct_dim %>% 
  left_join(scene_mbr_acct_key_map, by=c("scene_mbr_acct_key" = "scene_mbr_acct_key_orig")) %>%
  rename(scene_mbr_acct_key_orig = scene_mbr_acct_key) %>%
  rename(scene_mbr_acct_key = scene_mbr_acct_key.y) %>%
  select(-scene_mbr_acct_key_orig) %>% 
  select(scene_mbr_acct_key, everything())
  

head(mbr_acct_dim)
head(mbr_acct_dim2)

colnames(mbr_acct_dim2)

fwrite(x=mbr_acct_dim2, file=paste(out_path, tbl,sep=""), sep=",", row.names=FALSE, col.names=TRUE)

rm(mbr_acct_dim)
rm(mbr_acct_dim2)
gc()


tbl = 'scene_pt_fact.csv'
pt_fact = fread(paste(in_path, tbl, sep=""), sep=",", header=TRUE)

pt_fact2 = pt_fact %>% 
  left_join(scene_mbr_key_map, by=c("scene_mbr_key" = "scene_mbr_key_orig")) %>%
  rename(scene_mbr_key_orig = scene_mbr_key)%>%
  rename(scene_mbr_key = scene_mbr_key.y) %>%
  select(-scene_mbr_key_orig) 

pt_fact2 = pt_fact2 %>% 
  left_join(scene_mbr_acct_key_map, by=c("scene_mbr_acct_key" = "scene_mbr_acct_key_orig")) %>%
  rename(scene_mbr_acct_key_orig = scene_mbr_acct_key) %>%
  rename(scene_mbr_acct_key = scene_mbr_acct_key.y) %>%
  select(-scene_mbr_acct_key_orig)


head(pt_fact)
head(pt_fact2)

fwrite(x=pt_fact2, file=paste(out_path, tbl,sep=""), sep=",", row.names=FALSE, col.names=TRUE)