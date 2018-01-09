library(tidyverse)
library(lubridate)
library(stringr)

in_path = '/global/project/queens-mma/scene-csv/full/raw/'
out_path = '/global/project/queens-mma/scene-csv/full/clean/'
scene_mbr_dim <- read_csv(paste(in_path,'scene_mbr_dim.csv', sep=""), quote = "\"")

# removing what appears to be a testing record
scene_mbr_dim %>%
  filter(scene_mbr_key == -1)

scene_mbr_dim <- scene_mbr_dim %>%
  filter(scene_mbr_key != -1)

scene_mbr_dim$brth_dt[scene_mbr_dim$brth_dt==1900]<-NA
scene_mbr_dim$brth_dt[scene_mbr_dim$brth_dt==9999]<-NA


# convert all letters in postal code to upper case
scene_mbr_dim$psnl_post_cd <-
  str_to_upper(scene_mbr_dim$psnl_post_cd)

scene_mbr_dim$psnl_prov_state_cd[scene_mbr_dim$psnl_prov_state_cd=="null"]<-NA
scene_mbr_dim$psnl_prov_state_cd[scene_mbr_dim$psnl_prov_state_cd=="NULL"]<-NA
scene_mbr_dim$psnl_prov_state_cd[scene_mbr_dim$psnl_prov_state_cd=="NA's"]<-NA

# convert all city names to lower case
scene_mbr_dim$psnl_city <- str_to_lower(scene_mbr_dim$psnl_city)

# convert all characters to ASCII
scene_mbr_dim$psnl_city <- iconv(scene_mbr_dim$psnl_city, to = "ASCII//TRANSLIT")

# remove all punctuations from city names
scene_mbr_dim$psnl_city <- str_replace_all(scene_mbr_dim$psnl_city,"[[:punct:]]","")

# standardize city names
# list and count city names per FSA, take city name with highest count in each FSA
fsa_city <- group_by(scene_mbr_dim, psnl_post_cd, psnl_city)%>%
  summarise(count = n())%>%
  arrange(desc(count))

dupe_fsa<- group_by(fsa_city, psnl_post_cd)%>%
  summarise(count = n())%>%
  filter(count>1)%>%
  arrange(desc(count))

dupe_city <- fsa_city%>%
  filter(psnl_post_cd %in% dupe_fsa$psnl_post_cd)

Max_city <- dupe_city %>%
  group_by(psnl_post_cd) %>% slice(which.max(count))

Max_city <- subset(Max_city, select = -c(count))

# replace low freq city names (likely mispelling) with high freq city name within the same FSA
scene_mbr_dim <-
  left_join(scene_mbr_dim, Max_city, by="psnl_post_cd")

colnames(scene_mbr_dim)[8]<-"psnl_city"
colnames(scene_mbr_dim)[39]<-"psnl_city_2"


for (i in 1:nrow(scene_mbr_dim)) {
  if (scene_mbr_dim$psnl_post_cd[i] %in% Max_city$psnl_post_cd) {
    scene_mbr_dim[i, "psnl_city"] <- scene_mbr_dim[i, "psnl_city_2"] 
  }else
    scene_mbr_dim[i, "psnl_city"] <- scene_mbr_dim[i, "psnl_city"]
}

scene_mbr_dim <- subset(scene_mbr_dim, select = -c(psnl_city_2))

# individual changes
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('montrACal',scene_mbr_dim$psnl_city,                                                ignore.case = TRUE),'montreal',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('montrACalnord',scene_mbr_dim$psnl_city,                                                ignore.case = TRUE),'montreal',psnl_city))  
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('montrealnord',scene_mbr_dim$psnl_city,                                                ignore.case = TRUE),'montreal',psnl_city))  
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('montreasl',scene_mbr_dim$psnl_city,                                                ignore.case = TRUE),'montreal',psnl_city))  
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('monrteal',scene_mbr_dim$psnl_city,                                                ignore.case = TRUE),'montreal',psnl_city))  
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('QuACbec',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'quebec',psnl_city))  
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('toronto thornhiil',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'thornhill',psnl_city)) 
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse('tor','toronto',psnl_city)) 
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('northyork',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'north york',psnl_city)) 
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('northvancouver',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'north vancouver',psnl_city)) 
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('saintlAConard',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'saintleonard',psnl_city)) 
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('sanitleonard',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'saintleonard',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('saintjeansurrichelieu',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'saint jean sur richelieux',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('stjeanrichelieu',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'saint jean sur richelieux',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('stjeansurrichel',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'saint jean sur richelieux',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('stjACrAme',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'st jerome',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('troisriviAres',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'troisrivieres',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('lACvis',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'levis',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('longuieul',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'longueuil',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('rcihmond',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'richmond',psnl_city))
scene_mbr_dim<-
  mutate(scene_mbr_dim, psnl_city=ifelse(grepl('rmd',scene_mbr_dim$psnl_city, 
                                               ignore.case = TRUE),'richmond',psnl_city))

scene_mbr_dim = subset(scene_mbr_dim, select = -c(psnl_cntry_cd))
scene_mbr_dim = subset(scene_mbr_dim, select = -c(dlvy_mode_tp_cd))
scene_mbr_dim = subset(scene_mbr_dim, select = -c(kill_word_f))
scene_mbr_dim = subset(scene_mbr_dim, select = -c(web_acss_f))
scene_mbr_dim = subset(scene_mbr_dim, select = -c(hh_incm_ref_key, hh_incm_desc))
scene_mbr_dim = subset(scene_mbr_dim, select = -c(pgm_hd_abt_from_input, pgm_hd_abt_from_ref_key, pgm_hd_abt_from_desc))


show_time <- c("1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm",
               "8:00pm","9:00pm","10:00pm","11:00pm","12 midnight")

scene_mbr_dim$prefrd_show_tm_desc<-factor(scene_mbr_dim$prefrd_show_tm_desc,
                                          levels=show_time, ordered=TRUE)

movie_freq <- c("<2","3-6","7-10","11-20","20+")

scene_mbr_dim$movie_gng_frq_ref_desc<-
  factor(scene_mbr_dim$movie_gng_frq_ref_desc,levels=movie_freq, ordered=TRUE)

write_csv(scene_mbr_dim, paste(out_path, 'scene_mbr_dim.csv', sep=""))
