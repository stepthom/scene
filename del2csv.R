library("data.table")

# Actually, this script is no longer necessary. A better, faster way is to use the following shell command:
# sed 's/\x01/,/g' scene_mbr_dim.del | 's/ *,/,/g' | sed 's/, +/,/g'  > ../../scene-csv/full/raw/scene_mbr_dim.csv
# sed 's/\x01/,/g' scene_mbr_acct_dim.del | sed 's/ *,/,/g' | sed 's/, +/,/g' > ../../scene-csv/full/raw/scene_mbr_acct_dim.csv
# sed 's/\x01/,/g' scene_pt_fact.del | sed 's/ *,/,/g' | sed 's/, +/,/g' > ../../scene-csv/full/raw/scene_pt_fact.csv
# sed 's/\x01/,/g' scene_pt_tp_dim.del | sed 's/ *,/,/g' | sed 's/, +/,/g' > ../../scene-csv/full/raw/scene_pt_tp_dim.csv
# sed 's/\x01/,/g' iwd_time.del | sed 's/ *,/,/g' | sed 's/, +/,/g' > ../../scene-csv/full/raw/iwd_time.csv



#tbls = c("iwd_chnl", "iwd_pd_pln", "iwd_step_plan", "scene_mbr_hist_dim")
custs = c(
          "iwd_custaa",
          "iwd_custab",
          "iwd_custac",
          "iwd_custad",
          "iwd_custae",
          "iwd_custaf",
          "iwd_custag",
          "iwd_custah",
          "iwd_custai",
          "iwd_custaj",
          "iwd_custak",
          "iwd_custal",
          "iwd_custam",
          "iwd_custan",
          "iwd_custao",
          "iwd_custap",
          "iwd_custaq",
          "iwd_custar",
          "iwd_custas",
          "iwd_custat",
          "iwd_custau",
          "iwd_custav",
          "iwd_custaw"
          )



acty = c(
         "iwf_acty_rollupaa",
         "iwf_acty_rollupab",
         "iwf_acty_rollupac",
         "iwf_acty_rollupad",
         "iwf_acty_rollupae",
         "iwf_acty_rollupaf",
         "iwf_acty_rollupag",
         "iwf_acty_rollupah",
         "iwf_acty_rollupai",
         "iwf_acty_rollupaj",
         "iwf_acty_rollupak",
         "iwf_acty_rollupal",
         "iwf_acty_rollupam",
         "iwf_acty_rollupan",
         "iwf_acty_rollupao",
         "iwf_acty_rollupap",
         "iwf_acty_rollupaq",
         "iwf_acty_rollupar",
         "iwf_acty_rollupas",
         "iwf_acty_rollupat",
         "iwf_acty_rollupau",
         "iwf_acty_rollupav"
         )





custaccts = c(
              "iwf_cust_acctaa",
              "iwf_cust_acctab",
              "iwf_cust_acctac",
              "iwf_cust_acctad",
              "iwf_cust_acctae",
              "iwf_cust_acctaf",
              "iwf_cust_acctag",
              "iwf_cust_acctah",
              "iwf_cust_acctai",
              "iwf_cust_acctaj",
              "iwf_cust_acctak",
              "iwf_cust_acctal",
              "iwf_cust_acctam",
              "iwf_cust_acctan",
              "iwf_cust_acctao",
              "iwf_cust_acctap",
              "iwf_cust_acctaq",
              "iwf_cust_acctar",
              "iwf_cust_acctas",
              "iwf_cust_acctat",
              "iwf_cust_acctau",
              "iwf_cust_acctav",
              "iwf_cust_acctaw",
              "iwf_cust_acctax",
              "iwf_cust_acctay",
              "iwf_cust_acctaz",
              "iwf_cust_acctba",
              "iwf_cust_acctbb",
              "iwf_cust_acctbc",
              "iwf_cust_acctbd",
              "iwf_cust_acctbe",
              "iwf_cust_acctbf",
              "iwf_cust_acctbg",
              "iwf_cust_acctbh",
              "iwf_cust_acctbi",
              "iwf_cust_acctbj",
              "iwf_cust_acctbk",
              "iwf_cust_acctbl",
              "iwf_cust_acctbm",
              "iwf_cust_acctbn",
              "iwf_cust_acctbo",
              "iwf_cust_acctbp",
              "iwf_cust_acctbq",
              "iwf_cust_acctbr",
              "iwf_cust_acctbs",
              "iwf_cust_acctbt",
              "iwf_cust_acctbu",
              "iwf_cust_acctbv",
              "iwf_cust_acctbw",
              "iwf_cust_acctbx",
              "iwf_cust_acctby",
              "iwf_cust_acctbz",
              "iwf_cust_acctca",
              "iwf_cust_acctcb",
              "iwf_cust_acctcc",
              "iwf_cust_acctcd",
              "iwf_cust_acctce",
              "iwf_cust_acctcf",
              "iwf_cust_acctcg",
              "iwf_cust_acctch",
              "iwf_cust_acctci",
              "iwf_cust_acctcj",
              "iwf_cust_acctck",
              "iwf_cust_acctcl",
              "iwf_cust_acctcm",
              "iwf_cust_acctcn",
              "iwf_cust_acctco",
              "iwf_cust_acctcp",
              "iwf_cust_acctcq",
              "iwf_cust_acctcr",
              "iwf_cust_acctcs",
              "iwf_cust_acctct",
              "iwf_cust_acctcu",
              "iwf_cust_acctcv",
              "iwf_cust_acctcw",
              "iwf_cust_acctcx",
              "iwf_cust_acctcy",
              "iwf_cust_acctcz",
              "iwf_cust_acctda",
              "iwf_cust_acctdb",
              "iwf_cust_acctdc"
              )

facts = c(
  #"scene_pt_factaa",
  #"scene_pt_factab",
  #"scene_pt_factac",
  #"scene_pt_factad",
  #"scene_pt_factae",
  "scene_pt_factaf",
  "scene_pt_factag",
  "scene_pt_factah",
  "scene_pt_factai",
  "scene_pt_factaj",
  "scene_pt_factak"
)





# The big ones, do them later.
tbls = c(custs, acty, facts, custaccts)

#tbls = c(
         #"iwd_time.del", "iwf_cust_step_acct.del",
         #"pt_bal_cat_ref.del", "pt_bal_sum.del", "scene_mbr_acct_dim.del", "scene_mbr_cis_cust_lnk.del",
         #"scene_mbr_dim.del", "scene_pt_tp_dim.del")

tbls = c("scene_mbr_acct_dim", 
         "scene_mbr_dim", "scene_pt_fact", "scene_pt_tp_dim", "iwd_time")

tbls = facts

in_path = '/global/project/queens-mma/scene/raw/'
in_path = '/global/home/hpc3552/test/'
out_path = '/global/project/queens-mma/scene-csv/'

for (tbl in tbls) {
      print(paste("Reading table ", tbl, sep=""))
      #t <- read.delim(paste(in_path, tbl, sep=""), sep = "\x01")
      t <- fread(paste("sed -r 's/\\0//g' ", in_path, tbl, sep=""), sep = "\x01")
    print(paste("Writing table ", tbl, sep=""))
    write.csv(t, file = paste(out_path, tbl, ".csv", sep=""),row.names=FALSE)
      rm(t)
      gc()
}
