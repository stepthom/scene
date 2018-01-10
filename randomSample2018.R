library(tidyr)
library(dplyr)
library(sparklyr)
library(magrittr) # To give use the cool %<>% operator
library(data.table)

in_path = '/global/project/queens-mma/scene2018/full/'
out_path = '/global/project/queens-mma/scene2018/sample01/'

# First, randomly select the IDs to keep around.
file_name = 'SceneAnalytics.dbo.SP_CustomerDetail.csv'
takeSample = TRUE
if (takeSample == TRUE) {
    t = fread(paste(in_path, file_name, sep=""), sep=",", header=TRUE)
    set.seed(1234)
    ids = t %>% 
        select(Unique_member_identifier) %>% 
        distinct(Unique_member_identifier) %>%
        sample_frac(.01, replace=FALSE)

    rm(t)
    gc()
    fwrite(x=ids, file=paste(out_path, "ids.csv", sep=""), sep=",", row.names=FALSE, col.names=TRUE)
} else {
    ids = fread(paste(out_path, "ids.csv", sep=""), sep=",", header=TRUE)
}

file_names = c(
"SceneAnalytics.dbo.LK_account_unique_member_identifier_sample10.csv",
"SceneAnalytics.dbo.SP_AccountBalance.csv",
"SceneAnalytics.dbo.SP_AccountHistory.csv",
"SceneAnalytics.dbo.SP_AccountHistoryType.csv",
"SceneAnalytics.dbo.SP_ActivityStatusScotiaScene_E.csv",
"SceneAnalytics.dbo.SP_CineplexStore.csv",
"SceneAnalytics.dbo.SP_CustomerDetail.csv",
"SceneAnalytics.dbo.SP_CustomerExtension.csv",
"SceneAnalytics.dbo.SP_DimProxyLocation.csv",
"SceneAnalytics.dbo.SP_FactAttribute.csv",
"SceneAnalytics.dbo.SP_FactEnrollment.csv",
"SceneAnalytics.dbo.SP_LocationCARA.csv",
"SceneAnalytics.dbo.SP_Location.csv",
"SceneAnalytics.dbo.SP_Partner_E.csv",
"SceneAnalytics.dbo.SP_Points.csv",
"SceneAnalytics.dbo.SP_PointsType.csv",
"SceneAnalytics.dbo.SP_PointTypeStatistics.csv",
"SceneAnalytics.dbo.SP_ProxyPointTransaction_10.csv",
"SceneAnalytics.dbo.SP_QualityActivity.csv",
"SceneAnalytics.dbo.SP_Source.csv"
)

for (file_name in file_names) {
      print(paste("Reading table ", file_name, sep=""))
      t = fread(paste(in_path, file_name, sep=""), sep=",", header=TRUE)

      if ("Unique_member_identifier" %in% colnames(t)) {
        print("Filter")
        t2 = t %>% filter(Unique_member_identifier %in% ids$Unique_member_identifier)
      } else {
        print("No Filter needed.")
        t2 = t
      }
      rm(t)
      gc()
      
      print("Writing")
      fwrite(x=t2, file=paste(out_path, file_name, sep=""), sep=",", row.names=FALSE, col.names=TRUE)
      rm(t2)
      gc()
}

