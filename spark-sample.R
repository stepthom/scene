# Just copying the tutorial from http://spark.rstudio.com/index.html
# Everything should "just work" and works as a good test of Spark
# -jeff

# Install these packages if you don't have them already
#install.packages(c("nycflights13", "Lahman"))

# connect to spark
library(sparklyr)
sc <- spark_connect(master = "yarn-client", spark_home = "/usr/hdp/current/spark-client/")

library(dplyr)
iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
src_tbls(sc)

iris_tbl %>% filter(petal_width < 0.3)

# filter by departure delay and print the first few records
flights_tbl %>% filter(dep_delay == 2)

# an example from the dplyr library
delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# testing spark sql
library(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview

# view spark log 
spark_log(sc)

# NOTE - due to security configuration, the spark web ui will not work!

# now disconnect from spark
spark_disconnect(sc)
