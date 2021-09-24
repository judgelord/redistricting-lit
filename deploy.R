#FIXME - maybe this should pull data from literature NetLit 

# install.packages("rsconnect")
library(rsconnect)

# refresh from google sheet
# NOTE THIS IS NOW DONE IN THE redistricting-NetLit
# source("update.R")

# build app
source("app.R")
rsconnect::deployApp()
Y

# a loop to refresh every 10 min
while(T){
  source("update.R")
  
  source("app.R")
  rsconnect::deployApp(forceUpdate = T)
  Sys.sleep(600)
}
