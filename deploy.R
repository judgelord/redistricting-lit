# install.packages("rsconnect")
library(rsconnect)

# refresh from google sheet
source("update.R")

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
