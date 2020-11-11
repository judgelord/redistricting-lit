# install.packages("rsconnect")
library(rsconnect)

# refresh
source("app.R")
rsconnect::deployApp()
Y

# a loop to refresh every 10 min
while(T){
  source("app.R")
  rsconnect::deployApp(forceUpdate = T)
  Sys.sleep(600)
}
