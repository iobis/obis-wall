require(readr)
require(dplyr)
require(RMySQL)
require(stringr)
require(ggplot2)

getData <- function(host = "192.168.5.41", user = "iode_general", password = "bulk3pinj@", dbname = "haedat") {
  con <- dbConnect(MySQL(), user = user, password = password, dbname = dbname, host = host)
  query <- read_file("haedat_query.sql")
  rs <- dbSendQuery(con, query)
  data <- fetch(rs, n = 20000)
  dbHasCompleted(rs)
  dbClearResult(rs)
  dbDisconnect(con)
  return(data)  
}

haedat <- getData()
haedat <- haedat %>% group_by(longitude, latitude) %>% summarize(events = n()) %>% arrange(events)
