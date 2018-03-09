# requires GDAL with PG support and manual creation of an entry in public.geometry_columns
# see http://www.kyngchaos.com/software/frameworks (also get gdal from there)

require(dplyr)
require(rgdal)
require (mapdata)
require(marmap)
require(httr)
require(R.utils)
require(RPostgreSQL)

level <- 4
#query <- sprintf("select hexgrid%s.id, hexgrid%s.geom, count(*) as records from hexgrid.hexgrid%s left join obis.positions on positions.hexgrid%s_id = hexgrid%s.id left join explore.points on points.position_id = positions.id inner join explore.taxon on taxon.id = points.valid_id where taxon.hab is true group by hexgrid%s.id, hexgrid%s.geom", level, level, level, level, level, level, level)
#query <- sprintf("select hexgrid%s.id, hexgrid%s.geom, count(*) as records from hexgrid.hexgrid%s left join obis.positions on positions.hexgrid%s_id = hexgrid%s.id left join explore.points on points.position_id = positions.id group by hexgrid%s.id, hexgrid%s.geom", level, level, level, level, level, level, level)
query <- sprintf("select hexgrid%s.id, hexgrid%s.geom, count(*) as records from hexgrid.hexgrid%s left join obis.positions on positions.hexgrid%s_id = hexgrid%s.id left join explore.points on points.position_id = positions.id inner join explore.taxon on taxon.id = points.valid_id where taxon.status in ('EX', 'EN', 'CR', 'EW', 'VU') group by hexgrid%s.id, hexgrid%s.geom", level, level, level, level, level, level, level)

host <- "obisdb-stage.vliz.be"
db <- "obis"
user <- "obisadmin"
password <- "8lu3Whale$"
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname=db, host=host, user=user, password=password)
dsn <- sprintf("PG:dbname=%s host=%s user=%s password=%s port=5432", db, host, user, password)
viewname <- "temp_view"

ogrListLayers(dsn)
ogrDrivers()

doQuery <- function(query) {
  dbGetQuery(con, paste0("drop view if exists ", viewname))
  dbGetQuery(con, paste0("create view ", viewname, " as ", query))
  result <- readOGR(dsn = dsn, layer = viewname)
  return(result)
}
data <- doQuery(query)

save(data, file = "threatened_4.dat")
  
