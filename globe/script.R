# requires GDAL with PG support and manual creation of an entry in public.geometry_columns
# see http://www.kyngchaos.com/software/frameworks (also get gdal from there)

require(ggplot2)
require(dplyr)
require(rgdal)
require (mapdata)
require(marmap)
require(httr)
require(R.utils)
require(RPostgreSQL)

level <- 3

# load shapes

datafile <- sprintf("data_%s.dat", level)

if (file.exists(datafile)) {
  load(datafile)  
} else {
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
  
  query <- sprintf("select hexgrid%s.id, hexgrid%s.geom, count(*) as records from hexgrid.hexgrid%s left join obis.positions on positions.hexgrid%s_id = hexgrid%s.id left join explore.points on points.position_id = positions.id group by hexgrid%s.id, hexgrid%s.geom", level, level, level, level, level, level, level)
  data <- doQuery(query)
  # todo: save  
}

# process

hex <- fortify(data)
data@data$id <- as.numeric(rownames(data@data))
hex$id <- as.numeric(hex$id)
hex <- left_join(hex, data@data[,c("id", "records")], by = "id")

nat <- readOGR("world", "OGRGeoJSON")
world <- fortify(nat)

ggplot() +
  geom_polygon(data = hex, aes(x = long, y = lat, fill = records, group = group)) +
  scale_fill_distiller(limits = c(-1, 10000000), palette = "Spectral", trans = "log", labels = function (x) floor(x), na.value = "#eeeeee") +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "black", color = "black") +
  coord_map("ortho", orientation = c(15, -50, 0), xlim = c(-180, 180)) +
  #coord_quickmap() + xlim(-180, 180) + ylim(-90, 90) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "transparent"), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank(), legend.position = "none", legend.key.width = unit(3, "line"))
