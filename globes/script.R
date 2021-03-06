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

#datafile <- "extinct_4.dat"
datafile <- "hab_4.dat"
#imagefile <- "extinct_mollweide.png"
imagefile <- "hab_mollweide.png"
#projection <- coord_map("ortho", orientation = c(50, -50, 10), xlim = c(-180, 180))
projection <- coord_map("moll", xlim = c(-180, 180))
#projection <- coord_map("ortho", orientation = c(-40, 100, 0), xlim = c(-180, 180))
#projection <- coord_map("ortho", orientation = c(15, -50, 0), xlim = c(-180, 180))

# load data

load(datafile)  
source("haedat.R")

# calculate (!!!!!! disable if necessary)
data@data$records <- (data@data$s / data@data$completeness_biota) - data@data$s
data@data$records[data@data$records < 0] <- 0

# process

hex <- fortify(data)
data@data$id <- as.numeric(rownames(data@data))
hex$id <- as.numeric(hex$id)
hex <- left_join(hex, data@data[,c("id", "records")], by = "id")
if (!exists("world")) {
  nat <- readOGR("world", "OGRGeoJSON")
  world <- fortify(nat)
}

# plot and save

baseplot <- ggplot() +
  projection +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.key.width = unit(3, "line")
  )
baselayer <- geom_rect(data = data.frame(xmin = -180, xmax = 180, ymin = -90, ymax = 90), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ffffff")
hexplot <- geom_polygon(data = hex, aes(x = long, y = lat, fill = records, group = group))
hexplothab <- geom_polygon(data = hex, aes(x = long, y = lat, fill = records, group = group), fill = "#dddddd")
hexscale <- scale_fill_distiller(limits = c(-1, 10000000), palette = "Spectral", trans = "log", labels = function (x) floor(x), na.value = "#eeeeee")
hexscale_undiscovered <- scale_fill_distiller(limits = c(-1, 100000), palette = "Spectral", trans = "log", labels = function (x) floor(x), na.value = "#eeeeee")
hexscale_extinct <- scale_fill_distiller(limits = c(-1, 1500), palette = "Spectral", trans = "log", labels = function (x) floor(x), na.value = "#eeeeee")
hexscalegray <- scale_fill_gradient(low = "#eeeeee", high = "#aaaaaa", trans = "log")
worldplot <- geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "black", color = "black")
haedatplot <- geom_point(data = haedat, aes_string(x = "longitude", y = "latitude", size = "events", fill = "events"), stroke = 0.5, alpha = 1, shape = 21, colour = "white")
haedatscale <- scale_radius(range = c(2, 12))

# HAB
#baseplot + baselayer + hexplothab + worldplot + haedatplot + haedatscale + scale_fill_gradient(low = "#1789FC", high = "#FDB833") #+ scale_fill_distiller(palette = "PuRd", direction = 1)
#baseplot + baselayer + hexplothab + worldplot + haedatplot + haedatscale + scale_fill_gradient(low = "#2F97C1", high = "#2F97C1") #+ scale_fill_distiller(palette = "PuRd", direction = 1)
baseplot + baselayer + hexplothab + worldplot + haedatplot + haedatscale + scale_fill_gradient(low = "#ff6600", high = "#ff6600") #+ scale_fill_distiller(palette = "PuRd", direction = 1)

baseplot + baselayer + hexplot + hexscale_extinct + worldplot

ggsave(file = imagefile, height = 7, width = 14, bg = "transparent")

#writeOGR(obj = data, dsn = "extinct", layer = "data", driver = "ESRI Shapefile", overwrite_layer = TRUE)
