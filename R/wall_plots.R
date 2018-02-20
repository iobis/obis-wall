library(ggplot2)

plot_proportional_bathymetry <- function() {
  bathy <- sdmpredictors::load_layers('BO_bathymean', equalarea = TRUE, rasterstack = FALSE)[[1]]
  bv <- raster::values(bathy)
  bv <- bv[!is.na(bv) & bv <= 0]
  bv <- data.frame(bathy = bv)
  p <- ggplot(bv, aes(bathy)) +
    geom_density()
  p <- ggplot_build(p)

  pdata <- p$data[[1]][,c('x', 'y')]
  pdata$y <- -1*cumsum(pdata$y)
  colnames(pdata) <- c('Depth', 'Density')

  bottom_pdata <- data.frame(Depth=rep(min(pdata$Depth), nrow(pdata)), Density=pdata$Density)

  bathy_prop <- ggplot(pdata, aes(Density, Depth)) +
    geom_area(data=bottom_pdata, color = '#734523', fill = '#734523') + # Brown
    geom_area(data=pdata, color = "#4488BB", fill = "#4488BB") + # Blue
    scale_x_continuous(expand=expand_scale(c(0, 0.005)), breaks=NULL) +
    scale_y_continuous(expand=expand_scale(c(0, 0))) +
    theme_minimal() +
    labs(x=NULL, y=NULL)
  print(bathy_prop)

  ggsave('files/bathymetry_proportional.pdf', plot = bathy_prop, width = 20, height = 10, units = "cm")
}
# plot_proportional_bathymetry()

