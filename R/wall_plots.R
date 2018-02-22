library(ggplot2)
library(dplyr)
MINDEPTH = -10994

plot_proportional_bathymetry <- function() {
  bathy <- sdmpredictors::load_layers('BO_bathymean', equalarea = TRUE, rasterstack = FALSE)[[1]]
  bv <- raster::values(bathy)
  bv <- bv[!is.na(bv) & bv <= 0]
  bv <- c(bv, MINDEPTH)
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
  ggsave('files/bathymetry_proportional.pdf', plot = bathy_prop, width = 20, height = 10, units = "cm")
  bathy_prop
}
# plot_proportional_bathymetry()

cache_call <- function(cachefile, expr, env = NULL) {
  if(file.exists(cachefile)) {
    return(readRDS(cachefile))
  } else {
    if(is.null(env)) {
      env = parent.frame()
    }
    result <- eval(expr, envir = NULL, enclos = env)
    saveRDS(result, cachefile)
    return(result)
  }
}

get_records_depth <- function(mindepth, maxdepth) {
  cache_call(paste0('files/records_depth_',mindepth,'_',round(maxdepth),'.rds'), {
    all_records_with_depth <- robis2::occurrence(startdepth = mindepth, enddepth = maxdepth)
  })
}

collect_depth_statistics <- function() {
  cache_call('files/depth_stats.rds', {

    depths <- c(0, 5, 10, 50, 100, 150, 200, 400, 700, 1000, 1500, 2500, 4000, 6000, 8000, 120000)
    # depths <- c(111, 112, 113)
    all_aphia <- c()
    nrecords_per_depth <- NULL
    nspecies_per_depth <- NULL
    aphia_at_depth <- NULL
    for(i in 2:length(depths)) {
      mindepth <- depths[i-1]
      maxdepth <- depths[i]-0.000001
      records <- get_records_depth(mindepth, maxdepth)
      records <- records[!is.na(records$species), c('depth', 'worms_id')]
      records$depth <- floor(records$depth) # floor to avoid rounding to the next min-max depth range, makes below easier

      all_aphia <- union(all_aphia, unique(records$aphiaid))
      nrecords_per_depth <- rbind(nrecords_per_depth, records %>% group_by(depth) %>% summarise(n = n()))
      uniquespdepth <- unique(records)
      aphia_at_depth <- rbind(aphia_at_depth, uniquespdepth)
      nspecies_per_depth <- rbind(nspecies_per_depth, unique(records) %>% group_by(depth) %>% summarise(n = n()))
    }
    list(unique_aphia=all_aphia, nrecords_per_depth=nrecords_per_depth,
         nspecies_per_depth=nspecies_per_depth, aphia_at_depth=aphia_at_depth)
  })
}
# rawdepthstats <- collect_depth_statistics()

qc_depth_stats <- function(depthstats) {
  filter_mindepth <- function(depthstats, key) {
    depthstats[[key]] <- depthstats[[key]] %>% filter(-1*depth > MINDEPTH)
    depthstats
  }
  for (k in c('nrecords_per_depth', 'nspecies_per_depth', 'aphia_at_depth')) {
    depthstats <- filter_mindepth(depthstats, k)
  }
  return(depthstats)
}
# depthstats <- qc_depth_stats(rawdepthstats)

plot_nrecords <- function(depthstats) {
  d <- depthstats$nrecords_per_depth
  d_binned <- d %>% group_by(depth=floor(depth / 10)*10) %>% summarise(n = sum(n))

  nrecords <- ggplot(d_binned, aes(y=log(n), x=-1*depth)) +
    geom_bar(stat="identity", width=1, col="#a6bddb") +
    coord_flip() +
    scale_x_continuous(expand=expand_scale(c(0, 0))) +
    scale_y_continuous(expand=expand_scale(c(0, 0)), breaks=NULL) +
    theme_minimal() +
    labs(x=NULL, y=NULL)
  ggsave('files/nrecords_depth.pdf', plot = nrecords, width = 5, height = 10, units = "cm")
  dev.off()
  nrecords
}
# plot_nrecords(depthstats)

plot_percentage_species <- function(depthstats) {

}
# plot_percentage_species(depthstats)


plot_all <- function() {
  plot_proportional_bathymetry()
  rawdepthstats <- collect_depth_statistics()
  depthstats <- qc_depth_stats(rawdepthstats)
  plot_nrecords(depthstats)
  plot_percentage_species(depthstats)
}
