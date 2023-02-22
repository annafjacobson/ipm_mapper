#
# Inclusions
#
library(rgdal)
library(ggplot2)
library(sp)
library(dplyr)
library(colorspace)

# Functions for defining map colors
source("color_functions.r")

#
# Relevant filenames / variable definitions
#
csv_fn = "aggregations_current.csv"
case_name = "heavy_aggregation"

#
# Code execution
#
shp <- readOGR( 
  dsn= file.path(getwd(),"/Shapefiles/IPM_Regions_201770405.shp")
)

colors <- get_palette(dim(shp@data)[1])
region_data <- read.csv(file.path(getwd(), csv_fn))

shp@data <- shp@data %>% mutate(color = rgb(0,1,0))

for (i in 1:length(colors)) {
  regs = which(region_data$Region_Agg_ID == region_data$Region_Agg_ID[i])

  cols = colors[regs]

  r = get_mean_r(cols) / 255
  g = get_mean_g(cols) / 255
  b = get_mean_b(cols) / 255

  col = rgb(r,g,b)
  
  shp@data$color[i] = cols[1]
}

#
# Output
#
out_dir = file.path(getwd(), "/Maps/", case_name)

if (!file.exists(out_dir)) {
  dir.create(out_dir)
}

png(file.path(out_dir, paste(case_name, ".png")), res = 120)

# Create map
plot(shp, col = shp@data$color)

dev.off()

write.csv(region_data, file.path(out_dir, paste(case_name, ".csv")))