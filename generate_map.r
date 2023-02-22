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

# Functions for cleaning data
source("region_data_functions.r")

#
# Relevant filenames / variable definitions
#
csv_fn = "aggregations_current.csv"
case_name = "21_region"

#
# Code execution
#
shp <- readOGR( 
  dsn= file.path(getwd(),"/Shapefiles/IPM_Regions_201770405.shp")
)

colors <- get_palette(dim(shp@data)[1])

region_data <- read.csv(file.path(getwd(), csv_fn))
region_data <- make_regions_sequential(region_data)

aggregated_colors = average_all_colors(colors, region_data)

shp@data <- shp@data %>% mutate(color = rgb(0,1,0))

for (i in 1:length(colors)) {

  shp@data$color[i] = aggregated_colors[region_data$Region_Agg_ID[i]]

}

#
# Output
#
out_dir = file.path(getwd(), "/Maps/", case_name)

if (!file.exists(out_dir)) {
  dir.create(out_dir)
}

# Setup plot parameters
png(file.path(out_dir, paste(case_name, ".png")))
par(mar=c(0,0,0,0))

# Create map
plot(shp, col = shp@data$color, border = NA, height = 200, width = 300)

dev.off()

write.csv(region_data, file.path(out_dir, paste(case_name, ".csv")))