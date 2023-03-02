#
# Inclusions
#
library(rgdal)
library(ggplot2)
library(sp)
library(sf)
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
case_name = "3_region_test"

# Any IPM regions in this array will be plotted with a dot over their centroid
special = {}

#
# Code execution
#
shp <- readOGR( 
  dsn= file.path(getwd(),"/Shapefiles/IPM_Regions_201770405.shp")
)

# Get region centroids in case they need to be plotted
points = st_point_on_surface(st_as_sf(shp))

# Get the palette of colors to be used
colors <- get_palette(dim(shp@data)[1])

# Get the data on which aggregated region each IPM region belongs to
region_data <- read.csv(file.path(getwd(), csv_fn))
region_data <- make_regions_sequential(region_data)

# Get the aggregated color palette given region assignments
agg_vals = average_all_colors(colors, region_data)

# Assign the colors to the regions given aggregations
shp@data <- shp@data %>% mutate(color = rgb(0,1,0))
for (i in 1:length(colors)) 
{
  shp@data$color[i] = agg_vals$colors[region_data$Region_Agg_ID[i]]
}

#
# Output
#
out_dir = file.path(getwd(), "/Maps/", case_name)

# Make the output directory if it does not yet exist
if (!file.exists(out_dir)) {
  dir.create(out_dir)
}

# Setup plot parameters
png(file.path(out_dir, paste(case_name, ".png", sep = "")))
par(mar=c(0,0,0,0))

# Create map
plot(shp, col = shp@data$color, border = NA, height = 200, width = 300)

# Add legend with aggregated colors
legend("bottom", legend = agg_vals$names, fill = agg_vals$colors)

# Plot centroid dot for regions in "special"
for (i in 1:length(colors)) 
{
  if (shp@data$IPM_Region[i] %in% special) 
  {
    print(paste("Plotting a centroid for", shp@data$IPM_Region[i], sep = " "))
    plot(points$geometry[i], add = TRUE, pch = 21, col = "black", bg = "white")
  }
}

dev.off()

# Save the region aggregations used to the directory of the same name
region_data_save = region_data[,!(names(region_data) %in% c("Region_Agg_ID", "X", ""))]
write.csv(region_data_save, file.path(out_dir, paste(case_name, ".csv", sep = "")), row.names = FALSE)