make_regions_sequential <- function(region_data) {

    regions = unique(region_data$Region_Agg_ID)
    region_data = region_data %>% mutate(Region_name = region_data$Region_Agg_ID)

    for (i in 1:length(region_data$Region_Agg_ID)) {
        
        region_data$Region_Agg_ID[i] = which(regions == region_data$Region_Agg_ID[i])[1]

    }

    region_data$Region_Agg_ID = sapply(region_data$Region_Agg_ID, as.integer)
    
    return(region_data)
}