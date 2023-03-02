# Get a color palette for the simulation, palette will have 'n' colors
get_palette <- function(n) {
    base_palette = c(
        "firebrick4", "firebrick3", "firebrick2", "firebrick1",
        "red4", "red3", "red2", "red1",
        "orangered4", "orangered3", "orangered2", "orangered1",
        "tomato4", "tomato3", "tomato2", "tomato1",
        "darkorange3", "darkorange2", "darkorange1", "darkorange",
        "orange4", "orange3", "orange2", "orange1",
        "goldenrod3", "goldenrod2", "goldenrod1", "goldenrod",
        "yellow3", "yellow2", "yellow1", "yellow",
        "darkolivegreen4", "darkolivegreen3", "darkolivegreen2", "darkolivegreen1",
        "green4", "green3", "green2", "green1",
        "dodgerblue4", "dodgerblue3", "dodgerblue2", "dodgerblue1",
        "blue4", "blue3", "blue2", "blue1",
        "slategray4", "slategray3", "slategray2", "slategray1",
        "mediumpurple4", "mediumpurple3", "mediumpurple2", "mediumpurple1",
        "maroon4", "maroon3", "maroon2", "maroon1",
        "purple4", "purple3", "purple2", "purple1"
    )

    base_length = length(base_palette)

    if (n < base_length) {
        return(base_palette[1:n])
    }

    if (n > base_length) {
        return (c(base_palette, colors()[1:(n - base_length)]))
    }

    return (base_palette)
}

# Get average r,g,b values of lists
get_mean_r <- function(cols) {
  cols_ret = data.frame(lapply(cols, col2rgb))

  red_vals = as.numeric(cols_ret[1,])
  mean_red = mean(red_vals)
  
  return(mean_red)
}

get_mean_g <- function(cols) {
  cols_ret = data.frame(lapply(cols, col2rgb))

  green_vals = as.numeric(cols_ret[2,])
  mean_green = mean(green_vals)
  
  return(mean_green)
}

get_mean_b <- function(cols) {
  cols_ret = data.frame(lapply(cols, col2rgb))

  blue_vals = as.numeric(cols_ret[3,])
  mean_blue = mean(blue_vals)
  
  return(mean_blue)
}

# Push colors in a palette further apart, for visibility
differentiate_palette <- function(cols, threshold) {

  for (c in 1:length(cols)) {

    r = col2rgb(cols[c])[1]
    g = col2rgb(cols[c])[2]
    b = col2rgb(cols[c])[3]

    for (i in (c+1):length(cols)) {
      r_test = col2rgb(cols[i])[1]
      g_test = col2rgb(cols[i])[2]
      b_test = col2rgb(cols[i])[3]

      # Euclidean distance between the two colors
      color_distance = sqrt((r - r_test)^2 + (g - g_test)^2 + (b - b_test)^2)
      if (color_distance < threshold) {

        if (r < r_test) {
          r = r - threshold
        } else {
          r = r + threshold
        }

        if (g < g_test) {
          g = g - threshold
        } else {
          g = g + threshold
        }

        if (b < b_test) {
          b = b - threshold
        } else {
          b = b + threshold
        }
      
        # Normalize into range [0,1]
        r = min(r / 255, 1)
        g = min(g / 255, 1)
        b = min(b / 255, 1)

        r = max(r, 0)
        g = max(g, 0)
        b = max(b, 0)

        cols[c] = rgb(r, g, b)
      }
    }
  }

  return(cols)
}

average_all_colors <- function(colors, region_data) {

  num_regions = max(region_data$Region_Agg_ID)
  colors_ret = rep(rgb(0,0,0), num_regions)
  names_ret = rep("", num_regions)

  for (i in 1:num_regions) {
    regs = which(region_data$Region_Agg_ID == i)

    cols = colors[regs]

    r = get_mean_r(cols) / 255
    g = get_mean_g(cols) / 255
    b = get_mean_b(cols) / 255

    colors_ret[i] = rgb(r,g,b)
    names_ret[i] = region_data$Region_name[which(region_data$Region_Agg_ID == i)[1]]
  }

  # Threshold for calling colors "too similar"
  # Two passes of differentiation for fuller comparisons
  threshold = 64 / num_regions + 40
  colors_ret = differentiate_palette(colors_ret, threshold)
  
  threshold = 64 / num_regions + 25
  colors_ret = differentiate_palette(colors_ret, threshold)

  ret_df = data.frame(colors = colors_ret, names = names_ret, ids = 1:num_regions)

  return(ret_df)
}