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