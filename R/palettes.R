


# core function that takes an input palette (as 4-vector plus # of steps)
# and runs a genetic algorithm to make a colourblind-friendly bivariate palette.
# specify num_children per generation, and num_Generations to run the evolution.
# the scoring function maximizes the minimum distance between colours.
palette_evolution <- function(palette_coords, num_steps, num_children, num_generations, verbose = FALSE){
  new_palette <- palette_coords
  for (i in 1:num_generations){
    if (verbose) message(paste0("Generation #",i))
    new_palette <- evolve_palette(new_palette, num_steps, num_children = num_children)
  }
  return(new_palette)
}


# trying evolution without tidy functions, see if it's faster
# it's only a little bit  faster! ~290ms vs. ~306ms
# score_palette() is by far the bottleneck: 290ms out of ~ 300ms
evolve_palette_base <- function(parent_coords, num_steps, num_children){
  # set up the parent
  #parent_coords <- c(var12min, var1max, var2max, var12max)
  
  # our children will go into a list
  
   child_list <- rep(list(parent_coords), times = num_children)
  
  # use lapply to apply spawn_palette() to each child in the list
  evolved_list <- lapply(child_list, spawn_palette, num_steps)
  # children <- children %>%
  #   mutate(evolved = purrr::map(children, spawn_palette, num_steps)) %>%
  #   mutate(scores = purrr::map_dbl(evolved, score_palette, num_steps))
  
  # get the one with the highest score
  scores <- sapply(evolved_list, score_palette, num_steps)
  best <- evolved_list[scores == max(scores)][[1]]  # add the last [[1]] in case of tie
    
  # best <- children %>%
  #   filter(scores == max(scores)) %>%
  #   pull(evolved) %>%
  #   unlist()
  
  return (best)
  # use lapply to get a score for each evolved palette
  #scores <- sapply(evolved_list, score_palette, num_steps)
  
}

evolve_palette <- function(parent_coords, num_steps, num_children){
  # set up the parent
  #parent_coords <- c(var12min, var1max, var2max, var12max)
  
  # our children will go into a tibble
  # note: did it originally with lists & lapply but I prefer tidier
  children <- tibble::tibble( children = rep(list(parent_coords),
      times = num_children))

  # use lapply to apply spawn_palette() to each child in the list
  #evolved_list <- lapply(child_list, spawn_palette, num_steps)
  children <- children %>%
    mutate(evolved = purrr::map(children, spawn_palette, num_steps)) %>%
    mutate(scores = purrr::map_dbl(evolved, score_palette, num_steps))
  
  # get the one with the highest score
  best <- children %>%
    filter(scores == max(scores)) %>%
    pull(evolved) %>%
    unlist()
  
  return (best)
  # use lapply to get a score for each evolved palette
  #scores <- sapply(evolved_list, score_palette, num_steps)
  
}

# score a palette for colourblindness-friendliness
score_palette <- function (palette_coords, num_steps){
  # generate a palette
  p <- new_bivariate_palette(var12min = palette_coords[[1]], 
                             var1max = palette_coords[[2]], 
                             var2max = palette_coords[[3]], 
                             var12max = palette_coords[[4]], 
                             num_steps = num_steps)
  
  # apply the colourblindness check
  # this is the bottleneck for now: ~ 26ms per child! for 10 it's ~260ms!
  results <- colorblindcheck::palette_check(p)

  ## SCORING! This is a crucial design decision.
  # Let's use minimum distance, so we try to make it as big as possible
  score <- min(results$min_dist)
  
  return (score)
}

#spawn_palette(var12min, var1max, var2max,var12max, num_steps)

spawn_palette <- function(parent_coords, num_steps){#var12min, var1max, var2max,var12max, num_steps){
  # setup variable: the standard deviation of the change to RGB values
  # RGB values range frm 0-255
  rgb_sd <- 15
  
  # turn it into a length-4 vector for easier indexing
  #parent_coords <- c(var12min, var1max, var2max, var12max)
  child_coords <- parent_coords
  
  pal_parent <- new_bivariate_palette(var12min = parent_coords[[1]], 
                                      var1max = parent_coords[[2]], 
                                      var2max = parent_coords[[3]], 
                                      var12max = parent_coords[[4]], 
                                      num_steps = num_steps)

  # how many of the palette's anchor colours will we change?
  num_to_change <- sample(1:4, size = 1, replace = FALSE,
         prob = c(.5,.3,.15,.05))
  
  # and which anchor colours will we change?
  cols_to_change <- sample(1:4, 
                           size = num_to_change)
  
  # let's change each of the anchor colours we've decided to change
  for (i in 1:num_to_change){
    # figure out which of the RGB values we're going to change
    num_rgbs_to_change <- sample(1:3, size = 1, replace = FALSE,
                                 prob = c(.5,.3,.2))
    
    # figure out which ones to change
    rgbs_to_change <- sample(1:3, size = num_rgbs_to_change)
    
    # get the original colour's hex, then convert it to rgb vector
    orig_hex <- parent_coords[[cols_to_change[[i]]]]
    
    col_rgb <- col2rgb(orig_hex) %>% as.vector()
    
    # change the rgb values we decided to change
    for (j in 1:num_rgbs_to_change){
      # how much should we change it?
      change_val <- rnorm(n=1, mean=0, sd=rgb_sd) %>%
        round(digits = 0)
      col_rgb[[rgbs_to_change[[j]]]] <- col_rgb[[rgbs_to_change[[j]]]] + change_val
    }
    
    # make sure no values are above 255 or below 0
    # want to use purrr::map but am trying to make sure I know the base functions
    col_rgb <- sapply(col_rgb, min, 255)
    col_rgb <- sapply(col_rgb, max, 0)
    
    # get it back into hex form  
    new_hex <- rgb(col_rgb[[1]], col_rgb[[2]], col_rgb[[3]], maxColorValue = 255)
    
    child_coords[[cols_to_change[[i]]]] <- new_hex
  }
  
  return(child_coords)
}
 
 
 # plot_palette(p)
# 
# ggplot_palette(p)
# 
# ggplotly_palette(p)
# # 
# 

# 





###
new_bivariate_palette <- function(var12min, var1max, var2max, var12max, num_steps){
  
  # set up matrix to be the palette
  new_palette <- matrix(0, nrow = num_steps, ncol = num_steps)
  
  # matrices convert to vectors column-wise, so we have to do it sideways
  # do left column
  new_palette[,num_steps] <- interpolate_colours(var1max, var12max, steps = num_steps) 
  
  # do right column
  new_palette[,1] <- interpolate_colours(var12min, var2max, num_steps) 
  
  # do rows
  for (col_num in 1:num_steps){
    new_palette[col_num,] <- interpolate_colours(new_palette[col_num, 1], new_palette[col_num, num_steps], num_steps)
  }
  
  return(c(new_palette))
}

#c(new_palette)


##############

# plot the palette, not using ggplot
plot_palette <- function(new_palette){
  image(matrix(seq_along(new_palette), nrow = sqrt(length(new_palette)), byrow = TRUE),
        axes = FALSE, 
        col = new_palette, 
        asp = 1)
}

# plot a single swatch non-interactively
ggplot_swatch <- function(swatch){
  
  swatch_df <- data.frame(x=0,y=0,palette = swatch)
  
  ggplot() +
    geom_raster(data = swatch_df,
                aes(x=x,y=y,fill=palette)) +
    scale_x_continuous(expand = c(0,0), labels = NULL) +
    scale_y_continuous(expand = c(0,0), labels = NULL) +
    labs(x=NULL,
         y=NULL)  +
    theme(axis.ticks.y.left = element_blank(),
          axis.ticks.x.bottom = element_blank(),
          aspect.ratio = 1) +
    scale_fill_identity()
  
}

# plot the palette using ggplotly. assumes square palette
ggplotly_palette <- function(palette, verbose = TRUE){
  
  if (verbose) message("Re-drawing plot")
  
  num_steps <- sqrt(length(palette))
  
  if (verbose) message(paste0("Num steps: ", sqrt(length(palette))))
  
  
  swatches <- data.frame(y = rep(1:num_steps, times = num_steps),
                         x = unlist(lapply(1:num_steps, rep, times = num_steps)),
                         palette = p)
  
  swatch_plot <- ggplot() +
    geom_raster(data = swatches,
                aes(x=x,y=y,fill=palette,
                    text = paste0("Variable 1:",y,
                                     "<br>Variable 2:",x,
                                     "<br>Colour: ",palette))) +
    scale_x_continuous(expand = c(0,0), labels = NULL) +
    scale_y_continuous(expand = c(0,0), labels = NULL) +
    labs(x="Variable 2",
         y="Variable 1",
         fill = NULL) +
    theme(axis.ticks.y.left = element_blank(),
          axis.ticks.x.bottom = element_blank(),
          #aspect.ratio = 1
          ) +
    scale_fill_identity()

  #swatch_plot
  
  plotly::ggplotly(swatch_plot, tooltip = c("text")) %>%
    plotly::hide_legend()
}


# plot the palette using ggplotly. assumes square palette
ggplot_palette <- function(pal, use_plotly = FALSE, verbose = TRUE){
  
  if (verbose) message("Re-drawing plot")
  if (verbose) message(paste0("Palette:", pal))
  
  num_steps <- sqrt(length(pal))
  
  if (verbose) message(paste0("Num steps: ", sqrt(length(pal))))
  
  swatches <- data.frame(y = rep(1:num_steps, times = num_steps),
                         x = unlist(lapply(1:num_steps, rep, times = num_steps)),
                         pal = pal)
  
  if (verbose) message(swatches)
  
  swatch_plot <- ggplot() +
    geom_raster(data = swatches,
                aes(x=x,y=y,fill=pal,
                    text = paste0("Variable 1:",y,
                                  "<br>Variable 2:",x,
                                  "<br>Colour: ",pal))) +
    scale_x_continuous(expand = c(0,0), labels = NULL) +
    scale_y_continuous(expand = c(0,0), labels = NULL) +
    labs(x="Variable 2",
         y="Variable 1",
         fill = NULL) +
    theme(axis.ticks.y.left = element_blank(),
          axis.ticks.x.bottom = element_blank(),
          aspect.ratio = 1
    ) +
    scale_fill_identity()
  
  if (!use_plotly) return(swatch_plot)
  
  plotly::ggplotly(swatch_plot, tooltip = c("text")) %>%
    plotly::hide_legend() %>%
    plotly::config(displayModeBar = FALSE)
}

#interpolated <- interpolate_colours(var1max, var12max, 50)

# function takes two character hex values and returns a character vector
# of num_steps hex values with a smooth colour gradient
interpolate_colours <- function(col1, col2, steps){
  
  reds <- interpolate_values(col2rgb(col1)[[1]], col2rgb(col2)[[1]], steps)
  greens <- interpolate_values(col2rgb(col1)[[2]], col2rgb(col2)[[2]], steps)
  blues <- interpolate_values(col2rgb(col1)[[3]], col2rgb(col2)[[3]], steps)
  
  interpolated <- rgb(reds, greens, blues, maxColorValue = 255)
  return(interpolated)
}

#interpolated <- paste0("#",reds, greens, blues) 



# ##############
# image(matrix(seq_along(interpolated)),
#       axes = FALSE, 
#       col = interpolated, 
#       asp = 1)



##############
interpolate_values <- function(s, e, steps){
  newcols <- vector(mode = "integer", length = (steps))
  newcols[[1]] <- s
  newcols[[steps]] <- e
  
  for (i in 1:(steps-2)){
    newcols[[i+1]] <- round(s + i * (e - s)/(steps-1))
  }

  # extra make sure we don't return any values greater than 255
  newcols <- sapply(newcols, min, 255)
  
  return (newcols)
}




# 
# 
# rgb(1,2,3, maxColorValue = 255)
# 
# var1_rgb <- col2rgb(var1max)
# var2_rgb <- col2rgb(var2max)
# var12max_rgb <- col2rgb(var12max)
# var12min_rgb <- col2rgb(var12min)
# 
# 
# # sequence between var1max and var12max
# 
# 
# r_start <- var1_rgb[[1]]
# g_start <- var1_rgb[[2]]
# b_start <- var1_rgb[[3]]
# 
# r_end <- var12max_rgb[[1]]
# g_end <- var12max_rgb[[2]]
# b_end <- var12max_rgb[[3]]
# 
# s <- r_start
# e <- r_end