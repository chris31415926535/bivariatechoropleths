# getwd()
# library(tidyverse)
# source("R/palettes.R")
#
# #remotes::install_github("nowosad/colorblindcheck")
# # library(tidyverse)
# # library(plotly)
#
# # # pals::stevens.greenblue()
# # var1max <- "#6c83b5"
# # var2max <- "#73ae80"
# # var12max <- "#2a5a5b"
# # var12min <- "#e8e8e8"
# #
# p_source <- pals::brewer.seqseq2()
# # p_source <- pals::stevens.greenblue()
# #p_source <- pals::stevens.purplegold()
#
# var12min <- p_source[[1]]
# var1max <-  p_source[[3]]
# var2max <-  p_source[[7]]
# var12max <- p_source[[9]]
#
# num_steps <- 4
#
# p <- new_bivariate_palette(var12min = var12min, var1max = var1max, var2max = var2max, var12max = var12max, num_steps = num_steps)
#
# colorblindcheck::palette_check(p, bivariate = TRUE)
# colorblindcheck::palette_bivariate_plot(p)
#
# palette_coords <- c(var12min, var1max, var2max, var12max)
#
# new_palette <- palette_evolution(palette_coords,
#                                  num_steps,
#                                  num_children = 10,
#                                  num_generations = 1,
#                                  verbose = TRUE)
#
#
# bench::mark( palette_evolution(palette_coords,
#                                num_steps,
#                                num_children = 10,
#                                num_generations = 1,
#                                verbose = TRUE))
#
# pp <- new_bivariate_palette(new_palette[[1]],
#                             new_palette[[2]],
#                             new_palette[[3]],
#                             new_palette[[4]],
#                             num_steps)
#
# colorblindcheck::palette_check(pp, bivariate = TRUE, plot = TRUE)
# colorblindcheck::palette_check(p, bivariate = TRUE, plot = TRUE)
#
