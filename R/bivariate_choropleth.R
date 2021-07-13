#green/pink-purple/black #FFFFFF, #4A9433, #BE44F3, #001400
#blue/brown-gold-red/black
 # sedate blue and brown-gold  # #F9FFFF, #3A81AE, #A28F37, #29050D
 # brighted blue and orange # #F7FFFF, #2B70FF, #F15E08, #09000F



#

#' Make a Bivariate Choropleth with ggplot2, cowplot, and ggiraph
#'
#' @param df
#' @param var1_name
#' @param var2_name
#' @param var1_label
#' @param var2_label
#' @param polygon_names
#' @param na_values
#' @param plot_title
#' @param plot_subtitle
#' @param legend_xpos
#' @param legend_ypos
#' @param legend_size
#' @param interactive
#' @param label_arrows
#'
#' @return
#' @export
#'
#' @examples
bivariate_choropleth <- function (df, var1_name, var2_name,
                                  var1_label= "var1", var2_label = "var2",
                                  polygon_names,
                                  na_values = c("zero","drop_na"),    # what happens to NAs in var1 and var2?
                                  plot_title = NULL, plot_subtitle = NULL,
                                  legend_xpos = 0.75,
                                  legend_ypos = 0.1,
                                  legend_size = 0.15,
                                  interactive = TRUE,
                                  label_arrows = TRUE){

  # handle NA values in the two variables of interest
  na_values <- match.arg(na_values, na_values)

  # if we're adding arrows to the variable labels, do that here
  if (label_arrows){
    var1_label <- paste0(var1_label, "\u2192")
    var2_label <- paste0(var2_label, "\u2192")
  }

  # create a renamed dataframe to work with
  forplot <- dplyr::rename(df,
                           var1 = tidyselect::all_of(var1_name),
                           var2 = tidyselect::all_of(var2_name),
                           Name = tidyselect::all_of(polygon_names))

  # handle NA values
  if (na_values == "zero"){
    forplot <- dplyr::mutate(forplot,
                             var1 = dplyr::if_else(is.na(var1), 0, var1),
                             var2 = dplyr::if_else(is.na(var2), 0, var2))
  }
  if (na_values == "drop_na"){
    forplot <- tidyr::drop_na(forplot, var1, var2)
  }

  # convert raw values to n-tiles
  forplot <- forplot %>%
    dplyr::mutate(var1_ntile = dplyr::ntile(n=3, var1),
                  var2_ntile = dplyr::ntile(n=3, var2),
                  uni_dim_fill = (var1_ntile-1)*3 + var2_ntile)

  ###### DEFINE OUR COLOUR SCALES
  # https://rpubs.com/ayushbipinpatel/593942
  # https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  bivariate_color_scale <- tibble::tribble(
    ~var1, ~var2, ~fill,
    3,  3,  "#3F2949", # high inequality, high income
    2,  3,  "#435786",
    1,  3,  "#4885C1", # low inequality, high income
    3,  2,  "#77324C",
    2,  2,  "#806A8A", # medium inequality, medium income
    1,  2,  "#89A1C8",
    3,  1,  "#AE3A4E", # high inequality, low income
    2,  1,  "#BC7C8F",
    1,  1,  "#CABED0" # low inequality, low income
  )

  ## TODO: Make the palette an input to the function. for now:
  bivariate_color_scale <- tibble::tibble(
    var1 = c(1,1,1,2,2,2,3,3,3),
    var2 = rep(c(1,2,3), times= 3),
    fill = pals::stevens.purplegold())


  bivariate_color_scale_unidim <- bivariate_color_scale %>%
    dplyr::mutate(uni_dim_fill = (var1-1)*3 + var2) %>%
    dplyr::select(uni_dim_fill, fill)



  forplot <- dplyr::left_join(forplot,
                              bivariate_color_scale_unidim,
                              by = "uni_dim_fill")

  # create the map plot
  var1_tooltip <- stringr::str_replace_all(var1_label, "\\n", " ") %>%
    stringr::str_squish()

  var2_tooltip <- stringr::str_replace_all(var2_label, "\\n", " ") %>%
    stringr::str_squish()

  p <- forplot %>%
    ggplot2::ggplot() +
    ggiraph::geom_sf_interactive(aes(tooltip = paste0("<b>",Name,"</b>",
                                                      "\n",var1_tooltip,":",
                                                      "<ul><li>Tertile: ", var1_ntile,"</li>",
                                                      "<li>Raw Value: ", round(var1, digits = 1),"</li></ul>",
                                                      "",var2_tooltip,": ",
                                                      "<ul><li>Tertile: ", var2_ntile,"</li>",
                                                      "<li>Raw Value: ", round(var2, digits = 2),"</li></ul>",
                                                      "\nFill Colour:",fill),
                                     fill = fill),
                                 size = .2) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_identity() +
   # theme_map() +
    ggplot2::labs(title = plot_title,
                  subtitle = plot_subtitle)

  # create the legend
  legend <- ggplot2::ggplot() +
    map_theme_sparse() +
    ggplot2::geom_tile(
      data = bivariate_color_scale,
      mapping = ggplot2::aes(
        x = var1,
        y = var2,
        fill = fill)
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = var1_label,
                  y = var2_label) +
    # make font small enough
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 6)
    ) +
    # quadratic tiles
    ggplot2::coord_fixed()

  t <- cowplot::ggdraw() +
    cowplot::draw_plot(p, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, legend_xpos, legend_ypos, legend_size, legend_size)

  # return either the ggdraw object or the ggiraph object dependinf on interactive flag
  result <- t

  if (interactive) result <- ggiraph::girafe(ggobj = t) #%>%ggiraph::girafe_options(opts_zoom(min = 1, max = 5) )

  return (result)
}





#' Add a Bivariate Choropleth to a Leaflet Map
#'
#' @param map A Leaflet map created by `leaflet::leaflet()`.
#' @param map_data
#' @param var1_name
#' @param var2_name
#' @param ntiles
#' @param var1_label
#' @param var2_label
#' @param label_arrows
#' @param region_names
#' @param paletteFunction
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
addBivariateChoropleth <- function(map, map_data, var1_name, var2_name, ntiles = 3,
                                   var1_label = NA, var2_label = NA,
                                   label_arrows = TRUE,
                                   region_names = NA,
                                   paletteFunction = pals::stevens.pinkblue, ...){

  if (is.na(var1_label)) var1_label <- rlang::enexpr(var1_name)
  if(is.na(var2_label))  var2_label <- rlang::enexpr(var2_name)


  # separate labels for the palette and the map, if so desired
  var1_pal_label <- var1_label
  var2_pal_label <- var2_label

  # are we putting fun arrows on the labels?
  if (label_arrows){
    var1_pal_label <- paste0(var1_label, " \U2192")
    var2_pal_label <- paste0(var2_label, " \U2192")
  }

  # the pals package has many palettes for bivariate choropleths.
  # NOTE!! That all of them are 2x2 or 3x3. larger matrices will probably be
  # harder to interpret, and it looks like people don't use them
  bivar_pal <- function(x) paletteFunction(n=ntiles^2)[x]

    forplot <- map_data %>%
    rename (var1 = {{var1_name}},
            var2 = {{var2_name}}) %>%
    mutate(var1_ntile = dplyr::ntile(var1, n = ntiles),
           var2_ntile = dplyr::ntile(var2, n = ntiles),
           pal_num = var1_ntile + (var2_ntile - 1)*ntiles,#(ntiles -var1_ntile)*3 + var2_ntile,
           pal_colour = bivar_pal(pal_num))

    # FIXME: region names not working
    if (!is.na(region_names)) forplot <- dplyr::rename(forplot,
                                                       region_name_label = region_names)

  # set up some css for the html palette swatch
  palette_size_px <- 120
  swatch_size_px <- round(palette_size_px / ntiles)

  row_col_px <- rep(paste0(swatch_size_px,"px"), times = ntiles) %>%
    stringr::str_flatten(collapse = " ") %>%
    paste0(., ";")


  div_var1 <- paste0('<div class = "var1-label" style="grid-row-start:1; grid-row-end:',(ntiles+1),'; text-align: center; writing-mode: tb-rl;
        transform: rotate(-180deg);">',var1_pal_label,'</div>')
  div_var2 <- paste0('<div style="text-align:center; grid-column:2 / ',(ntiles+2),';">',var2_pal_label,'</div>')

  # set up the indices for the palette
  div_indices <- matrix((1:ntiles^2),
                        nrow=ntiles,
                        ncol = ntiles,
                        byrow = TRUE)

  div_indices <- div_indices[,c(ntiles:1)]

  # set up the divs for the palette squares
  divs <- paste0('<div style="background-color:',bivar_pal(div_indices),
                 '; color:',bivar_pal(div_indices),
                 ';">',div_indices,' </div>') %>%
    stringr::str_flatten()

  # combine the above bits with a css grid wrapper for the html palette
  palette_html <- paste0(
    '<style> .grid-container { display: grid;
    grid-template-columns: 40px ',row_col_px,
    'grid-auto-rows: ',row_col_px,' 40px;','}
    </style>
    <div class="grid-container">',
    div_var1,
    divs,
    div_var2,
    '</div>')

  labs <- paste0("<b>",var1_label,"</b>",
                 "<br>Ntile: ", forplot$var1_ntile,
                 "<br>Value: ", forplot$var1,
                 "<br><b>",var2_label,"</b> ",
                 "<br>Ntile: ",forplot$var2_ntile,
                 "<br>Value: ", forplot$var2)

  # FIXME: region names not working
  if(!is.na(region_names)) labs <- paste0("<b>",forplot$region_name_label,"</b><br>",labs)

  labs <- purrr::map(labs, htmltools::HTML)

  map %>%
    leaflet::addControl(
      html = palette_html,
      position = "bottomleft"
    ) %>%
    addPolygons(data = forplot,
                label = labs,
                fillColor = ~pal_colour,
                ...)

}


# function for removing legend entries etc.
# add it to a ggplot object
map_theme_sparse <- function(){
    ggplot2::theme(
      axis.ticks = element_blank(),
      #axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank()
    )
}
