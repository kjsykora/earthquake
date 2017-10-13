
#' Create timeline plots
#'
#' `stat_timeline` is a useful tool for filter operations on timeline geoms
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#'
#' @param n_max Optional parameter that determines the max number of earthquakes to plot/annotate
#'
#' @section Aesthetics:
#' `geom_timeline` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{xmin}
#'   \item \strong{xmax}
#'   \item mag
#' }
#'
#' @rdname geom_timeline
#'


stat_timeline <- function(mapping = NULL
                          , data = NULL
                          , geom = 'timeline'
                          , position = 'identity'
                          , na.rm = FALSE
                          , show.legend = NA
                          , inherit.aes = TRUE
                          , n_max = NULL
                          , ...){
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL


StatTimeline <- ggproto("StatTimeline", Stat,
                        required_aes = c("x", "xmin", "xmax"),
                        optional_aes = c("mag"),
                        compute_group = function(data, scales, n_max = NULL) {
                          if(! is.null(n_max) ){
                            if(is.null(data$mag)){
                              stop("Must supply mag aesthetic if you want to filter on n_max")
                            }
                            #filter on date range
                            temp <- data[data$x > data$xmin & data$x < data$xmax, ]
                            #order by desc mag
                            temp <- temp[order(-temp$mag), ]
                            #return n_max or n_earthquakes, whichever is smaller
                            return_number <- base::min(n_max, base::dim(temp)[1])
                            return(temp[1:return_number,])
                          } else{
                            #just filter on date range
                            return(data[data$x > data$xmin & data$x < data$xmax, ])
                          }

                        })



#' Creates a timeline of earthquakes
#'
#' `geom_timeline()` reads in cleaned data and creates timeline plots of earthquakes.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics:
#' `geom_timeline` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{xmin}
#'   \item \strong{xmax}
#'   \item y
#'   \item colour
#'   \item size
#'   \item alpha
#'   \item shape
#'   \item fill
#'   \item stroke
#' }
#'
#'
#' @export


geom_timeline <- function(mapping = NULL
                          , data = NULL
                          , stat = 'timeline'
                          , position = 'identity'
                          , na.rm = FALSE
                          , show.legend = NA
                          , inherit.aes = TRUE
                          , ...){
  ggplot2::layer(
    geom = GeomTimeline
    , mapping = mapping
    , data = data, stat = stat, position = position
    , show.legend = show.legend, inherit.aes = inherit.aes
    , params = list(na.rm, ...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c("x", "xmin", "xmax"),
                        optional_aes = c("y", "colour", "size", "alpha"),
                        default_aes = aes(y = 0.2, colour = 'black', size = 4, alpha = NA, shape = 16, fill = NA, stroke = 0.5),
                        draw_key = draw_key_point,
                        draw_panel = function(data, panel_params, coord, na.rm = FALSE){

                          coords <- coord$transform(data, panel_params)

                          grid::pointsGrob(

                            x = coords$x, y = as.numeric(coords$y),

                            pch = coords$shape,

                            gp = grid::gpar(
                              col = ggplot2::alpha(coords$colour, coords$alpha),
                              fill = ggplot2::alpha(coords$fill, coords$alpha),

                              fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                              lwd = coords$stroke * ggplot2::.stroke / 2

                            )
                          )
                        }
)



#' Create timeline labels
#'
#' Provides labels for a timeline generated from `geom_timeline()`
#' `geom_timelinelabel()` reads in cleaned data and creates labels for the data points
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#'
#' @inheritParams ggplot2::geom_text
#'
#' @section Aesthetics:
#' `geom_timelinelabel` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{label}
#'   \item \strong{xmin}
#'   \item \strong{xmax}
#'   \item y
#'   \item mag
#'   \item colour
#'   \item size
#'   \item angle
#'   \item hjust
#'   \item vjust
#'   \item alpha
#'   \item family
#'   \item fontface
#'   \item lineheight
#'   \item linesize
#'   \item linecolor
#' }
#'
#' @rdname geom_timeline
#'
#'
#' @export


geom_timelinelabel <- function(mapping = NULL
                          , data = NULL
                          , stat = 'timeline'
                          , position = 'identity'
                          , na.rm = FALSE
                          , show.legend = NA
                          , inherit.aes = TRUE
                          , ...){
  ggplot2::layer(
    geom = GeomTimelineLabel
    , mapping = mapping
    , data = data, stat = stat, position = position
    , show.legend = show.legend, inherit.aes = inherit.aes
    , params = list(na.rm, ...)
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c("x", "label", "xmin", "xmax"),
                             optional_aes = c("y", "mag"),
                             default_aes = aes(y = 0.2, colour = "black", size = 3.88, angle = 45, hjust = "left",
                                               vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2, linesize = 0.5, linecolor = "grey20"),
                             draw_key = draw_key_text,
                             draw_panel = function(data, panel_params, coord, na.rm = FALSE){

                               lab <- data$label


                               data$y <- data$y+0.15


                               if (is.character(data$vjust)) {
                                 data$vjust <- compute_just(data$vjust, data$y)
                               }
                               if (is.character(data$hjust)) {
                                 data$hjust <- compute_just(data$hjust, data$x)
                               }

                               coords <- coord$transform(data, panel_params)

                               data2 <- data
                               data2$size <- data2$linesize
                               data2$xend <- data2$x
                               data2$y <- data2$y-0.15
                               data2$yend <- data2$y+0.13
                               data2$colour <- data2$linecolor

                               grobTree(
                                 grid::textGrob(
                                   lab,
                                   x = coords$x, y = coords$y,
                                   default.units = "native",
                                   hjust = data$hjust, vjust = data$vjust,
                                   rot = data$angle,


                                   gp = grid::gpar(
                                     col = alpha(data$colour, data$alpha),
                                     fontsize = data$size * .pt,
                                     fontfamily = data$family,
                                     fontface = data$fontface,
                                     lineheight = data$lineheight
                                   )
                                 ),ggplot2::GeomSegment$draw_panel(data = data2, panel_params, coord)

                               )

                             }
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}


