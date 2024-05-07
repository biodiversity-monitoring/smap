
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggrepel)


#' Generate Geographic Boundary as sf Object
#'
#' This function generates a rectangular geographic boundary covering the whole globe, specified by
#' a total number of points. The generated boundary is a simple rectangle with the extremes
#' being the maximum latitudes (90 and -90) and longitudes (180 and -180). This rectangle
#' can serve multiple purposes in spatial analysis, such as creating a universal background, or
#' serving as a basis for cropping or overlaying other spatial data.
#'
#' @param n_points An integer indicating the total number of points that form the perimeter
#'                 of the rectangle. To accurately form a closed rectangle, the number must be
#'                 at least 8 (4 corners with at least one point on each side).
#'
#' @return A `sf` (simple features) object representing the rectangular boundary as a polygon.
#'         The function relies on the `sf` package to ensure compatibility with spatial operations.
#'
#' @export
#'
#' @examples
#' # Generate a boundary covering the whole globe with 3000 points and plot it.
#' boundary <- generate_boundary(n_points = 3000)
#' plot(boundary)
generate_boundary <- function(n_points = 3000) {
  if(n_points < 8) {
    stop("n_points must be at least 8 to form a valid boundary.")
  }

  # Calculate the number of points per side, excluding corners.
  points_per_side <- (n_points - 4) / 4

  # Generate points for each side of the rectangle.
  top_side <- cbind(seq(-180, 180, length.out = points_per_side + 1), rep(90, points_per_side + 1))
  bottom_side <- cbind(seq(180, -180, length.out = points_per_side + 1), rep(-90, points_per_side + 1))
  right_side <- cbind(rep(180, points_per_side - 1), seq(90, -90, length.out = points_per_side - 1))
  left_side <- cbind(rep(-180, points_per_side - 1), seq(-90, 90, length.out = points_per_side - 1))

  # Combine the points into a list, close the polygon by repeating the first point.
  all_points <- rbind(top_side, right_side, bottom_side[-nrow(bottom_side), ], left_side[-nrow(left_side), ], top_side[1, ])

  # Convert the points into a 'sf' polygon object.
  boundary_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(all_points)), crs = 4326))

  return(boundary_sf)
}



#' Plot World Map
#'
#' This function creates a simple, customizable world map. Options include toggling
#' drawing coastlines, setting fill color, and choosing map projection. It relies
#' on 'ggplot2' for plotting, 'rnaturalearth' for geographic data, and 'sf' for spatial
#' operations.
#'
#' @param draw_coastline Logical flag to indicate whether to draw the coastline.
#'                       Default is `TRUE`.
#' @param fill_color The hexadecimal code for the countries' fill color.
#'                   Default is `"#e0e0eb"`, a light grey-blue.
#' @param projection Cartographic projection in PROJ format.
#'                   Default is `"'+proj=robin'"` for Robinson projection.
#'
#' @return A `ggplot` object representing the world map that can be either further customized
#'         with 'ggplot2' syntax or displayed using the `print()` function.
#'
#' @examples
#'
#' # Plot a simple world map with default settings:
#' plot_map()
#'
#' # Plot a world map showing with coastline
#' plot_map(draw_coastline = TRUE,fill_color = "#A6D96A")
#'
#' @export
#'
#' @importFrom sf st_transform
#' @importFrom ggplot2 ggplot geom_sf theme_classic
#' @importFrom rnaturalearth ne_countries ne_coastline
plot_map <- function(draw_coastline = FALSE, fill_color = "#e0e0eb", projection = "+proj=robin") {
  p <- ggplot2::ggplot() + ggplot2::theme_classic()
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  countries_transformed <- sf::st_transform(countries, crs = projection)
  p <- p + ggplot2::geom_sf(data = countries_transformed, fill = fill_color, color = if(draw_coastline) NA else fill_color)
  if (draw_coastline) {
    coastline <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
    coastline_transformed <- sf::st_transform(coastline, crs = projection)
    p <- p + ggplot2::geom_sf(data = coastline_transformed, color = "black", fill = NA)
  }
  boundary_sf <- generate_boundary()
  boundary_transformed <- sf::st_transform(boundary_sf, crs = projection)
  p <- p + ggplot2::geom_sf(data = boundary_transformed, fill = NA, color = "black", size = 0.5)
  return(p)
}


#' Plot Sampling Points on a Map
#'
#' Adds sampling points and their labels to an existing ggplot object, transforming
#' the point coordinates into the specified projection before plotting. It leverages
#' 'sf' for spatial data transformation, 'ggplot2' for plotting, and 'ggrepel' for
#' better label placement to avoid overlap.
#'
#' @param p A ggplot object as the base map for plotting points.
#'          It should be created with `ggplot2::ggplot()` and compatible geometries such
#'          as those created by `ggplot2::geom_sf()`.
#' @param sampling_points A dataframe containing the sampling points. Must include
#'                        'site' (labels), 'x' (longitude), and 'y' (latitude).
#' @param projection The projection in PROJ format to be applied to points.
#'                   Defaults to Robinson projection (`"+proj=robin"`).
#'
#' @return A modified ggplot object including the sampling points and labels.
#'
#' @examples
#' # Generate random sampling points
#' n <- 5
#' x <- runif(n, min = 0, max = 100)
#' y <- runif(n, min = 0, max = 50)
#' site <- paste0("Plot ", 1:n)
#' sampling_points <- data.frame(site = site, x = x, y = y)
#'
#' # Assuming `plot_map` is defined and creates a base map
#' p <- plot_map(fill_color = "gray",  draw_coastline = FALSE)
#'
#' # Add the generated sampling points to the map
#' plot_sampling_points(p, sampling_points)
#' @export
#'
#' @importFrom sf st_as_sf st_transform
#' @importFrom ggplot2 ggplot geom_sf
#' @importFrom ggrepel geom_label_repel
plot_sampling_points <- function(p, sampling_points, projection = "+proj=robin") {
  if (!is.null(sampling_points) && all(c("site", "x", "y") %in% names(sampling_points))) {
    points_sf <- sf::st_as_sf(sampling_points, coords = c("x", "y"), crs = 4326)
    points_transformed <- sf::st_transform(points_sf, crs = projection)
    points_df <- as.data.frame(sf::st_coordinates(points_transformed))
    points_df$site <- sampling_points$site

    pp <- p +
      ggplot2::geom_sf(data = points_transformed, fill = "red", size = 3,shape = 21) +
      ggrepel::geom_label_repel(data = points_df, ggplot2::aes(x = X, y = Y, label = site),
                                nudge_y = 0.5,
                                box.padding = 0.35,
                                point.padding = 0.3,
                                segment.color = 'black',
                                min.segment.length = 0.01) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  } else {
    stop("sampling_points must be a data frame with 'site', 'x', and 'y' columns.")
  }
  return(pp)
}


#' Plot Custom-Colored Biomes Map
#'
#' Visualizes a global map of biomes with custom color coding, with options
#' for overlaying country borders and coastlines. This function depends on the 'sf',
#' 'ggplot2', and 'rnaturalearth' packages to appropriately handle spatial data and mapping.
#' The biome data is expected to be loaded with `data("biomes")`, which should produce
#' an 'sf' object including a column for biome names (`BIOME_NAME`).
#'
#' @param biomes An 'sf' object representing the biome data, expected to include a
#'               `BIOME_NAME` column which matches the keys in `biome_colors`.
#' @param biome_colors A named vector providing colors for each biome, where the names
#'                     must correlate with the unique `BIOME_NAME`s in the `biomes` dataset.
#' @param draw_coastline Logical indicating if the global coastline should be drawn.
#'                       Defaults to `TRUE`.
#' @param projection Map projection parameters in PROJ format.
#'                   Defaults to Robinson projection (`"+proj=robin"`).
#'
#' @return A `ggplot` object depicting the biomes map which can be either directly displayed
#'         using `print()` or further customized.
#'
#' @examples
#' # biomes data can be downloaded from https://ecoregions.appspot.com/
#' library(sf)
#' biomes <- st_read("Ecoregions2017.shp")
#'
#' # Define custom colors for biomes
#' biome_colors <- c(
#'   "Boreal Forests/Taiga" = "#7AB6F5",
#'   "Deserts & Xeric Shrublands" = "#CC6767",
#'   "Flooded Grasslands & Savannas" = "#BEE7FF",
#'   "Mangroves" = "#FE01C4",
#'   "Mediterranean Forests, Woodlands & Scrub" = "#FE0000",
#'   "Montane Grasslands & Shrublands" = "#D6C39D",
#'   "Temperate Broadleaf & Mixed Forests" = "#00734C",
#'   "Temperate Conifer Forests" = "#458970",
#'   "Temperate Grasslands, Savannas & Shrublands" = "#FEFF73",
#'   "Tropical & Subtropical Coniferous Forests" = "#88CE66",
#'   "Tropical & Subtropical Dry Broadleaf Forests" = "#CCCD65",
#'   "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "#FEAA01",
#'   "Tropical & Subtropical Moist Broadleaf Forests" = "#38A700",
#'   "Tundra" = "#9ED7C2"
#' )
#'
#' # Create and display the biomes map, without country borders and with coastline
#' plot_biomes_map(biomes, biome_colors, draw_coastline = TRUE, projection = "+proj=robin")
#'
#' @export
plot_biomes_map <- function(biomes, biome_colors, draw_coastline = TRUE, projection = "+proj=robin") {
  valid_biomes <- biomes[biomes$BIOME_NAME != "N/A", ]
  valid_biomes_transformed <- sf::st_transform(valid_biomes, crs = projection)
  p <- ggplot2::ggplot() + ggplot2::theme_classic()
  p <- p + ggplot2::geom_sf(data = valid_biomes_transformed, aes(fill = BIOME_NAME), color = NA)

  if (draw_coastline) {
    coastline <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
    coastline_transformed <- sf::st_transform(coastline, crs = projection)
    p <- p + ggplot2::geom_sf(data = coastline_transformed, color = "black", fill = NA)
  }
  boundary_sf <- generate_boundary()
  boundary_transformed <- sf::st_transform(boundary_sf, crs = projection)
  p <- p + ggplot2::geom_sf(data = boundary_transformed, fill = NA, color = "black", size = 0.5)

  p <- p + ggplot2::scale_fill_manual(values = biome_colors, name = "Biome")

  p <- p + ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.box.background = ggplot2::element_rect(color = "black", linewidth = 1),
    legend.key.size = grid::unit(0.3, "cm"),
    legend.title = ggplot2::element_text(size = 5),
    legend.text = ggplot2::element_text(size = 5)
  )

  return(p)
}




