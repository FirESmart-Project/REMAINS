#' Annual area burnt in Galicia
#'
#' Discrete distribution of annual area burnt in Galicia observed between 1986-2011 
#'
#' @format A data frame with as many rows as grid cells in the study area and 2 variables:
#' \describe{
#'   \item{upper.th}{Upper threshold of discrete intervals of annual area burnt}
#'   \item{freq}{Number of fires per class}
#' }
#' 
"burnt.area.dist.gz"

#' Annual area burnt in Portugal
#'
#' Discrete distribution of annual area burnt in Portugal observed between 1980-2016 
#'
#' @format A data frame with as many rows as grid cells in the study area and 2 variables:
#' \describe{
#'   \item{upper.th}{Upper threshold of discrete intervals of annual area burnt}
#'   \item{freq}{Number of fires per class}
#' }
#' 
"burnt.area.dist.pt"

#' UTM coordinates 
#'
#' Coordinates x and y in UTM29N-ETRS89 of the 1 ha cells in the study area
#'
#' @format A data frame with as many rows as grid cells in the study area and 3 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{x}{UTM coordinate X}
#'   \item{y}{UTM coordinate Y}
#' }
#' 
"coords"

#' Fire size distribution in Galicia
#'
#' Discrete distribution of annual area burnt in Galicia observed between 1986-2011 
#'
#' @format A data frame with two variables:
#' \describe{
#'   \item{upper.th}{Upper threshold of discrete intervals of fire size}
#'   \item{freq}{Number of fires per class}
#' }
#' 
"fire.size.dist.gz"

#' Fire size distribution in Portugal
#'
#' Discrete distribution of annual area burnt in Portugal observed between 1980-2016
#'
#' @format A data frame with two variables:
#' \describe{
#'   \item{upper.th}{Upper threshold of discrete intervals of fire size}
#'   \item{freq}{Number of fires per class}
#' }
#' 
"fire.size.dist.pt"

#' Forest landscape features of the Biosphere Reserve Ger?s-Xur?s
#'
#' Landscape and forest stands characteristics of the study area in 2020 at 1 ha of spatial resolution 
#'
#' @format A data frame with as many rows as grid cells in the study area and 3 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{lct}{Tree species or land-cover types: pine, oak, water, shrub, sparseveg, crop, grass, urban}
#'   \item{tschg}{Time since change (in years)}
#' }
#' 
"landscape"

#' Induced landscape changes
#'
#' Induced landscape changes: rural abandonment, agricultural conversion and smart plantations
#'
#' @format A data frame with as many rows as grid cells in the study area and 3 variables:
#' \describe{
#'   \item{SmartPlant}{Conversion rate for smart plantations: oaks spp (in ha/year)}
#'   \item{AgriConver}{Conversion rate for agricultural conversion (in ha/year)}
#'   \item{RuralAbnd}{Conversion rate for rural abandonment (in ha/year)}
#' }
#' 
"lcc.demand"

#' Mask of the study area
#'
#' Binary raster to identify the study area (1 or NA) 
#'
#' @format Raster of 648 (nrow) x 791 (ncol)
#' \describe{
#' Raster of the study area (1 or NA) in the UTM29N - ETRS89 projection, at 1 ha of spatial resolution. 
#' The unique grid cell identificator \code{cell.id} in the \code{landscape} data frame coincides with 
#' the position of the location in the \code{mask} raster.
#' }
#' 
"mask.study.area"

#' Orography 
#'
#' Orographic characteristics of the study area
#'
#' @format A data frame with as many rows as grid cells in the study area and 8 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{region}{\code{GZ} is Galicia and \code{PT} is Portugal)}
#'   \item{protected}{protected status (\code{TRUE}/\code{FALSE})}
#'   \item{elevation}{Elevation (in m)}
#'   \item{aspect}{Aspect, 0 - flat, 1 - north, 2 - east, 3 - south, 4 - west}
#'   \item{dens.road}{density of roads (in m^2^)}
#'   \item{slope}{Slope (in %)}
#'   \item{utm}{Code of the 1K UTM grid}
#' }
#' 
"orography"

#' Post-fire transitions matrix
#'
#' Post-fire transition rates calibrated for the study area 
#'
#' @format A data frame with as many rows and columns as land-cover types in the study area:
#' \describe{
#'   \item{lc.prefire}{Land-cover types}
#'   \item{crop}{Croplands}
#'   \item{grass}{Grasslands}
#'   \item{shrub}{Shrublands}
#'   \item{pine}{Pine forest}
#'   \item{oak}{Oak forest}
#'   \item{sparseveg}{Sparse vegetation and rocky areas}
#'   \item{water}{Water}
#'   \item{urban}{Urban}
#'   \item{baresoil}{baresoil}
#' }
#' 
"postfire.trans.matrix"

