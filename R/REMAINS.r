#' REMAINS: A package for simulating landscape dynamics in mountain landscapes
#' 
#' The REMAINS package provides a set of functions to simulate the main landscape-level processes 
#' driving fire-vegetation dynamics in mountain landscapes of northern Iberian Peninsula.
#' It includes the main anthropogenic and natural (abiotic and biotic) drivers of landscape change to study 
#' their spatio-temporal interactions and feedback effects.
#' 
#' @section Author(s):
#' \bold{Maintainer}: Núria Aquilué \email{nuria.aquilue@ctfc.cat}  
#' 
#' \bold{Authors}: Núria Aquilué, Silvana Pais, Adrián Regos
#'
#' @docType package
#' @name REMAINS
#' 
#' @importFrom tidyr %>% pivot_wider
#' @importFrom dplyr group_by summarise filter select mutate count left_join 
#' @importFrom RANN nn2
#' @importFrom stats aggregate dist quantile rexp rlnorm runif
#' @importFrom utils write.table
NULL
#> NULL
