#' Compute fire risk as a function of landscape susceptibility, danger, and damage 
#' 
#' @name fire.risk
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param ha.cell Number of hectares each raster cell in the grid represents
#'
#' @return Returns a vector with the fire risk of the raster cells that is function fo the land-cover type and 
#' the orography of the landscape
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' res = fire.risk(landscape, 1)
#' 
 
fire.risk = function(land, ha.cell){
  
  ## Susceptibility = gradient * land-cover type (it's 0 for urban and water)
  gradient = ifelse(orography$slope<=5, 2,
                    ifelse(orography$slope<=10, 3,
                           ifelse(orography$slope<=15, 4,
                                  ifelse(orography$slope<=20, 5, 6))))
  land.fire.suscep = ifelse(land$lct %in% c("urban", "water"), 0,
                            ifelse(land$lct %in% c("baresoil", "crop"), 2, 
                                   ifelse(land$lct == "grass", 3, 4)))
  
  ## Danger = Fire probability * Susceptibility
  danger = land$prob.fire * gradient * land.fire.suscep
  
  ## Damage = Vulnerability * Value (Vulnerability is 0 for crops, grass, and water)
  vulner = ifelse(land$lct == "pine", 1,
              ifelse(land$lct == "oak", 0.6,
                     ifelse(land$lct == "urban", 0.75,
                          ifelse(land$lct %in% c("shrub", "shrub2pine", "shrub2oak", "sparseveg"), 0.4, 0))))
  valor = ifelse(land$lct == "pine", 91*ha.cell,
                  ifelse(land$lct == "oak", 87*ha.cell,
                         ifelse(land$lct == "urban", 0.074148*ha.cell,
                                ifelse(land$lct %in% c("shrub", "shrub2pine", "shrub2oak", "sparseveg"), 52*ha.cell, 0))))
  damage = vulner * valor + 10^(-6)

  ## Risk = Damage * Danger
  risk = vulner * danger
  return(risk)  
}
