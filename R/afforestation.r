#' Simulation of afforestation in shrublands
#'
#' @name afforestation
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model
#'
#' @return Returns a list of two vectors with cells' identifiers afforested by oaks and pines, respectively, 
#' and a data frame with tracking data of the afforestation process 
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = afforestation(landscape, params)
#' 

afforestation = function(land, params){
  
  cat(" Afforestation\n")
  
  ## Count available shrublands that can become oak/pine (n.cells)
  target.coord = land %>% filter(lct == "shrub", tschg > params$rewilding.th) %>% 
    left_join(coords, by = "cell.id") %>% select(cell.id, x, y)
  
  ## According to the annual rates, determine the target for each transition (number of cells)
  target.afforest.oak = round(nrow(target.coord)*params$rate.afforest.oak/100) 	
  target.afforest.pine = round(nrow(target.coord)*params$rate.afforest.pine/100) 	
  track.afforest.oak = target.afforest.oak
  track.afforest.pine = target.afforest.pine
  
  ## Find neighbors in a squared neighborhood of a fixed radius
  cell.radius = round(max(params$radius.afforest.pine, params$radius.afforest.oak)/params$cell.size)
  
  ## Unsorted cell.id to potentially be changed from shrub to forest
  unsort.cell.id = sample(target.coord$cell.id, size = nrow(target.coord), replace=F)
  
  ## Initialize vectors to save the cell.id of those cells that will change
  afforest.pine.cells = afforest.oak.cells = numeric()
  
  ## Because of problems with allocation memory, split the task in sets of 4000 cells
  i=0
  while(track.afforest.pine>0 | track.afforest.oak>0){
  
    ## Find neighbors in a squared neighborhood of a fixed radius for a set of 4000 cells
    set = (i+1):(i+4000)
    neigh.id = nn2(coords[,-1], target.coord[target.coord$cell.id %in% unsort.cell.id[set],-1],  
                   searchtype="priority", k=(cell.radius*2)^2) 
    
    ## PINES
    if(track.afforest.pine>0){
      ## Mark the the presence of pine for all the neighbors
      land$is.target = land$lct=="pine" 
      
      ## Determine if transition happens according to the percentage of pines
      ## in a circular neighborhood of fixed radius 
      ptrans = .neigh.transition(land, neigh.id, params$radius.afforest.pine)
      
      ## Save cell.id for encroachment
      afforest.pine.cells = c(afforest.pine.cells, sample(unsort.cell.id[set][ptrans], 
                            size=pmin(track.afforest.pine, sum(ptrans, na.rm=T)), replace=F))
      
      ## Update the amount of shrub to be converted to pine
      track.afforest.pine = track.afforest.pine - sum(ptrans, na.rm=T)
    }
    
    ## OAKS
    if(track.afforest.oak>0){
      ## Mark the the presence of oak for all the neighbors
      land$is.target = land$lct=="oak" 
      
      ## Determine if transition happens according to the percentage of pines
      ## in a circular neighborhood of fixed radius 
      ptrans = .neigh.transition(land, neigh.id, params$radius.afforest.oak)
      
      ## Save cell.id for encroachment
      afforest.oak.cells = c(afforest.oak.cells, sample(unsort.cell.id[set][ptrans], 
                            size=pmin(track.afforest.oak, sum(ptrans, na.rm=T)), replace=F))
      
      ## Update the amount of shrub to be converted to oak
      track.afforest.oak = track.afforest.oak - sum(ptrans, na.rm=T)
    }

    ## New set of cells
    i = max(set)
  }
  
  ## Data frame with tracking values
  res = data.frame(potential.shrub = nrow(target.coord),
                   target.afforest.oak = target.afforest.oak*params$ha.cell,
                   target.afforest.pine = target.afforest.pine*params$ha.cell,
                   afforest.oak = length(afforest.oak.cells)*params$ha.cell,
                   afforest.pine = length(afforest.pine.cells)*params$ha.cell)
  
  gc(verbose = FALSE) 
  return(list(afforest.oak.cells=afforest.oak.cells, afforest.pine.cells=afforest.pine.cells, res=res))
}