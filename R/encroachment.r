#' Simulation of encroachment in sparse vegetation
#'
#' @name encroachment
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#'
#' @return Returns a list of one vector with cells' identifiers colonized by shrublands and a data frame 
#' with tracking data of the encroachment process 
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = encroachment(landscape, params)
#' 

encroachment = function(land, params){
  
  cat(" Encroachment\n")
  
  ## Count available rocky vegetation that can become shrub (n.cells)
  ## Add coordinates
  target.coord = land %>% filter(lct == "sparseveg", tschg > params$shruby.th) %>% 
    left_join(coords, by = "cell.id") %>% select(cell.id, x, y) 
  
  ## According to the annual rate, determine the target of transition (number of cells)
  target.encroach = round(nrow(target.coord)*params$rate.encroach/100) 	
  track.encroach = target.encroach
  
  ## Find neighbors in a squared neighborhood of a fixed radius
  cell.radius = round(params$radius.encroach/params$cell.size)
  
  ## Unsorted cell.id to potentially be changed from sparseveg to shrub
  unsort.cell.id = sample(target.coord$cell.id, size = nrow(target.coord), replace=F)
  
  ## Initialize vectors to save the cell.id of those cells that will change
  encroach.cells = numeric()
  
  ## Because of problems with allocation memory, split the task in sets of 4000 cells
  i=0
  while(track.encroach>0){
    
    ## Find neighbors in a squared neighborhood of a fixed radius for a set of 1000 cells
    set = (i+1):(i+4000)
    neigh.id = nn2(coords[,-1], target.coord[target.coord$cell.id %in% unsort.cell.id[set],-1],  
                   searchtype="priority", k=(cell.radius*2)^2)
    
    ## Mark the the presence of shrub not recently converted to it for all the neighbors
    land$is.target = land$lct=="shrub" & land$tschg>0
    
    ## Determine if transition happens according to the percentage of shrub
    ## in a circular neighborhood of fixed radius 
    ptrans = .neigh.transition(land, neigh.id, params$radius.encroach)
    
    ## cell.id for encroachment
    encroach.cells = c(encroach.cells, sample(unsort.cell.id[set][ptrans],
                      size=pmin(track.encroach, sum(ptrans, na.rm=T)), replace=F))
    
    ## Update the amount of sparseveg to be converted to shrub
    track.encroach = track.encroach - sum(ptrans, na.rm=T)
    
    ## New set of cells
    i = max(set)
  }
  
  ## Data frame with tracking values
  res = data.frame(potential.rocky = nrow(target.coord),
                   target.encroach = target.encroach*params$ha.cell,
                   encroach = length(encroach.cells)*params$ha.cell)
  gc(verbose = FALSE)
  return(list(encroach.cells=encroach.cells, res=res))
}