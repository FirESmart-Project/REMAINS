#' Simulation of encroachment in sparse vegetation
#'
#' @name foerst.recover
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#' @param shrub2pine Vector to accumulate the number of shrub.to.pine cells that have to transform to pine each time step
#' @param shrub2oak Vector to accumulate the number of shrub.to.oak cells that have to transform to oak each time step
#' @param t Value of the current time step 
#'
#' @return Returns a list with (1) a vector of cells' identifiers that have transformed to pine, (2) a vector of cell's 
#' identifiers that have transformed to oak, (3) a vector with the number of cells to be transformed to pine each time step, 
#' (4) a vector with the number of cells to be transformed to oak each time step, and (5) a data frame with tracking data 
#' of the forest recover process 
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' shrub2pine = shrub2oak = numeric(length=params$time.horizon+8)
#' res = forest.recover(landscape, params, shrub2pine, shrub2oak, 1)
#' 
 
forest.recover = function(land, params, shrub2pine, shrub2oak, t = 1){
  
  ## Tracking the forest recover (pine and oak) after fire 
  cat(" Forest recover", "\n") 
  ## Count the ShrubToPine and ShrubToOak of the current year
  current.shrub.to.pine = land %>% filter(tschg==0 & lct == "shrub.to.pine") %>% 
    group_by(lct) %>% summarise(ncell=length(lct))
  current.shrub.to.pine = current.shrub.to.pine$ncell
  if(length(current.shrub.to.pine)==0){
    current.shrub.to.pine = 0
  }
  current.shrub.to.oak = land %>% filter(tschg==0 & lct == "shrub.to.oak") %>% 
    group_by(lct) %>% summarise(ncell=length(lct))
  current.shrub.to.oak = current.shrub.to.oak$ncell
  if(length(current.shrub.to.oak)==0){
    current.shrub.to.oak = 0
  }
  
  ## Split the amount of ShrubToPine and ShrubToOak of the current year in the next 8 years
  ## Thus, at time t=1 no shrub.to.pine or shrub.to.oak (that have just been stablish) will be
  ## converted to pine or oak, respectively
  shrub2pine[(t+1):(t+7)] = shrub2pine[(t+1):(t+7)] + round(current.shrub.to.pine/8)
  shrub2pine[t+8] = shrub2pine[t+8] + current.shrub.to.pine - 7*round(current.shrub.to.pine/8)
  shrub2oak[(t+1):(t+7)] = shrub2oak[(t+1):(t+7)] + round(current.shrub.to.oak/8)
  shrub2oak[t+8] = shrub2oak[t+8] + current.shrub.to.oak - 7*round(current.shrub.to.oak/8)
  
  ## Initialize vectors to save the cell.id of those cells that will change
  pine.recover.cells = oak.recover.cells = numeric()
  
  ## If there are cells to be converted to pine ...
  if(shrub2pine[t]>0){
    
    ## Count available shrublands that can become pine (n.cells)
    target.coord = land %>% filter(lct == "shrub.to.pine" & tschg>0) %>% 
      left_join(coords, by = "cell.id") %>% select(cell.id, x, y)

    ## Find neighbors in a squared neighborhood of a fixed radius
    cell.radius = round(params$radius.pine.recover/params$cell.size)
    
    ## Unsorted cell.id to potentially be changed from shrub.to.pine to pine
    unsort.cell.id = target.coord$cell.id[sample(nrow(target.coord), size = nrow(target.coord), replace=F)]  
    
    ## Because of problems with allocation memory, split the task in sets of 4000 cells
    i=0
    ptrans = numeric()
    while(i<length(unsort.cell.id)){
      
      ## Find neighbors in a squared neighborhood of a fixed radius for a set of 4000 cells
      set = (i+1):(i+4000)
      neigh.id = nn2(coords[,-1], target.coord[target.coord$cell.id %in% unsort.cell.id[set],-1],  
                     searchtype="priority", k=(cell.radius*2)^2) 
      
      ## Mark the the presence of pine for all the neighbors
      land$is.target = (land$lct=="pine") 
      
      ## Determine if transition happens according to the percentage of pines
      ## in a circular neighborhood of fixed radius 
      ptrans = c(ptrans, .neigh.transition(land, neigh.id, params$radius.pine.recover, numeric=T))

      ## New set of cells
      i = max(set)
    }
    
    ## Sample those potential shrub.to.pine cells that can change to pine
    pine.recover.cells = sample(target.coord$cell.id, size=pmin(shrub2pine[t], nrow(target.coord)), p=ptrans+10^-6, replace=F)
  }
  
  
  ## If there are cells to be converted to oak ...
  if(shrub2oak[t]>0){
    
    ## Count available shrublands that can become oak (n.cells)
    target.coord = land %>% filter(lct == "shrub.to.oak" & tschg>0) %>% 
      left_join(coords, by = "cell.id") %>% select(cell.id, x, y)

    ## Find neighbors in a squared neighborhood of a fixed radius
    cell.radius = round(params$radius.oak.recover/params$cell.size)
    
    ## Unsorted cell.id to potentially be changed from shrub.to.oak to oak
    unsort.cell.id = target.coord$cell.id[sample(nrow(target.coord), size = nrow(target.coord), replace=F)]  
      
    ## Because of problems with allocation memory, split the task in sets of 4000 cells
    i=0
    ptrans = numeric()
    while(i<length(unsort.cell.id)){
      
      ## Find neighbors in a squared neighborhood of a fixed radius for a set of 4000 cells
      set = (i+1):(i+4000)
      neigh.id = nn2(coords[,-1], target.coord[target.coord$cell.id %in% unsort.cell.id[set],-1],  
                     searchtype="priority", k=(cell.radius*2)^2) 
      
      ## Mark the the presence of oak for all the neighbors
      land$is.target = land$lct=="oak" 
      
      ## Determine if transition happens according to the percentage of oaks
      ## in a circular neighborhood of fixed radius 
      ptrans = c(ptrans, .neigh.transition(land, neigh.id, params$radius.oak.recover, numeric=T))
      
      ## New set of cells
      i = max(set)
    }
    
    ## Sample those potential shrub.to.oak cells that can change to oak
    oak.recover.cells =  sample(target.coord$cell.id, size=pmin(shrub2oak[t], nrow(target.coord)), p=ptrans+10^-6, replace=F)
  } 
 
  gc(verbose = FALSE) 
  return(list(pine.recover.cells = pine.recover.cells, oak.recover.cells = oak.recover.cells,
              shrub2pine = shrub2pine, shrub2oak = shrub2oak,
              res= data.frame(current.shrub.to.pine = current.shrub.to.pine*params$ha.cell, 
                              current.shrub.to.oak = current.shrub.to.oak*params$ha.cell,
                              convert.to.pine = shrub2pine[t]*params$ha.cell, 
                              convert.to.oak = shrub2oak[t]*params$ha.cell)))
}