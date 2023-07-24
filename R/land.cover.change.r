#' Simulation of land-cover changes
#'
#' @name land.cover.change
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#' @param lc.trans Identifier of the land-cover transition: 1 - smart plantation, 2 - agriculture conversion, 
#' 3 - rural abandonment, and 4 - pasture abandonment.
#' @param trgt.dmnd Number of hectares to be converted to the target land-cover type
#' @param visit.cells Vector of cells' identifier that have already been subject of change for a land-cover change process
#'
#' @return Returns a vector with cells' identifiers to change the land-cover type
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = land.cover.change(landscape, params, 1, 100, NULL)
#' 

land.cover.change = function(land, params, lc.trans = 1, trgt.dmnd = 0, visit.cells = NULL){

  if(trgt.dmnd==0)
    return(numeric())
  
  cat(paste0(" Land-cover transition: ", 
             ifelse(lc.trans==1, "Smart plantation.", 
                ifelse(lc.trans==2, "Agriculture conversion.", 
                  ifelse(lc.trans==3, "Rural abandonment.",   # crop to shurb
                    ifelse(lc.trans==4, "Pasture abandonment.", # grass to shrub
                      ifelse(lc.trans==5, "Pasture conversion.", # sparseveg to grass
                                  "Undefined.")))))), "\n")
  
  
  ## Function to select items not in a vector
  `%notin%` = Negate(`%in%`)

  ## Join land-cover/spp info to coordinates to preselect coordinates of those cells that may
  ## under go change per each land-cover transition
  coord.land = left_join(coords, select(land, cell.id, lct), by="cell.id")
  
  ## Define per each land-cover transition, the transition potential layer (probability of ignition) 
  ## and the land-cover types that can potentially change to the target land-cover
  ## By now, the transition potential is 1 everywhere
  if(lc.trans==1){
    trans.pot = ifelse(params$mode.trans.potential$SmartPlant=="random", 1, NA)
    lc.source = (land$lct == "pine")
    coord.land = filter(coord.land, lct=="pine") %>% select(-lct)
  }
  if(lc.trans==2){
    x = (land$interface == "crp") | (land$interface == "urbcrp") | (land$interface == "crpshrb") | (land$interface == "crpfrst") 
    x = x * (orography$elevation >= params$elev.for.crop) * (orography$slope <= params$slope.for.crop) 
    trans.pot = ifelse(params$mode.trans.potential$AgriConver=="random", 1, 
                  ifelse(params$mode.trans.potential$AgriConver=="interface", x, NA))
    lc.source = (land$lct == "shrub")
    coord.land = filter(coord.land, lct=="shrub") %>% select(-lct)
  }
  if(lc.trans==3){
    x = (land$interface == "shrb") | (land$interface == "urbshrb") | (land$interface == "crpshrb") | (land$interface == "shrbfrst") 
    x = x * pmin(orography$elevation, 1000, na.rm=T)/1000
    trans.pot = ifelse(params$mode.trans.potential$RuralAbnd=="random", 1, 
                  ifelse(params$mode.trans.potential$RuralAbnd=="interface", x, NA))
    lc.source = (land$lct == "crop")
    coord.land = filter(coord.land, lct=="crop") %>% select(-lct)
  }
  if(lc.trans==4){
    x = (land$interface == "shrb") 
    trans.pot = ifelse(params$mode.trans.potential$PastureAbnd=="random", 1, 
                  ifelse(params$mode.trans.potential$PastureAbnd=="interface", x, NA))
    lc.source = (land$lct == "grass")
    coord.land = filter(coord.land, lct=="grass") %>% select(-lct)
  }
  if(lc.trans==5){
    x = (land$interface == "crp") | (land$interface == "urbcrp") | (land$interface == "crpshrb") | (land$interface == "crpfrst") 
    trans.pot = ifelse(params$mode.trans.potential$PastureConver=="random", 1, 
                       ifelse(params$mode.trans.potential$PastureConver=="interface", x, NA))
    lc.source = (land$lct == "sparseveg")
    coord.land = filter(coord.land, lct=="sparseveg") %>% select(-lct)
  }
  
  ## Idem for parameters driving spatial aggregation of change
  k = params$lc.alloc[1, lc.trans]
  ligni = params$lc.alloc[2, lc.trans]
  lsprd = params$lc.alloc[3, lc.trans]
  
  
  ## Choose according to trans.pot and lc.source as many cells as target demand to potentially start 
  ## clusters of change. For ignition points, wt is as wt.ini.
  ## Remove cells that have been already changed by other land-cover transitions
  chg = data.frame(cell.id=sample(land$cell.id, size=trgt.dmnd, p=trans.pot*lc.source, replace = F)) %>%
         mutate(wt.ini = rexp(trgt.dmnd, rate=ligni)) %>% mutate(wt=wt.ini) %>%
         filter(cell.id %notin% visit.cells)
  
  
  ## Select around 20% of cells to start a cluster (at least one cell) according to wt.ini
  if(nrow(chg)>1){
    front = sample(chg$cell.id, size=pmax(1,round(nrow(chg)*0.01)), p=1/(chg$wt.ini^2), replace=F)  
  } else{
    front = chg$cell.id
  }
  
  
  ## Make effective the change
  achg = length(front)
  chg.cells = front 
  

  ## Keep spreading the change until area changed is at least as target demand
  while(achg <= trgt.dmnd){
  
    ## Look for 8+1 neighbours of front cells
    neighs = nn2(coord.land[,-1], filter(coord.land, cell.id %in% front)[,-1], searchtype="priority", k=9)  
    
    ## Recuperate initial waiting times of the first source cells
    wt.inis = matrix(unlist(filter(chg, cell.id %in% front) %>% select(wt.ini)), 
                      nrow=length(front), ncol=8, byrow=F)
    
    ## Remove front cells from the 'chg' data.frame
    ## Then add new cells that (1) are less than 200m appart from the source cell, 
    ## (2) a waiting time = rexp(lsprd) * w.tini^k (if a cell is visited more than once, 
    ## keep the minimum waiting time), and (3) have not been changed by other land-cover transitions
    chg = rbind(filter(chg, cell.id %notin% front), 
                 data.frame(cell.id=coord.land$cell.id[neighs$nn.idx[,-1][neighs$nn.dists[,-1] <200]], 
                            wt.ini=wt.inis[neighs$nn.dists[,-1] <200]) %>% 
                      mutate(wt = rexp(sum(neighs$nn.dists[,-1] <200), rate=lsprd) * wt.ini^k ) %>%
                      filter(cell.id %notin% visit.cells)  ) %>%
           group_by(cell.id) %>% summarize(wt.ini=min(wt.ini), wt=min(wt) )
                      
    ## Select around 10% of cells to start new clusters or keep spreading from current ones according to wt
    if(nrow(chg)>1){
      front = sample(chg$cell.id, size=pmax(1,round(nrow(chg)*0.1)), p=1/(chg$wt^2), replace=F)
    } else{
      front = chg$cell.id
    }
    
    ## Make effective the change
    achg = achg + length(front)*params$ha.cell
    chg.cells = c(chg.cells, front)
    visit.cells = c(visit.cells, chg.cells)
    
    ## Check condition to break the while loop and avoid the nn2 Error 'no points in query'
    if(nrow(coord.land[coord.land$cell.id %in% front,])==0){
      break
    }
  }
  
  return(chg.cells)
}