#' Simulation of wildfires
#'
#' @name postfire.rege
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#' @param out.maps A flat to save maps as .tif files of wildfire identifier, wildfire step and fire risk
#' 
#' @return Returns a vector with cells' identifiers burnt, suppressed
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = wildfires(landscape, params, out.maps=F, verbose=F)
#' 

wildfires = function(land, params, out.maps = FALSE, verbose = FALSE){
  # wildfires = function(land, orography, coords, mask.study.area, burnt.area.dist.pt, fire.size.dist.pt, 
  #                      burnt.area.dist.gz, fire.size.dist.gz, params, out.maps = FALSE, verbose = FALSE){
     
  ## Initialize tracking variables
  fire.id = 0
  track.fires = NULL
  visit.cells = eval.cells = burnt.cells = integer()
  
  ## Sub-select the 'land' data frame to be sure that non-burnable covers do not burn
  subland = land %>% filter(lct!="water") %>% filter(lct!="urban") %>% filter(lct!="baresoil") %>% select(-trans.type)
  
  ## Add two variables to record whether the cell has been suppressed by any of the two strategies.
  ## This info will be used when the cells become fire.front
  subland$is.source.supp.fuel = subland$is.source.supp.mosaic = FALSE
  
  ## Add region to subland
  subland = subland %>% left_join(select(orography, cell.id, region, elevation), by="cell.id")
                                  
  ## Initialize a df such as subland to store the fire.id and the spread step of the 
  ## burnt and suppressed cells
  if(out.maps){
    map = data.frame(cell.id=subland$cell.id, id=NA, step=NA)
  }
  
  ## Set fire probability
  subland$pigni = fire.risk(subland, orography[orography$cell.id %in% subland$cell.id,], params$ha.cell)

  ## Build a data frame with the position and distance of the neighbors to a target cell
  ## By now, this data frame has the target cell
  ## Work with 8 neighbors 
  pos = c(-1,0,1)
  ncol = ncol(mask.study.area)
  d1 = sqrt(2*params$cell.size^2)
  default.neigh = data.frame(x=c(-ncol+pos, pos, ncol+pos),
                             dist=c(d1, params$cell.size, d1,
                                    params$cell.size, 0, params$cell.size,
                                    d1, params$cell.size, d1))
  
  ## Count the number of neighs 
  default.neigh = default.neigh[default.neigh$dist!=0,]
  default.nneigh = nrow(default.neigh)
  
  ## Wildfires in each region
  for(region in c("Portugal", "Galicia")){
    cat(paste0(" Wildfires in ", region, "\n"))
    
    ## Set the annual burnt area and fire size distributions according to the region
    if(region=="Portugal"){
      burnt.area.dist = burnt.area.dist.pt
      fire.size.dist = fire.size.dist.pt  
    }
    if(region=="Galicia"){
      burnt.area.dist = burnt.area.dist.gz
      fire.size.dist = fire.size.dist.gz
    }    
    
    ## Select an annual burnt area class according to frequencies
    ## Then pick a random annual target area within this class (in ha)
    ba.cls = sample(burnt.area.dist$upper.th, size = 1, p = burnt.area.dist$freq)
    ba.pos = which(burnt.area.dist$upper.th == ba.cls)
    annual.target.area = round(runif(1, burnt.area.dist$upper.th[ba.pos-1], burnt.area.dist$upper.th[ba.pos]))
    cat(paste("   Annual target area:", annual.target.area, "ha", "\n"))
    
    ## Assign the two fire suppression levels according to the region
    fuel.th = params$fire.suppression$fuel.th[params$fire.suppression$region == region]
    mosaic.th = params$fire.suppression$mosaic.th[params$fire.suppression$region == region]
    cat(paste0("   Fuel suppression th: ", fuel.th*100, "%  -  Mosaic suppression th: ", mosaic.th, " pixel \n"))
    
    ## Start burn (or suppress) until annual area target is not reached
    while(annual.target.area>0){
      
      ## ID for each fire event, and restart step
      fire.id = fire.id + 1
      step = 1
      
      ## Select an ignition point
      igni.id = sample(subland$cell.id, 1, replace=F, 
                      p=subland$pigni*(subland$region==ifelse(region=="Portugal", "PT", "GZ")))
      
      ## Select a fire size class according to frequencies
      ## Then pick a random annual target area within this class (in ha)
      fs.cls = sample(fire.size.dist$upper.th, size = 1, p = fire.size.dist$freq)
      fs.pos = which(fire.size.dist$upper.th == fs.cls)
      fire.target.area = round(runif(1, fire.size.dist$upper.th[fs.pos-1], fire.size.dist$upper.th[fs.pos]))
      
      ## Bound fire.target.area to not exceed remaining annual.target.area
      if(fire.target.area > annual.target.area){
        fire.target.area = annual.target.area
      }
      
      ## Controls of the fire shape
      ## Max number of cells in the fire front
      mx.ncell.ff = ifelse(fire.target.area<=500, 12, ifelse(fire.target.area<=1500, 20, ifelse(fire.target.area<=5000, 30, 40)))
      ## Min number of cells in the fire front
      mn.ncell.ff = ifelse(fire.target.area<=5000, 8, 16) 
      # threshold ratio.burnt to be applied
      thruky = ifelse(fire.target.area<=5000, 0.85, 0.95)
      
      ## Initialize tracking variables
      ## Ignition always burnt, and it does in high intensity when no-PB
      fire.front = igni.id
      cumul.source = 1  
      cumul.agri = 0
      aburnt = params$ha.cell
      asupp.fuel= asupp.mosaic = 0
      visit.cells = c(visit.cells, igni.id) 
      eval.cells = c(eval.cells, igni.id)
      burnt.cells = c(burnt.cells, igni.id) 
      if(out.maps){
        map$id[map$cell.id==igni.id] = fire.id
        map$step[map$cell.id==igni.id] = step
      }
      
      if(verbose){
        cat(paste0("\n     Fire ", fire.id,  " of target area ", fire.target.area, " ha"))
      }
      
      ## Start speading from active cells (i.e. the fire front)
      while((aburnt+asupp.fuel+asupp.mosaic)<fire.target.area){
        
        if(verbose){
          cat(paste0("    Step ", step,  ", aburnt+asupp = ", aburnt+asupp.fuel+asupp.mosaic, " ha"))
        }
        
        ## Increment step
        step = step + 1
        
        ## Build a data frame with the theoretical #default.nneigh# neighbors of all the cells in the fire.front, 
        ## and include the distance between the source and the neighbor.
        ## Filter by cells that have not been burnt yet.
        neigh.id = data.frame(cell.id=as.integer(rep(fire.front, each=default.nneigh)+rep(default.neigh$x, length(fire.front))),
                              source.id=rep(fire.front, each=default.nneigh),
                              dist=rep(default.neigh$dist,length(fire.front)),
                              position=rep(cumul.source, each=default.nneigh),
                              agri.front=rep(cumul.agri, each=default.nneigh)) %>%
          filter(!(cell.id %in% eval.cells)) %>% 
          left_join(select(subland, cell.id, lct, elevation, is.source.supp.fuel, is.source.supp.mosaic), by=c("source.id"="cell.id")) %>% 
          mutate(is.source.agri = (lct=="crop"), elevation.source = elevation) %>% select(-lct, -elevation) 

        ## no need to filter for those cells that are not of the study area        
        # neigh.id = rbind(neigh.id, data.frame(cell.id = 1, source.id = 2923415, dist=0, is.source.agri = F, elevation.source=622.7))
        neigh.id  
        
        ## Compute the spread rate and the probability of burning by 
        neigh.land = subland[subland$cell.id %in% neigh.id$cell.id, c("cell.id", "lct", "tschg", "elevation")] %>%  
          left_join(params$lct.fire.prone, by="lct") %>% 
          left_join(neigh.id, by="cell.id") %>% 
          mutate(dif.elev = (elevation-elevation.source)/dist,
                 slope = pmax(pmin(dif.elev,0.5),-0.5)+0.5,
                 agri.front = ifelse(lct=="crop" & is.source.agri, agri.front+1, 0),
                 sr = params$wslope*slope + params$wlc*flam,
                 sr = ifelse((lct=="shrub" & tschg<=4) | (lct %in% c("pine", "oak") & tschg <=8), 0.2, sr))   # Low fire spread rate for low-fuel land-covers
        neigh.land$pb = 1-exp(-params$facc*neigh.land$sr) + runif(nrow(neigh.land), -params$rpb, params$rpb)   
        neigh.land
        sprd.rate = group_by(neigh.land, cell.id) %>% 
          summarise(sr=max(sr), pb=max(pb), nsource=sum(position)) %>% 
          left_join(select(neigh.land, cell.id, pb, agri.front,
                           is.source.supp.fuel, is.source.supp.mosaic), by=c("cell.id", "pb")) %>% 
          mutate(tosupp.fuel=(is.source.supp.fuel | sr<=fuel.th), 
                 tosupp.mosaic=(is.source.supp.mosaic | agri.front>=mosaic.th))
        sprd.rate$burn = sprd.rate$pb >= runif(nrow(sprd.rate), params$pb.lower.th, params$pb.upper.th)
        sprd.rate
        
        ## Record in 'subland' the suppression state of future source cells
        ## Suppression by mosaic has a prevalence over suppression by low fuel load
        subland$is.source.supp.fuel[subland$cell.id %in% 
                sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.mosaic & sprd.rate$tosupp.fuel]] = TRUE
        subland$is.source.supp.mosaic[subland$cell.id %in% 
                sprd.rate$cell.id[sprd.rate$burn & sprd.rate$tosupp.mosaic]] = TRUE
        
        ## Mark with 'fire.id' the cells that have effectively burn and with 'step' all the evaluated cells 
        ## that time step, no matter if these have been effectively burnt or suppressed
        if(out.maps){
          map$id[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic]] = fire.id
          map$step[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] = step
        }
        
        ## Record the neighbors that have been visited (before breaking in case any cell verifies burn = T)
        visit.cells = c(visit.cells, sprd.rate$cell.id)
        eval.cells = c(eval.cells, sprd.rate$cell.id[sprd.rate$burn]) # effectively burnt or suppressed
        burnt.cells = c(burnt.cells, sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic]) # only burnt
        
        ## If at least there's a burnt or suppressed cell, continue, otherwise, stop
        ## If any cell has effectively burnt or suppressed in the current step, stop, because there is no
        ## cells from which to spread from.
        ## Otherwise, keep spreading even if everything is by suppression. that's what we want!
        if(!any(sprd.rate$burn))
          break

        ## Increase area burnt and suppressed
        aburnt = aburnt + sum(sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic)*params$ha.cell
        asupp.fuel = asupp.fuel + sum(sprd.rate$burn & !sprd.rate$tosupp.mosaic & sprd.rate$tosupp.fuel)*params$ha.cell
        asupp.mosaic = asupp.mosaic + sum(sprd.rate$burn & sprd.rate$tosupp.mosaic)*params$ha.cell
            
        ## Select the new fire front
        nburn = sum(sprd.rate$burn)

        if(nburn<=mn.ncell.ff){
          fire.front = sprd.rate$cell.id[sprd.rate$burn]
        } else{
          ratio.burnt = (aburnt+asupp.fuel+asupp.mosaic)/fire.target.area
          z = runif(1,mx.ncell.ff-5,mx.ncell.ff)
          ncell.ff = min(nburn*runif(1,0.5,0.7), round(z), na.rm=T)
          if(ncell.ff==z | (ratio.burnt>=thruky & runif(1,0,1)>=0.75)){
            p = sprd.rate$nsource[sprd.rate$burn]/100
            if(class(try(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F, prob=p), silent=T))=="try-error"){
              fire.front = sort(sprd.rate$cell.id[sprd.rate$burn]) ## all burnt in fire.front
            } else{
              fire.front = sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F))  
            }
          } else{
            if(class(try(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F), silent=T))=="try-error"){
              fire.front = sort(sprd.rate$cell.id[sprd.rate$burn]) ## all burnt in fire.front
            } else{
              fire.front = sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F))
            }
          }
        }
        
        ## Record the cumulative number of cell sources (that is, the relative position of the 
        ## new cells in the spreading process, this is needed for choosing the amount of cells 
        ## forming the fire.front in the different phases - beginning vs. ending - of the fire spreading)
        ## Also record, the cumulative number of agricultural cells burnt
        cumul.source = sprd.rate$nsource[sprd.rate$cell.id %in% fire.front]
        cumul.agri = sprd.rate$agri.front[sprd.rate$cell.id %in% fire.front]
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
      } # while 'fire.target.area'
      
      ## Write info about the current fire event
      if(is.null(track.fires)){
        track.fires = data.frame(region, fire.id, atarget=aburnt+asupp.fuel+asupp.mosaic, 
                                 aburnt, asupp.fuel, asupp.mosaic)  
      } else{
        track.fires = rbind(track.fires, data.frame(region, fire.id, 
                            atarget=aburnt+asupp.fuel+asupp.mosaic, aburnt, asupp.fuel, asupp.mosaic))
      }
      
      ## Decrease the annual target area by the total area burnt and suppressed in the current fire
      annual.target.area = annual.target.area - (aburnt + asupp.fuel + asupp.mosaic)
      if(verbose){
        cat(paste("     remaining:", annual.target.area, "ha", "\n"))
      }
      
    } # while 'annual.target.area'
    
  }  # for 'region'
  

  ## Remanent area of all fires
  track.fires$rem = pmax(0, track.fires$atarget-track.fires$aburnt-track.fires$asupp.fuel-track.fires$asupp.mosaic)
  
  ## Count land cover types burnt and suppressed by region
  lc.burnt = subland %>% filter(cell.id %in% burnt.cells) %>% group_by(region, lct) %>% 
    summarise(burnt = length(cell.id)*params$ha.cell)
  track.lc.burnt.supp = subland %>% filter(cell.id %in% eval.cells) %>% group_by(region, lct) %>% 
    summarise(target = length(cell.id)*params$ha.cell) %>% 
    left_join(lc.burnt, by=c("region", "lct")) %>% mutate(supp = ifelse(is.na(target-burnt), target, target-burnt))
  track.lc.burnt.supp[is.na(track.lc.burnt.supp)] = 0
  track.lc.burnt.supp$region = ifelse(track.lc.burnt.supp$region=="PT", "Portugal", "Galicia")
  
  ## Count burnt pines and oaks by region and age
  track.forest.age.burnt = subland %>% filter(cell.id %in% burnt.cells, lct %in% c("pine", "oak")) %>% 
    group_by(region, lct, tschg) %>% summarise(area = length(cell.id)*params$ha.cell)
  
  ## Count suppressed pines and oaks by region and age
  track.forest.age.supp = subland %>% filter(cell.id %in% eval.cells, lct %in% c("pine", "oak")) %>% 
    group_by(region, lct, tschg) %>% summarise(target = length(cell.id)*params$ha.cell) %>% 
    left_join(track.forest.age.burnt, by=c("region", "lct", "tschg")) %>% 
    mutate(area = ifelse(is.na(target-area), target, target-area)) %>% select(-target)
  
  ## Replace the code of the region by its name
  track.forest.age.burnt$region = ifelse(track.forest.age.burnt$region=="PT", "Portugal", "Galicia")
  track.forest.age.supp$region = ifelse(track.forest.age.supp$region=="PT", "Portugal", "Galicia")
  
  if(out.maps){
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.burnt.supp = track.lc.burnt.supp, 
                track.forest.age.burnt = track.forest.age.burnt, track.forest.age.supp = track.forest.age.supp, map = map))
  } else{
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.burnt.supp = track.lc.burnt.supp,
                track.forest.age.burnt = track.forest.age.burnt, track.forest.age.supp = track.forest.age.supp))
  }
  
}
