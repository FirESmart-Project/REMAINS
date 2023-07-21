#' Simulation of prescribed burns
#'
#' @name prescribed.burn
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#' @param out.maps A flat to save maps as .tif files of wildfire identifier, wildfire step and fire risk
#' 
#' @return Returns a vector with cells' identifiers burnt, suppresed
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = wildfires(landscape, params, out.maps=F, verbose=F)
#' 

prescribed.burn = function(land, params, out.maps = FALSE, verbose = FALSE){
  
  ## Prescribed burns
  cat(" Prescribed burns \n")
  
  ## Initialize tracking variables
  fire.id = 0
  track.fires = NULL
  visit.cells = eval.cells = burnt.cells = integer()
  
  ## Sub-select the 'land' data frame to the only two land-cover types that are burnt with pb
  ## Moreover, only cells with tschg >= 4 are elegible or tschg>=3 ???
  subland = land %>% filter(lct %in% c("grass", "shrub")) %>% select(-trans.type) %>% 
    filter(tschg >= 3) %>% left_join(orography, by = "cell.id") 
  
  ## Sub-select non-protected areas if pb is excluded within protected areas
  if(params$pb.exclude.protected){
    subland = subland %>% filter(!protected)
  }
  
  ## Initialize a df such as subland to store the fire.id and the spread step of the burnt cells
  if(out.maps){
    map = data.frame(cell.id=subland$cell.id, id=NA, step=NA)
  }
   
  ## Set pb ignition probability as wildfires
  if(params$pb.strategy == "containment"){
    subland$pigni = fire.risk(subland, orography[orography$cell.id %in% subland$cell.id,], params$ha.cell)
  }
  if(params$pb.strategy == "random"){
    subland$pigni = runif(nrow(subland), 0 , 1)
  }
  if(params$pb.strategy == "roadnet"){
    subland$pigni = 0
    subland$pigni[!is.na(subland$roadbuff)] = runif(sum(!is.na(subland$roadbuff)), 0 , 1)
  }
  
  ## Build a data frame with the position and distance of the neighbors to a target cell
  ## By now, this data frame has the target cell (it will be removed later)
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
  
  ## Set the annual burnt area 
  annual.target.area = params$annual.area.pb
  
  ## Start burn until annual area target is not reached
  while(annual.target.area>0){
    
    ## ID for each fire event, and restart step
    fire.id = fire.id + 1
    step = 1
    
    ## Select an ignition point
    igni.id = sample(subland$cell.id, 1, replace=F, p=subland$pigni)
    region.igni = ifelse(subland$region[subland$cell.id == igni.id]=="PT", "Portugal", "Galicia")
    
    ## Determine a pb target size according to the land-cover type and the tschg
    if(is.null(params$min.pb.target.area)){
      if(subland$tschg[subland$cell.id == igni.id] == 3){
        fire.target.area = round(runif(1, 0.5, 2))  
      } else if(subland$tschg[subland$cell.id == igni.id] >= 4 & subland$tschg[subland$cell.id == igni.id] <= 5){
        fire.target.area = round(runif(1, 1, 3))
      } else{
        fire.target.area = round(runif(1, 2, 6))
      }  
    } else{
      if(is.null(params$max.pb.target.area)){
        fire.target.area = round(runif(1, params$min.pb.target.area, params$min.pb.target.area*4))
      } else{
        fire.target.area = round(runif(1, params$min.pb.target.area, params$max.pb.target.area))
      }
    }
    
    
    ## Bound fire.target.area to not exceed remaining annual.target.area
    if(fire.target.area > annual.target.area){
      fire.target.area = annual.target.area
    }
    
    ## Controls of the fire shape
    ## Max number of cells in the fire front
    mx.ncell.ff = ifelse(fire.target.area<=500, 12, ifelse(fire.target.area<=1500, 20, ifelse(fire.target.area<=5000, 30, 40)))
    ## Min number of cells in the fire front, no sé si per incendis de me´s de 5000 o me´s d 10000
    mn.ncell.ff = ifelse(fire.target.area<=5000, 8, 16)  # not sure if 16 for wind and 12 for convective
    # threshodl ratio.burnt to be aplied, no sé si per incendis de me´s de 5000 o me´s d 10000
    thruky = ifelse(fire.target.area<=5000, 0.85, 0.95)
    
    ## Initialize tracking variables
    ## Ignition always burnt, and it does in high intensity when no-PB
    fire.front = igni.id
    cumul.source = 1  
    aburnt = params$ha.cell
    visit.cells = c(visit.cells, igni.id) 
    eval.cells = c(eval.cells, igni.id)
    burnt.cells = c(burnt.cells, igni.id) 
    if(out.maps){
      map$id[map$cell.id==igni.id] = fire.id
      map$step[map$cell.id==igni.id] = step
    }
    
    if(verbose){
      cat(paste0("\n     PB ", fire.id,  " of target area ", fire.target.area, " ha"))
    }
    
    ## Start speading from active cells (i.e. the fire front)
    while((aburnt)<fire.target.area){
      
      if(verbose){
        cat(paste0("    Step ", step,  ", aburnt = ", aburnt, " ha"))
      }
      
      ## Increment step
      step = step + 1
      
      ## Build a data frame with the theoretical #default.nneigh# neighbors of all the cells in the fire.front, 
      ## and include the distance between the source and the neighbor.
      ## Filter by cells that have not been burnt yet.
      neigh.id = data.frame(cell.id=as.integer(rep(fire.front, each=default.nneigh)+rep(default.neigh$x, length(fire.front))),
                            source.id=rep(fire.front, each=default.nneigh),
                            dist=rep(default.neigh$dist,length(fire.front)),
                            position=rep(cumul.source, each=default.nneigh)) %>%
        filter(!(cell.id %in% eval.cells)) %>%              # keep source cell's elevation
        left_join(select(subland, cell.id, elevation), by=c("source.id"="cell.id")) %>% 
        mutate(elevation.source = elevation) %>% select(-elevation) 
      
      ## Compute the spread rate and the probability of burning by 
      ## - 
      neigh.land = subland[subland$cell.id %in% neigh.id$cell.id, c("cell.id", "lct", "tschg", "elevation")] %>%  
        left_join(params$lct.fire.prone, by="lct") %>% 
        left_join(neigh.id, by="cell.id") %>% 
        mutate(dif.elev = (elevation-elevation.source)/dist,
               slope = pmax(pmin(dif.elev,0.5),-0.5)+0.5,
               sr = params$wslope*slope + params$wlc*flam,
               sr = ifelse((lct=="shrub" & tschg<=4) | (lct %in% c("pine", "oak") & tschg <=8), 0.2, sr))   # Low fire spread rate for low-fuel land-covers
      neigh.land$pb = 1-exp(-params$facc*neigh.land$sr) + runif(nrow(neigh.land), -params$rpb, params$rpb)   
      neigh.land
      sprd.rate = group_by(neigh.land, cell.id) %>% 
        summarise(sr=max(sr), pb=max(pb), nsource=sum(position))
      sprd.rate$burn = sprd.rate$pb >= runif(nrow(sprd.rate), params$pb.lower.th, params$pb.upper.th)
      sprd.rate
      
      ## Mark with 'fire.id' the cells that have effectively burn and with 'step' all the evaluated cells 
      ## that time step, no matter if these have been effectively burnt or suppressed
      if(out.maps){
        map$id[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] = fire.id
        map$step[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] = step
      }
      
      ## Record the neighbors that have been visited (before breaking in case any cell verifies burn = T)
      visit.cells = c(visit.cells, sprd.rate$cell.id)
      eval.cells = c(eval.cells, sprd.rate$cell.id[sprd.rate$burn]) # effectively burnt or suppressed
      burnt.cells = c(burnt.cells, sprd.rate$cell.id[sprd.rate$burn]) # only burnt
      
      ## If at least there's a burnt or suppressed cell, continue, otherwise, stop
      ## If any cell has effectively burnt or suppressed in the current step, stop, because there is no
      ## cells from which to spread from.
      ## Otherwise, keep spreading even if everything is by suppression. that's what we want!
      if(!any(sprd.rate$burn))
        break
      
      ## Increase area burnt and suppressed
      aburnt = aburnt + sum(sprd.rate$burn)*params$ha.cell
      
      ## Select the new fire front
      nburn = sum(sprd.rate$burn)
      if(nburn<=mn.ncell.ff){
        fire.front = sprd.rate$cell.id[sprd.rate$burn]
      } else{
        ratio.burnt = (aburnt)/fire.target.area
        z = runif(1,mx.ncell.ff-5,mx.ncell.ff)
        ncell.ff = min(nburn*runif(1,0.5,0.7), round(z), na.rm=T)
        # si el nombre cell del ff coincideix amb el màxim  
        # o bé aleatòriament cap al final de l'incendi, forço compacitat.
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
      cumul.source = sprd.rate$nsource[sprd.rate$cell.id %in% fire.front]
      
      ## In the case, there are no cells in the fire front, stop trying to burn.
      ## This happens when no cells have burnt in the current spreading step
      if(length(fire.front)==0)
        break
      
    } # while 'fire.target.area'
    
    ## Write info about the current fire event
    if(is.null(track.fires)){
      track.fires = data.frame(fire.id, region=region.igni, atarget=fire.target.area, aburnt)  
    } else{
      track.fires = rbind(track.fires, data.frame(fire.id, region=region.igni, atarget=fire.target.area, aburnt))
    }
    
    ## Decrease the annual target area by the total area burnt and suppressed in the current fire
    annual.target.area = annual.target.area - (aburnt)
    if(verbose){
      cat(paste("     remaining:", annual.target.area, "ha", "\n"))
    }
    
  } # while 'annual.target.area'
  
  ## Count cells prescribed-burnt categorized by land cover type and time since change
  track.lc.pburnt = subland %>% filter(cell.id %in% burnt.cells) %>% group_by(region, lct, tschg) %>% 
    summarise(aburnt = length(cell.id)*params$ha.cell)
  track.lc.pburnt$region = ifelse(track.lc.pburnt$region=="PT", "Portugal", "Galicia")
  
  ## Return tracking data frames
  if(out.maps){
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.pburnt = track.lc.pburnt, map = map))
  } else{
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.pburnt = track.lc.pburnt))
  }
  
}
