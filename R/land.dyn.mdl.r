#' REMAINS model
#'
#' Run the REMAINS model that includes the processes of land-cover changes, wildfires, prescribed fires, 
#' and vegetation dynamics in mountain landscapes.
#'
#' @name land.dyn.mdl
#'
#' @param scenDir String with the name of the scenario that is the directory path to save the model's outputs
#' @param is.land.cover.change A flag to indicate that land cover changes are simulated
#' @param is.wildfire A flag to indicate that wildfires are simulated
#' @param is.prescribed.burn A flag to indicate that prescribed burns are simulated
#' @param is.postfire.rege A flag to indicate that post-fire regeneration is simulated
#' @param is.forest.recover A flag to indicate that forest recover after fire is simulated
#' @param is.afforestation A flag to indicate that afforestation is simulated
#' @param is.encroachment A flag to indicate that encroachment is simulated
#' @param nrun Number of replicates to run the model
#' @param save.land A flag to save as a RDS file the \code{landscape} data frame at the time step indicated in \code{out.seq}
#' @param params A list of the default model's parameters
#' @param lcc.demand A data frame with the area to be changed by each land-cover transition per time step
#' @param out.maps A flat to save maps as .tif files of wildfire identifier, wildfire step and fire risk
#' @param write.outputs A flag to write the results of the landscape processes as text files
#' @param verbose A flag to prompt extra information 
#' 
#' @return A list with the following 18 items:
#'  \itemize{
#'    \item{\code{Land}: A data frame with the land-cover types areas, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{lcc}: A data frame of land-cover transformations or conversions 
#'    (included if \code{is.land.cover.change}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{trans.type}: Transformation type: Rural abandonment, Pasture abandonment, Agricultural conversion, Pasture conversion and/or Smart plantation.}
#'         \item{\code{region}: Region: Portugal - PT and Spain - GZ.}
#'         \item{\code{area}: Area (in ha).}
#'         \item{\code{trgt.dmnd}: Target demand (in ha).}
#'       }
#'    }
#'    \item{\code{Fires}: A data frame of target, burnt and suppressed area per wildfire
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{region}: Event locations: Portugal and Spain.}
#'         \item{\code{fire.id}: Fire event identificator.}
#'         \item{\code{atarget}: Target area to be burnt (in ha).}
#'         \item{\code{aburnt}: Area burnt (in ha).}
#'         \item{\code{asupp.fuel}: Area suppressed in low fire spread conditions (in ha).} 
#'         \item{\code{asupp.mosaic}: Area suppressed due to the suppression opportunities created by croplands (in ha).} 
#'         \item{\code{rem}: Remaining area, not burnt, neither suppressed (in ha).}    
#'       }
#'    }
#'    \item{\code{lc.burnt.supp}: A data frame of burned and suppressed land-cover types 
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{region}: Region: Portugal and Spain.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{target}: Target area to be burnt (in ha).} 
#'         \item{\code{burnt}: Area effectively burnt (in ha).}
#'         \item{\code{supp}: Area effectively suppressed (in ha).}
#'       }
#'    }   
#'     \item{\code{lc.burnt}: A data frame of burned land-cover types 
#'    (included if \code{is.wildfires}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{region}: Region: Portugal and Spain.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{target}: Target area to be burnt (in ha).} 
#'         \item{\code{burnt}: Area effectively burnt (in ha).}
#'         \item{\code{supp}: Area effectively suppressed (in ha).}
#'       }
#'    }
#'    \item{\code{unburnt.land}: A data frame of unburned area by land-cover type
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{Forest.age.burnt}: A data frame with the count burned pines and oaks by region and age
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{region}: Region: Portugal and Spain.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{Forest.age.supp}: A data frame with the count suppressed pines and oaks by region and age
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{region}: Region: Portugal and Spain.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{unburnt.aok.age}: A data frame with unburnt oak forest per age
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
##'       }
#'    }
#'    \item{\code{unburnt.pine.age}: A data frame with unburnt pine forest per age 
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{pb}: A data frame with the area burned by prescribed fire
#'    (included if \code{is.prescribed.burn}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{fire.id}: Fire event identificator.}
#'         \item{\code{atarget}: Target area to be burnt (in ha).}
#'         \item{\code{aburnt}: Area effectively burnt by prescribed fires(in ha).}
#'       }
#'    }
#'    \item{\code{lc.pburnt}: A data frame with prescribed fire areas by land-cover types 
#'    (included if \code{is.prescribed.burn}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{lct}: Land-cover type burned by prescribed fire.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{aburnt}: Area effectively burnt (in ha).}
#'       }
#'    }
#'    \item{\code{postfire.rege}: A data frame of post-fire regeneration by land-cover type
#'    (included if \code{is.postfire.rege}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{lct}: Land-cover type.}
#'         \item{\code{post.lct}: Land-cover type post-fire.}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{Forest.recover}: A data frame of forest recover
#'    (included if \code{is.forest.recover}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{current.shrub.to.pine}: XXX.}
#'         \item{\code{current.shrub.to.oak}: XXX.}
#'         \item{\code{convert.to.pine}: XXX.}
#'         \item{\code{convert.to.oak}: XXX.}
#'       }
#'    }
#'     \item{\code{Afforest}: A data frame with the colonization of shrublands by oak and/or pines species
#'    (included if \code{is.afforestation}), with columns:
#'      \itemize{
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{potential.shrub}: Area (in ha).}
#'         \item{\code{target.afforest.oak}: Area (in ha).}
#'         \item{\code{target.afforest.pine}: Area (in ha).}
#'         \item{\code{afforest.oak}: Area (in ha).}
#'         \item{\code{afforest.oak}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{oak.age}: A data frame of xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#'    (included if \code{is.afforestation}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'    \item{\code{pine.age}: A data frame of xxxxxxxxxxxxxxxxxxxxxxxxx
#'    (included if \code{is.afforestation}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{tschg}: Time since the last land-cover transition (in years).}
#'         \item{\code{area}: Area (in ha).}
#'       }
#'    }
#'     \item{\code{Encroach}: A data frame of new shrubland area following encroachment
#'    (included if \code{is.encroachment}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{potential.rocky}: Area (in ha).}
#'         \item{\code{target.encroach}: Area (in ha).}
#'         \item{\code{encroach}: Area (in ha).}
#'       }
#'     }
#'  }
#'  
#' @export
#' 
#' @examples
#'
#' \dontrun{
#' library(REMAINS) ############# right??????
#' # Run one single 2020 replicate with forest management ########### 2020 right???
#' result = land.dyn.mdl(is.harvest = T)
#' }
#'

land.dyn.mdl = function(scenDir, is.land.cover.change = TRUE, is.wildfire = TRUE,
                        is.prescribed.burn = TRUE, is.postfire.rege = TRUE, is.forest.recover = TRUE,
                        is.afforestation = TRUE, is.encroachment = TRUE, nrun = 1, params = NULL, 
                        lcc.demand = NULL, save.land = FALSE, out.maps = FALSE, write.outputs = FALSE, 
                        verbose = FALSE){

  
  options(dplyr.summarise.inform=F)
  
  cat(paste0("*************** Run scenario: ", scenDir," ************ \n"))
  cat("A. Data preparation ...\n")
  
  ## Retrieve default parameters
  if(is.null(params)){
    params = default.params()
  }
  
  ## Load default land cover change demand
  if(is.null(lcc.demand)){
    load("data/lcc.demand.rda")  
  }
  
  ## Create output folder  to write model's outputs and log files
  if(write.outputs) dir.create(file.path(scenDir), showWarnings = F) 
  if(save.land) dir.create(file.path(paste0(scenDir, "/landscape")), showWarnings = F) 
  if(out.maps){
    dir.create(file.path(paste0(scenDir, "/maps")), showWarnings = F) 
    land.cover.type = data.frame(lct.id = 1:10, lct = c("crop", "pine", "oak", "sparseveg", "shrub",
      "water", "grass", "urban", "shrub.to.pine", "shrub.to.oak")) 
  }
    
  ## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
  ## 1. Climate change, 2. Land-cover changes, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  time.seq = seq(1, params$time.horizon, params$time.step)
  lchg.schedule = seq(1, params$time.horizon, params$time.step)
  fire.schedule = seq(1, params$time.horizon, params$time.step)
  pb.schedule = seq(1, params$time.horizon, params$time.step)
  post.fire.schedule = seq(1, params$time.horizon, params$time.step)
  forest.recover.schedule = seq(1, params$time.horizon, params$time.step)
  afforest.schedule = seq(1, params$time.horizon, params$time.step)
  encroach.schedule = seq(1, params$time.horizon, params$time.step)
  growth.schedule = seq(1, params$time.horizon, params$time.step)
  
  ## Initialize tracking data.frames
  track.land = NULL
  track.oak.age = NULL
  track.pine.age = NULL
  track.lcc = NULL
  track.afforest = NULL
  track.encroach = NULL
  track.unburnt.land = NULL
  track.unburnt.oak.age = NULL
  track.unburnt.pine.age = NULL
  track.postfire.rege = NULL
  track.forest.recover = NULL
  track.fires = NULL
  track.lc.burnt.supp = NULL
  track.forest.age.burnt = NULL
  track.forest.age.supp = NULL
  track.pb = NULL
  track.lc.pburnt = NULL
  
  ## Assign initial interface value to cells
  landscape$interface = interface(landscape, params)
  
  ## Assign default prob.fire = 1 to all cells 
  landscape$prob.fire = 1 
  
  ## Compute the buffer around road network if the pb strategy is roadnet
  if(params$pb.strategy == "roadnet"){
    if(is.null(params$buffroad.file)){
      cat(paste0("Build buffer of ", params$buffer.roadnet, "m around the road network\n"))
      road.raster = mask.study.area
      dta = data.frame(cell.id = 1:ncell(road.raster)) %>% left_join(orography, by="cell.id")
      road.raster[] = dta$road  # NA where there is no Roads !!!!!!
      if(params$buffer.roadnet==0)
        stop("Width of the buffer around the road network needs to be positive")
      buff = buffer(road.raster, width=params$buffer.roadnet)  # 1 within the buffer, NA otherwise
      dta$roadbuff = buff[]
      dta = dta[!is.na(dta$region), c("cell.id", "roadbuff")]
    }
    else{
      load(params$buffroad.file)
    }
    orography = orography %>% left_join(dta, by="cell.id")
  }
  
  ## Start the simulations
  cat("\nB. Simulations ...\n")
  irun = 1
  for(irun in 1:nrun){
    
    ## Main landscape data frame, initialize variables 'transitions type' and 'interface'
    land = landscape
    land$trans.type = NA  
    
    ## Vectors to accumulate the number of shrub.to.pine and shrub.to.oak cells that 
    ## have to become pine and oak each time step, respectively.
    shrub2pine = shrub2oak = numeric(length=params$time.horizon+8)
    
    ## Land at time 0, at the initial stage
    aux = group_by(land, lct) %>% summarise(area = length(lct) * params$ha.cell) %>% 
      add_column(run = irun, .before = "lct") %>% add_column(year = 0, .before = "lct")
    if(is.null(track.land)){
      track.land = aux
    } else{
      track.land = rbind(track.land, aux)
    }
    ## Oak distribution per each age class at time 0
    aux = land %>% filter(lct=="oak") %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
      add_column(run = irun, .before = "tschg") %>% add_column(year = 0, .before = "tschg")
    if(is.null(track.oak.age)){
      track.oak.age = aux
    } else{
      track.oak.age = rbind(track.oak.age, aux)
    }
    ## Pine distribution per each age class at time 0
    aux = land %>% filter(lct=="pine") %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
      add_column(run = irun, .before = "tschg") %>% add_column(year = 0, .before = "tschg")
    if(is.null(track.pine.age)){
      track.pine.age = aux
    } else{
      track.pine.age = rbind(track.pine.age, aux)
    }
    
    ## Simulation of one time step
    t = 1
    for(t in time.seq){
      
      ## Print replicate and time step
      cat(paste0("Replicate ", irun, "/", nrun, " - time: ", t, "/", params$time.horizon), "\n")
      
      ## 1. LAND-COVER CHANGES
      if(is.land.cover.change & t %in% lchg.schedule){
        # Smart plantations
        chg.cells = land.cover.change(land, params, lc.trans = 1, trgt.dmnd = lcc.demand$SmartPlant[t], 
                                      visit.cells = numeric())
        land$lct[land$cell.id %in% chg.cells] = "oak"
        land$tschg[land$cell.id %in% chg.cells] = 0
        land$trans.type[land$cell.id %in% chg.cells] = "SmartPlant"
        # Agriculture conversion
        visit.cells = chg.cells
        chg.cells = land.cover.change(land, params, lc.trans = 2, trgt.dmnd = lcc.demand$AgriConver[t], 
                                      visit.cells = visit.cells)
        land$lct[land$cell.id %in% chg.cells] = "crop"
        land$tschg[land$cell.id %in% chg.cells] = 0
        land$trans.type[land$cell.id %in% chg.cells] = "AgriConver"
        # Rural abandonment
        visit.cells = c(visit.cells, chg.cells)
        chg.cells = land.cover.change(land, params, lc.trans = 3, trgt.dmnd = lcc.demand$RuralAbnd[t], 
                                      visit.cells = visit.cells)
        land$lct[land$cell.id %in% chg.cells] = "shrub"
        land$tschg[land$cell.id %in% chg.cells] = 0
        land$trans.type[land$cell.id %in% chg.cells] = "RuralAbnd"
        # Pasture abandonment
        visit.cells = c(visit.cells, chg.cells)
        chg.cells = land.cover.change(land, params, lc.trans = 4, trgt.dmnd = lcc.demand$PastureAbnd[t], 
                                      visit.cells = visit.cells)
        if(length(chg.cells)==1){
          land$lct[land$cell.id %in% chg.cells] = "sparseveg"
        } else{
          xx = sample(chg.cells, size=round(length(chg.cells)*params$ratio.pasture.abnd), replace=F)
          yy = chg.cells[!(chg.cells %in% xx)]
          land$lct[land$cell.id %in% xx] = "sparseveg"
          land$lct[land$cell.id %in% yy] = "shrub"
          land$trans.type[land$cell.id %in% xx] = "PastureAbnd_sparseveg"
          land$trans.type[land$cell.id %in% yy] = "PastureAbnd_shrub"
        }
        land$tschg[land$cell.id %in% chg.cells] = 0
        # Pasture conversion
        visit.cells = c(visit.cells, chg.cells)
        chg.cells = land.cover.change(land, orography, lc.trans = 5, 
                                      trgt.dmnd = lcc.demand$PastureConver[t], visit.cells = visit.cells)
        land$lct[land$cell.id %in% chg.cells] = "grass"
        land$tschg[land$cell.id %in% chg.cells] = 0
        land$trans.type[land$cell.id %in% chg.cells] = "PastureConver"
        
        # Update interface values
        land$interface = interface(select(land, cell.id, lct), params)
        # Track land-cover changes
        aux = land %>% filter(tschg == 0 & trans.type %in% c("SmartPlant", "AgriConver", "RuralAbnd", 
                                                             "PastureAbnd_sparseveg", "PastureAbnd_shrub", "PastureConver")) %>% 
          left_join(select(orography, cell.id, region), by="cell.id") %>% 
          group_by(trans.type, region) %>% summarise(area = length(cell.id)*params$ha.cell) %>% 
          add_column(run = irun, .before = "trans.type") %>% add_column(year = t, .before = "trans.type") 
        if(is.null(track.lcc)){
          track.lcc = aux
        } else{
          track.lcc = rbind(track.lcc, aux)
        }
        gc(verbose = FALSE)
      }
    
      ## 2. WILDFIRES
      if(is.wildfire & t %in% fire.schedule){
        fires = wildfires(land, params, out.maps, verbose=F)
        if(length(fires$burnt.cells)>0){
          land$tschg[land$cell.id %in% fires$burnt.cells] = 0
          land$trans.type[land$cell.id %in% fires$burnt.cells] = "Fire"
        }
        # Track fires
        if(is.null(track.fires)){
          track.fires = data.frame(run = irun, year = t, fires$track.fires)
        } else{
          track.fires = rbind(track.fires, data.frame(run = irun, year = t, fires$track.fires))
        }
        if(is.null(track.lc.burnt.supp)){
          track.lc.burnt.supp = data.frame(run = irun, year = t, fires$track.lc.burnt.supp)
        } else{
          track.lc.burnt.supp = rbind(track.lc.burnt.supp, data.frame(run = irun, year = t, fires$track.lc.burnt.supp))
        }
        if(is.null(track.forest.age.burnt)){
          track.forest.age.burnt = data.frame(run = irun, year = t, fires$track.forest.age.burnt)
        } else{
          track.forest.age.burnt = rbind(track.forest.age.burnt, data.frame(run = irun, year = t, fires$track.forest.age.burnt))
        }
        if(is.null(track.forest.age.supp)){
          track.forest.age.supp = data.frame(run = irun, year = t, fires$track.forest.age.supp)
        } else{
          track.forest.age.supp = rbind(track.forest.age.supp, data.frame(run = irun, year = t, fires$track.forest.age.supp))
        }
        # Write raster files with wildfires_IDs, wildfires_step and fire risk
        if(out.maps & t %in% params$tseq.out.maps){
          out.raster = mask.study.area
          dta = data.frame(cell.id = 1:ncell(out.raster)) %>% left_join(fires$map, by="cell.id")
          dta$fire.risk[dta$cell.id %in% land$cell.id] = fire.risk(land, orography, params$ha.cell)
          out.raster[] = dta$id
          writeRaster(out.raster, filename = paste0(scenDir, "/maps/wildfire.ids_r", irun, "t", t, ".tif"), overwrite = T, format = "GTiff")
          out.raster[] = dta$step
          writeRaster(out.raster, filename = paste0(scenDir, "/maps/wildfire.step_r", irun, "t", t, ".tif"), overwrite = T, format = "GTiff")
          out.raster[] = dta$fire.risk
          writeRaster(out.raster, filename = paste0(scenDir, "/maps/firerisk_r", irun, "t", t, ".tif"), overwrite = T, format = "GTiff")
        }
      }
      
      ## 3. PRESCRIBED BURNS
      id.fire = 0
      if(is.prescribed.burn & t %in% pb.schedule){
        pb = prescribed.burn(land, params, out.maps, verbose=F)
        if(length(pb$burnt.cells)>0){
          land$tschg[land$cell.id %in% pb$burnt.cells] = 0
          land$trans.type[land$cell.id %in% pb$burnt.cells] = "PBurn"
          # Track prescribed burns
          if(is.null(track.pb)){
            track.pb = data.frame(run = irun, year = t, pb$track.fires)
          } else{
            track.pb = rbind(track.pb, data.frame(run = irun, year = t, pb$track.fires))
          }
          # And land-covers burnt in pb
          if(is.null(track.lc.pburnt)){
            track.lc.pburnt = data.frame(run = irun, year = t, pb$track.lc.pburnt)
          } else{
            track.lc.pburnt = rbind(track.lc.pburnt, data.frame(run = irun, year = t, pb$track.lc.pburnt))
          }
          # Write raster files with wildfires_IDs, wildfires_step and fire risk
          if(out.maps & t %in% params$tseq.out.maps){
            out.raster = mask.study.area
            dta = data.frame(cell.id = 1:ncell(out.raster)) %>% left_join(pb$map, by="cell.id")
            out.raster[] = dta$id
            writeRaster(out.raster, filename = paste0(scenDir, "/maps/pburns_r", irun, "t", t, ".tif"), overwrite = T, format = "GTiff")
          }
        }
      }
      
      ## 4. POST-FIRE REGENERATION
      if(is.postfire.rege & t %in% post.fire.schedule & exists("fires")){
        if(length(fires$burnt.cells)>0){
          rege  = postfire.rege(land, params)
          if(!is.null(rege)){
            land$lct[land$cell.id %in% rege$cell.id] = rege$post.lct
            if(is.null(track.postfire.rege)){
              track.postfire.rege = group_by(rege, lct, post.lct) %>% summarise(area=length(lct)*params$ha.cell) %>% 
                add_column(run = irun, .before = "lct") %>% add_column(year = t, .before = "lct")
            } else{
              track.postfire.rege = rbind(track.postfire.rege, 
                group_by(rege, lct, post.lct) %>% summarise(area=length(lct)*params$ha.cell) %>% 
                add_column(run = irun, .before = "lct") %>% add_column(year = t, .before = "lct"))
            }
          }
        }
        gc(verbose = FALSE)
      }
     
      ## 5. FOREST RECOVER
      if(is.forest.recover & t %in% forest.recover.schedule){
        recover = forest.recover(land, params, shrub2pine, shrub2oak, t)
        if(length(recover$pine.recover.cells)>0){
          land$lct[land$cell.id %in% recover$pine.recover.cells] = "pine"
        }
        if(length(recover$oak.recover.cells)>0){
          land$lct[land$cell.id %in% recover$oak.recover.cells] = "oak"
        }
        land$tschg[land$cell.id %in% c(recover$pine.recover.cells, recover$oak.recover.cells)] = 0
        land$trans.type[land$cell.id %in% c(recover$pine.recover.cells, recover$oak.recover.cells)] = "ForestRecover"
        shrub2pine = recover$shrub2pine
        shrub2oak = recover$shrub2oak
        if(is.null(track.forest.recover)){
          track.forest.recover = data.frame(run = irun, year = t, recover$res)
        } else{
          track.forest.recover = rbind(track.forest.recover, data.frame(run = irun, year = t, recover$res))
        }
        gc(verbose = FALSE)
      }
      
      ## 6. AFFORESTATION
      if(is.afforestation & t %in% afforest.schedule){
        afforest.cells = afforestation(land, params)
        land$lct[land$cell.id %in% afforest.cells[[1]]] = "oak"
        land$lct[land$cell.id %in% afforest.cells[[2]]] = "pine"
        land$tschg[land$cell.id %in% afforest.cells[[1]]] = 0
        land$tschg[land$cell.id %in% afforest.cells[[2]]] = 0
        land$trans.type[land$cell.id %in% afforest.cells[[1]]] = "AfforestOak"
        land$trans.type[land$cell.id %in% afforest.cells[[2]]] = "AfforestPine"
        if(is.null(track.afforest)){
          track.afforest = data.frame(run = irun, year = t, afforest.cells[[3]])
        } else{
          track.afforest = rbind(track.afforest, data.frame(run = irun, year = t, afforest.cells[[3]]))
        }
        gc(verbose = FALSE)
      }
       
      ## 7. ENCROACHMENT
      if(is.encroachment & t %in% encroach.schedule){
        encroach.cells = encroachment(land, params)
        land$lct[land$cell.id %in% encroach.cells[[1]]] = "shrub"
        land$tschg[land$cell.id %in% encroach.cells[[1]]] = 0
        land$trans.type[land$cell.id %in% encroach.cells[[1]]] = "Encroach"
        if(is.null(track.encroach)){
          track.encroach = data.frame(run = irun, year = t, encroach.cells[[2]])
        } else{
          track.encroach = rbind(track.encroach, data.frame(run = irun, year = t, encroach.cells[[2]]))
        }
        gc(verbose = FALSE)
      }
      
      ## 8. AGING
      ## Increment the Time Since Last Change for the dynamic covers
      land$tschg = land$tschg + 1 
      
      ## 9. END OF THE YEAR: TRACKING
      ## Land-cover types distribution at time t
      aux = land %>% group_by(lct) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "lct") %>% add_column(year = t, .before = "lct")
      track.land = rbind(track.land, aux)
      ## Unburnt land-cover types distribution at time t
      sel = is.na(land$trans.type) | (land$trans.type!="Fire") | (land$trans.type=="Fire" & land$tschg>1) 
      aux = land[sel,] %>% group_by(lct) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "lct") %>% add_column(year = t, .before = "lct")
      if(is.null(track.unburnt.land)){
        track.unburnt.land = aux
      } else{
        track.unburnt.land = rbind(track.unburnt.land, aux)  
      }
      ## Oak distribution per each age class at time t
      aux = land %>% filter(lct=="oak") %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "tschg") %>% add_column(year = t, .before = "tschg")
      track.oak.age = rbind(track.oak.age, aux)
      ## Pine distribution per each age class at time t
      aux = land %>% filter(lct=="pine") %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "tschg") %>% add_column(year = t, .before = "tschg")
      track.pine.age = rbind(track.pine.age, aux)
      ## Unburnt oak distribution per each age class at time t
      sel = (land$lct=="oak") 
      sel = sel & (is.na(land$trans.type) | (land$trans.type!="Fire") | (land$trans.type=="Fire" & land$tschg>1))
      aux = land[sel,] %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "tschg") %>% add_column(year = t, .before = "tschg")
      if(is.null(track.unburnt.oak.age)){
        track.unburnt.oak.age = aux
      } else{
        track.unburnt.oak.age = rbind(track.unburnt.oak.age, aux)  
      }
      ## Unburnt pine distribution per each age class at time t
      sel = (land$lct=="pine") 
      sel = sel & (is.na(land$trans.type) | (land$trans.type!="Fire") | (land$trans.type=="Fire" & land$tschg>1) )
      aux = land[sel,] %>% group_by(tschg) %>% summarise(area = length(lct) * params$ha.cell) %>% 
        add_column(run = irun, .before = "tschg") %>% add_column(year = t, .before = "tschg")
      if(is.null(track.unburnt.pine.age)){
        track.unburnt.pine.age = aux
      } else{
        track.unburnt.pine.age = rbind(track.unburnt.pine.age, aux)  
      }
      
      ## 10. END OF THE YEAR: SAVE DF AND MAPS
      ## Save 'land' object
      if(save.land & t %in% params$tseq.save.land){
        saveRDS(land, paste0(scenDir, "/landscape/land_run", irun, "_time", t, ".rds"))
      }
      ## Write land.cover map
      if(out.maps & t %in% params$tseq.out.maps){
        out.raster = mask.study.area
        dta = data.frame(cell.id = 1:ncell(out.raster)) %>% left_join(land[,c("cell.id", "lct")], by="cell.id") %>% 
          left_join(land.cover.type, by = "lct")
        out.raster[] = dta$lct.id
        writeRaster(out.raster, filename = paste0(scenDir, "/maps/land.cover.type_run", irun, "time", t, ".tif"), overwrite = T, format = "GTiff")
      }
      ## Free memory
      gc(verbose = FALSE) 
    }
  
    if(write.outputs){
      names(track.oak.age)[3] = names(track.pine.age)[3] = "age"
      names(track.unburnt.oak.age)[3] = names(track.unburnt.pine.age)[3] = "age"
      names(track.forest.age.burnt)[5] = names(track.forest.age.supp)[5] = "age"
      write.table(track.land, paste0(scenDir, "/land", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.oak.age, paste0(scenDir, "/oak.age", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.pine.age, paste0(scenDir, "/pine.age", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.unburnt.land, paste0(scenDir, "/unburnt.land", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.unburnt.oak.age, paste0(scenDir, "/unburnt.oak.age", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.unburnt.pine.age, paste0(scenDir, "/unburnt.pine.age", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.lcc, paste0(scenDir, "/lcc", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.postfire.rege, paste0(scenDir, "/postfire.rege", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.forest.recover, paste0(scenDir, "/forest.recover", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.afforest, paste0(scenDir, "/afforest", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.encroach, paste0(scenDir, "/encroach", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.fires, paste0(scenDir, "/fires", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.pb, paste0(scenDir, "/pburns", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.lc.burnt.supp, paste0(scenDir, "/lc.burnt", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.forest.age.burnt, paste0(scenDir, "/forest.age.burnt", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.forest.age.supp, paste0(scenDir, "/forest.age.supp", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
      write.table(track.lc.pburnt, paste0(scenDir, "/lc.pburnt", params$outputs.suffix, ".txt"), row.names=F, quote=F, sep="\t", append=ifelse(irun==1,F,T), col.names=ifelse(irun==1,T,F))
    }
    
  }
  
  ## 
  cat("\nC. Build outputs ...\n")
  
  ## Add annual demand of land-cover transitions
  if(is.land.cover.change){
    lcc.demand$year = as.numeric(row.names(lcc.demand))
    lcc.demand = pivot_longer(lcc.demand, cols=SmartPlant:PastureAbnd, names_to="trans.type", values_to="trgt.dmnd")
    # track.lcc = track.lcc %>% left_join(lcc.demand, by = c("year", "trans.type"))
  }
  
  ## List of tracking data frames
  res = list(land = track.land, oak.age = track.oak.age, pine.age = track.pine.age, 
             unburnt.land = track.unburnt.land, unburnt.oak.age = track.unburnt.oak.age,
             unburnt.pine.age = track.unburnt.pine.age, land.cover.change = track.lcc,
             postfire.rege = track.postfire.rege, forest.recover = track.forest.recover, 
             afforest = track.afforest, encroach = track.encroach, fires = track.fires, 
             lc.burnt.supp = track.lc.burnt.supp, forest.age.burnt = track.forest.age.burnt, 
             forest.age.supp = track.forest.age.supp, pb = track.pb, lc.pburns = track.lc.pburnt)
  return(res)
}
  
  