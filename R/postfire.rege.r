#' Simulation of post-fire regeneration in forest areas
#'
#' @name postfire.rege
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#'
#' @return Returns a vector with cells' identifiers to change the land-cover type
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' data(postfire.trans.matrix)
#' params = default.params()
#' landscape$trans.type = NA 
#' x = landscape$cell.id[landscape$lct %in% c("pine", "oak")]
#' y = sample(x, 100, replace=F)
#' landscape$trans.type[landscape$cell.id %in% y] = "Fire"
#' landscape$tschg[landscape$cell.id %in% y] = 0
#' res = postfire.rege(landscape, params)
#' 

postfire.rege = function(land, params){
  
  ## Tracking the transition of burnt forest cells
  cat(" Post-fire regeneration", "\n") 
  
  ## Select the forest cells that have burnt the current time step and joint the data
  ## relative to elevation
  burnt.cells = land %>% filter(tschg==0, trans.type=="Fire") %>%   #lc %in% c("oak", "pine"),
    left_join(select(orography, cell.id, elevation), by="cell.id")
  
  ## Only continue if there are forest cells that have burnt 
  if(nrow(burnt.cells)>0){
    
    ## By default, make that all covers auto-regenerate (even if it's only true for crop, grass, sparseveg and shrub)
    burnt.cells$post.lct = burnt.cells$lct
      
    ## Pine and oaks regenerate to themselves, shrubs and sparseveg. 
    ## However, sparseveg only is found in high elevation, so the assignation of a new land-cover type
    ## has to be done according to the amount of burnt forest in each elevation class and respecting
    ## the transition probabilites for each species.
    
    ## So first, count the proportion of burnt pines and oaks in low and high elevations
    prop.lc.elev = burnt.cells %>% filter(lct %in% c("pine", "oak")) %>% group_by(lct) %>% 
      summarise(low=sum(elevation <= params$elevation.rocky), 
                high=sum(elevation > params$elevation.rocky),
                tot=sum(elevation > -1)) 
    prop.lc.elev$low = 100 * prop.lc.elev$low / prop.lc.elev$tot
    prop.lc.elev$high = 100 * prop.lc.elev$high / prop.lc.elev$tot
    prop.pine.elev = prop.lc.elev[prop.lc.elev$lct == "pine",]
    prop.oak.elev = prop.lc.elev[prop.lc.elev$lct == "oak",]
    
    ## For burnt pines,
    if(nrow(prop.pine.elev)>0){
      
      ## a. Retrieve the post-fire regeneration proportions of pines
      ptrans = postfire.trans.matrix[postfire.trans.matrix$lc.prefire=="pine", c("shrub", "pine", "sparseveg")]
      
      ## b. If the proportion of burnt pines in high elevations is higher than the sparseveg transition, 
      ## modify the transition probabilities of shrub and sparseveg.
      ptrans$shrub = ptrans$shrub + pmax(0, ptrans$sparseveg-prop.pine.elev$high)
      ptrans$sparseveg = ptrans$sparseveg - pmax(0, ptrans$sparseveg-prop.pine.elev$high)
      write.table(ptrans, file = "outputs/ptrans_pine.txt", quote=F, sep="\t", row.names = F, append = F)
      
      ## c. Find out the proportion of each new category according to 'ptrans'
      z = ptrans$sparseveg / prop.pine.elev$high
      x=y=w=NA
      if(is.na(z)|is.infinite(z)){
        y = 1
        x = ptrans$shrub
        w = ptrans$pine
        z = 0
      } else if(z == 1){
        y = 0
        x = ptrans$shrub
        w = ptrans$pine
        z = 1
      } else if(z<1){
        x = runif(1, 0, pmin(1,ptrans$shrub/prop.pine.elev$low))
        y = (ptrans$shrub - prop.pine.elev$low*x)/prop.pine.elev$high
        w = 1-x
      } else{
        cat(paste0("prop.pine.elev: ", prop.pine.elev, "\n"))
        write.table(ptrans, file = "outputs/ptrans.txt", quote=F, sep="\t", row.names = F)
        x=y=z=w=1
      }
      cat(paste0("PINE - x:", x, " y:", y, " z:", z, " w:", w, "\n"))
      if(sum(is.na(x))+sum(is.na(y))+sum(is.na(z))+sum(is.na(w))>0){
        cat(paste0("x: ", x, "\n"))
        cat(paste0("y: ", y, "\n"))
        cat(paste0("z: ", z, "\n"))
        cat(paste0("w: ", w, "\n"))
        cat(paste0("prop.pine.elev: ", prop.pine.elev, "\n"))
        write.table(ptrans, file = "outputs/ptrans.txt", quote=F, sep="\t", row.names = F)
      }
      # to avoid the error 'NA in probability vector'
      x = ifelse(is.na(x)|is.infinite(x), 0, x)
      y = ifelse(is.na(y)|is.infinite(y), 0, y)
      z = ifelse(is.na(z)|is.infinite(z), 0, z)
      w = ifelse(is.na(w)|is.infinite(w), 0, w)
      
      ## d. Assign a new post.lct type for burnt pines
      sel =  burnt.cells$lct=="pine" & burnt.cells$elevation <= params$elevation.rocky
      burnt.cells$post.lct[sel] = sample(c("shrub", "pine"), sum(sel), replace = T, prob = c(x,w))
      sel =  burnt.cells$lct=="pine" & burnt.cells$elevation > params$elevation.rocky
      burnt.cells$post.lct[sel] = sample(c("shrub", "sparseveg", "pine"), sum(sel), replace = T, prob = c(y,z,max(0,1-z-y)))      
    }


    ## For oaks,
    if(nrow(prop.oak.elev)>0){
      
      ## a. Retrieve the post-fire regeneration proportions of oaks
      ptrans = postfire.trans.matrix[postfire.trans.matrix$lc.prefire=="oak", c("shrub", "oak", "sparseveg")]
      
      ## b. If the proportion of burnt oaks in high elevatiosn is higher than the sparseveg transition, 
      ## modify the transition probabilities of shrub and sparseveg
      ptrans$shrub = ptrans$shrub + pmax(0,  ptrans$sparseveg-prop.oak.elev$high)
      ptrans$sparseveg = ptrans$sparseveg - pmax(0,  ptrans$sparseveg-prop.oak.elev$high)
      write.table(ptrans, file = "outputs/ptrans_oak.txt", quote=F, sep="\t", row.names = F, append = F)
      
      ## c. Find out the proportion of each new category according to 'ptrans'
      z = ptrans$sparseveg / prop.oak.elev$high
      x=y=w=NA
      if(is.na(z)|is.infinite(z)){
        y = 1
        x = ptrans$shrub
        w = ptrans$oak
        z = 0
      } else if(z == 1){
        y = 0
        x = ptrans$shrub
        w = ptrans$oak
        z = 1
      } else if(z<1){
        x = runif(1, 10^(-6), pmin(1,ptrans$shrub/prop.oak.elev$low))
        y = (ptrans$shrub - prop.oak.elev$low*x)/prop.oak.elev$high
        w = 1-x
      } else{
        cat(paste0("prop.pine.elev: ", prop.pine.elev, "\n"))
        write.table(ptrans, file = "outputs/ptrans.txt", quote=F, sep="\t", row.names = F)
        x=y=z=w=1
      }
      cat(paste0("OAK - x:", x, " y:", y, " z:", z, " w:", w, "\n"))
      if(sum(is.na(x))+sum(is.na(y))+sum(is.na(z))+sum(is.na(w))>0){
        cat(paste0("x: ", x, "\n"))
        cat(paste0("y: ", y, "\n"))
        cat(paste0("z: ", z, "\n"))
        cat(paste0("w: ", w, "\n"))
        cat(paste0("prop.pine.elev: ", prop.oak.elev, "\n"))
        write.table(ptrans, file = "outputs/ptrans.txt", quote=F, sep="\t", row.names = F, append = F)
      }
      # to avoid the error 'NA in probability vector'
      x = ifelse(is.na(x)|is.infinite(x), 0, x)
      y = ifelse(is.na(y)|is.infinite(y), 0, y)
      z = ifelse(is.na(z)|is.infinite(z), 0, z)
      w = ifelse(is.na(w)|is.infinite(w), 0, w)
      
      ## d. Assign a new post.lct type for burnt oaks
      sel =  burnt.cells$lct=="oak" & burnt.cells$elevation <= params$elevation.rocky
      burnt.cells$post.lct[sel] = sample(c("shrub", "oak"), sum(sel), replace = T, prob = c(x,w))
      sel =  burnt.cells$lct=="oak" & burnt.cells$elevation > params$elevation.rocky
      burnt.cells$post.lct[sel] = sample(c("shrub", "sparseveg", "oak"), sum(sel), replace = T, prob = c(y,z,max(0,1-z-y)))
    }
    
    ## Finally, the regeneration of the forest cells that regenerate to the same species 
    ## happens gradually over time, knowing that on a certain future 100% is converted 
    ## to the target forest species (see function forest.recover)
    burnt.cells$post.lct[burnt.cells$post.lct=="oak"] = "shrub.to.oak"
    burnt.cells$post.lct[burnt.cells$post.lct=="pine"] = "shrub.to.pine"
    
    return(select(burnt.cells, cell.id, lct, post.lct))  
      
  } else{
    return(NULL)
  }
}