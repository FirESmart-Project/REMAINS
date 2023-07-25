#' Compute the interface category 
#' 
#' @name interface
#'
#' @param land Data frame with the description of the landscape: cell identifier, land-cover type, and
#' time since last cover-change
#' @param params List of the parameters of the model 
#'
#' @return Returns a vector with the interface category: Urb = 1, Crp = 2, Shrb = 3, Frst = 4, Oth = 5,
#' UrbCrp = 6, UrbShrb = 7, UrbFrst = 8, CrpShrb = 9, CrpFrst = 10, and ShrbFrst = 11
#' 
#' @details
#' The interface category is a broad descriptor of the 1 km2 type of neighborhood of each cell that is function
#' of the distribution of the land-cover types within the neighborhood 
#'
#' @export
#' 
#' @examples
#' data(landscape)
#' params = default.params()
#' res = interface(landscape, params)
#' 

interface = function(land, params){
  
  ## Tracking
  cat(" Land interfaces", "\n")
  
  ## Join UTM info to land
  land.utm = left_join(land, select(orography, cell.id, utm), by="cell.id") 
  
  ## Count each land-cover type per UTM, compute percentages
  landtype = aggregate(list(tot = (!is.na(land.utm$lct)),
                            urb = (land.utm$lct=="urban"), 
                            crp = (land.utm$lct=="crop"),
                            shrb = (land.utm$lct %in% c("shrub", "grass", "shrub.to.pine", "shrub.to.oak")),
                            frst = (land.utm$lct %in% c("pine", "oak")),
                            oth = (land.utm$lct %in% c("sparseveg", "water", "baresoil"))), 
                       list(utm=land.utm$utm), sum)
  landtype[,3:ncol(landtype)] = 100*landtype[,3:ncol(landtype)]/landtype$tot
  
  ## Apply rules to classify: first step of classification in neighbor types
  dta = data.frame(utm=landtype$utm,
                   urb=landtype$urb>=80, crp=landtype$crp>=80, shrb=landtype$shrb>=80,
                   frst=landtype$frst>=80, oth=landtype$oth>=80)
  dta$urbcrp = landtype$urb>=20 & landtype$crp>30 & dta$crp==0
  dta$urbshrb = landtype$urb>=20 & landtype$shrb>30 & dta$shrb==0
  dta$urbfrst = landtype$urb>=20 & landtype$frst>30 & dta$frst==0
  dta$crpshrb = landtype$urb<20 & landtype$crp>=20 & landtype$shrb>=20 & !dta$crp & !dta$shrb & (!dta$urbcrp | !dta$urbshrb | !dta$urbfrst)
  dta$crpfrst = landtype$urb<20 & landtype$crp>=20 & landtype$frst>=20 & !dta$crp & !dta$frst & !dta$urbcrp & !dta$urbshrb & !dta$urbfrst
  dta$shrbfrst = landtype$urb<20 & landtype$crp<20 & landtype$shrb>=20 & landtype$frst>=20 & !dta$shrb & !dta$frst 
  dta$tot = dta$urb+dta$crp+dta$shrb+dta$frst+dta$oth+dta$urbcrp+dta$urbshrb+dta$urbfrst+dta$crpshrb+dta$crpfrst+dta$shrbfrst
  
  ##  Double classification of CrpShrb & CrpFrst 
  dta$crpshrb[dta$tot>1 & dta$crpshrb & dta$crpfrst] = 
    ifelse(landtype$shrb[dta$tot>1 & dta$crpshrb & dta$crpfrst] > landtype$frst[dta$tot>1 & dta$crpshrb & dta$crpfrst], T, F)
  dta$crpfrst[dta$tot>1 & dta$crpshrb & dta$crpfrst] = 
    ifelse(landtype$frst[dta$tot>1 & dta$crpshrb & dta$crpfrst] > landtype$shrb[dta$tot>1 & dta$crpshrb & dta$crpfrst], T, F)
  ##  Double classification of UrbCrp & UrbFrst
  dta$urbcrp[dta$tot>1 & dta$urbcrp & dta$urbfrst] = 
    ifelse(landtype$crp[dta$tot>1 & dta$urbcrp & dta$urbfrst] > landtype$frst[dta$tot>1 & dta$urbcrp & dta$urbfrst], T, F)
  dta$urbfrst[dta$tot>1 & dta$urbcrp & dta$urbfrst] = 
    ifelse(landtype$frst[dta$tot>1 & dta$urbcrp & dta$urbfrst] > landtype$crp[dta$tot>1 & dta$urbcrp & dta$urbfrst], T, F)
  ##  Double classification of UrbShrb & UrbFrst
  dta$urbshrb[dta$tot>1 & dta$urbshrb & dta$urbfrst] = 
    ifelse(landtype$shrb[dta$tot>1 & dta$urbshrb & dta$urbfrst] > landtype$frst[dta$tot>1 & dta$urbshrb & dta$urbfrst], T, F)
  dta$urbfrst[dta$tot>1 & dta$urbshrb & dta$urbfrst] = 
    ifelse(landtype$frst[dta$tot>1 & dta$urbshrb & dta$urbfrst] > landtype$shrb[dta$tot>1 & dta$urbshrb & dta$urbfrst], T, F)
  ##  Double classification of UrbShrb & UrbCrp
  dta$urbshrb[dta$tot>1 & dta$urbshrb & dta$urbcrp] = 
    ifelse(landtype$shrb[dta$tot>1 & dta$urbshrb & dta$urbcrp] > landtype$crp[dta$tot>1 & dta$urbshrb & dta$urbcrp], T, F)
  dta$urbcrp[dta$tot>1 & dta$urbshrb & dta$urbcrp] = 
    ifelse(landtype$crp[dta$tot>1 & dta$urbshrb & dta$urbcrp] > landtype$shrb[dta$tot>1 & dta$urbshrb & dta$urbcrp], T, F)
  dta$tot = dta$urb+dta$crp+dta$shrb+dta$frst+dta$oth+dta$urbcrp+dta$urbshrb+dta$urbfrst+dta$crpshrb+dta$crpfrst+dta$shrbfrst
  
  ## Some UTM has not been classified, so relax conditions for mono-interfaces
  dta$urb[dta$tot==0] = landtype$urb[dta$tot==0]>=75
  dta$crp[dta$tot==0] = landtype$crp[dta$tot==0]>=75
  dta$shrb[dta$tot==0] = landtype$shrb[dta$tot==0]>=75
  dta$frst[dta$tot==0] = landtype$frst[dta$tot==0]>=75
  dta$oth[dta$tot==0] = landtype$oth[dta$tot==0]>=75
  dta$tot = dta$urb+dta$crp+dta$shrb+dta$frst+dta$oth+dta$urbcrp+dta$urbshrb+dta$urbfrst+dta$crpshrb+dta$crpfrst+dta$shrbfrst
  
  
  ## Some UTM has not been classified, so relax conditions for combined interfaces
  # crpshrb
  dta$crpshrb[dta$tot==0] = landtype$urb[dta$tot==0]<20 & 
    landtype$crp[dta$tot==0]>=15 & landtype$shrb[dta$tot==0]>=15 & 
    dta$crp[dta$tot==0]==0 & dta$shrb[dta$tot==0]==0 &
    ((landtype$shrb[dta$tot==0]+landtype$crp[dta$tot==0])>=70)
  # crpfrst
  dta$crpfrst[dta$tot==0] = landtype$urb[dta$tot==0]<20 & 
    landtype$crp[dta$tot==0]>=15 & landtype$frst[dta$tot==0]>=15 & 
    dta$crp[dta$tot==0]==0 & dta$frst[dta$tot==0]==0 &
    dta$crpshrb[dta$tot==0]==0 & # to avoid double classification
    ((landtype$frst[dta$tot==0]+landtype$crp[dta$tot==0])>=70)
  # shrbfrst
  dta$shrbfrst[dta$tot==0] = landtype$urb[dta$tot==0]<20 & 
    landtype$shrb[dta$tot==0]>=15 & landtype$frst[dta$tot==0]>=15 & 
    dta$shrb[dta$tot==0]==0 & dta$frst[dta$tot==0]==0 & 
    dta$crpshrb[dta$tot==0]==0 & dta$crpfrst[dta$tot==0]==0 & # to avoid double classification
    ((landtype$frst[dta$tot==0]+landtype$shrb[dta$tot==0])>=70)
  dta$tot = dta$urb+dta$crp+dta$shrb+dta$frst+dta$oth+dta$urbcrp+dta$urbshrb+dta$urbfrst+dta$crpshrb+dta$crpfrst+dta$shrbfrst
  
  ## Assign the numeric value of the interface (still some have no interface assigned)
  dta$inter.id = apply(dta[,-c(1, ncol(dta))] * matrix(1:(ncol(dta)-2), 
                  nrow=nrow(dta), ncol=ncol(dta)-2, byrow=T), 1, sum)
  
  ## For those utm cells that have not been classified yet, find the two most abundant and mark the combination
  landtype$urbcrp = landtype$urb + landtype$crp
  landtype$urbshrb = landtype$urb + landtype$shrb
  landtype$urbfrst = landtype$urb + landtype$frst
  landtype$crpshrb = landtype$crp + landtype$shrb
  landtype$crpfrst = landtype$crp + landtype$frst
  landtype$shrbfrst = landtype$shrb + landtype$frst
  is.max = apply(landtype[,-c(1:2)], 1, which.max)
  dta$inter.id[dta$inter.id==0] = is.max[dta$inter.id==0]

  ## Join to the final data.frame
  dta = dta %>% left_join(params$interfaces, by="inter.id")
  land.utm = left_join(land.utm, select(dta, utm, inter.name), by="utm") %>% select(inter.name)
  gc(verbose = FALSE)
  return(unlist(land.utm))
}
