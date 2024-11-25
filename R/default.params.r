#' Default values of the model's parameters
#'
#' @name default.params
#'
#' @return Returns a list of the parameters of the model and the default values
#'
#' @export
#' 
#' @examples
#' params = default.params()
#' 
 
default.params = function(){
  return(list(
    
    ## Time
    time.horizon = 50,
    time.step = 1,
    cell.size = 100,
    ha.cell = (100^2)/10^4, # cell size in ha
    tseq.save.land = seq(0,50,5),
    tseq.out.maps = seq(0,50,5),
    outputs.suffix = "",
    
    ## Land-cover types
    land.cover.type = data.frame(lct.id = 1:10, lct = c("crop", "pine", "oak", "sparseveg", "shrub",
      "water", "grass", "urban", "shrub.to.pine", "shrub.to.oak")),  
                
    ## Interfaces
    interfaces = data.frame(inter.id = 1:11, inter.name = c("urb", "crp", "shrb", "frst", 
      "oth", "urbcrp", "urbshrb", "urbfrst", "crpshrb", "crpfrst", "shrbfrst")),
    
    ## Land-cover type fire proneness
    lct.fire.prone = data.frame(lct = c("crop", "pine", "oak", "sparseveg", "shrub",
                                        "water", "grass", "urban", "shrub.to.pine", "shrub.to.oak"),
                                flam = c(0.29, 0.90, 0.52, 1, 0.94, 0, 0.29, 0, 0.94, 0.94)),
    
    ## Weights of the spread rate factors
    wslope = 0.575,
    wlc = 0.425,
    facc = 2,
    rpb = 0.2,
    pb.upper.th = 0.8,
    pb.lower.th = -1,
    
    ## Fire suppression thresholds by region and strategy
    ## The fuel.th is between 0 and 1. Fire will be suppressed in those cells that spread.rate <= fuel.th
    ## The mosaic.th is an integer number greater or equal than 0. Fire will be suppressed if accumulated area 
    ## of contiguous agriculture is greater or equal than it
    fire.suppression = data.frame(region = c("Portugal", "Galicia"), fuel.th = c(0,0), mosaic.th = c(Inf,Inf)),
    
    ## Prescribed burns
    annual.area.pb = 0,
    pb.exclude.protected = F,
    pb.strategy = "random",
    buffroad.file = NULL,
    buffer.roadnet = 0,
    min.pb.target.area = NULL,
    max.pb.target.area = NULL,
    
    ## Ignition model
    ## Variables: intercept, urb, crp, shrb, urbcrp, urbshrb, urbfrst, crpshrb, crpfrst, elevation, dens.road
    igni.mdl = c(-2.057, 2.211, 1.886, 0.2442, 1.421, 1.034, 0.7438, 1.185, 1.18, -0.001512, 0.000003204),
    
    ## Parameters of the allocation procedure to simulate land-cover changes
    ## One column per land-cover transition: SmartPlant, AgriConver, RuralAbnd, PastureAbnd, PastureConver
    ## Per line: k, Ligni, Lsprd
    lc.alloc = data.frame(SmartPlant = c(0.3,0.25,10), AgriConver = c(0.3, 20, 10), RuralAbnd = c(0.2, 0.05, 20), 
                          PastureAbnd = c(0.2, 0.05, 20), PastureConver = c(0.3, 20, 10)),
    
    ## Fire-smart plantations 
    mode.trans.potential = data.frame(SmartPlant = "random", AgriConver = "random", RuralAbnd = "random", 
                                      PastureAbnd = "random", PastureConver = "random"),
    elev.for.crop = 1500,	# Maximum elevation for shrub convert to crop
    slope.for.crop = 17.5,	 # Maximum slope threshold for crops (quantile 90%)
    ratio.pasture.abnd = 0.5,  # ratio the Grassland to Sparsevegetation in PastureAbnd transition
    
    ## Afforestation rates and age constrictions
    rewilding.th = 9, # Minimum years after any transition to shrubs become oak forest through natural succession
    rate.afforest.oak = 1.6, # Annual rate in 100% of shrub converting to oak 
    rate.afforest.pine = 1.1, # Annual rate in 100% of shrub converting to pine
    radius.afforest.oak = 400, # Radius in m around a shrub location to find out oaks for the afforestation process
    radius.afforest.pine = 1000, # Radius in m around a shrub location to find out oaks for the afforestation process
    
    ## Encroachment rate and age constrictions
    shruby.th = 4,	 # Minimum years after any transition to rocky vegetation become shrublands
    rate.encroach = 0.7, # Annual rate in 100% of rocky veg converting to shrub
    # Radius around a a rocky location to find out shrubs, for the encroachment process
    radius.encroach = 120, # Radius in m 
    
    ## Post-fire
    is.contagion = FALSE, # Activate or deactivate the post-fire regeneration by contagion
    elevation.rocky = 543, # Minimum elevation where open - rocky vegetation is founded, otherwise it's shrub
    pcontag.postfire = 0.4, 	# Probability of post-fire regeneration by contagion
    
    ## Forest recover
    radius.pine.recover = 1000, # Radius in m around shrubs that have to become pine after fire. 1000/30m = 33.3 cells
    radius.oak.recover = 400 # Radius in m around shrubs that have to become oak after fire. 400/30m = 13.3 cells
      
    )
  )
}



