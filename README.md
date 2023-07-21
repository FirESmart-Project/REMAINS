# The REMAINS model

## Model's aim

**REMAINS** is a spatially explicit process-based model that integrates the main factors driving fire-landscape dynamics in Southern European mountain landscapes. 

**REMAINS** model includes fire-vegetation dynamics, fire management through different fire-suppression strategies, and land-cover changes (namely, forest plantations, agricultural conversion, and rural abandonment). It allows investigating how the spatio-temporal interactions between these processes affect fire regime and thereby landscape composition and dynamics at short- and medium-timescales. 

It is currently initialized and calibrated for the [Geres-Xures Transboundary Biosphere Reserve (Portugal/Spain)](https://en.unesco.org/biosphere/eu-na/geres-xures).

Each model scenario dictates which ecological and anthropogenic processes are active. The processes that are currently included are:  
1.	Land-cover changes  
2.	Wildfires and fire suppression  
3.	Prescribed fire  
4.	Forest recover  
5.	Post-fire regeneration  
6.	Afforestation   
7.	Vegetation encroachment  

Thus, the current version of the model is implemented thought the following functions:  
**afforestation**: simulates the colonization of scrublands by oak and pine species, occurs at a certain annual rate, only can take place after a period since the last land-cover transformation and depends on the percentage of oak and/or pine forest found in a circular neighborhood.  
**default.params**: initializes the parameters and the global variables of the model.  
**encroachment**: the vegetation encroachment is the transformation of rocky areas or open scrublands to close scrublands. 
**fire.risk**: simulates the fire risk areas as a function of various methodological parameters.  
**forest.recover**: simulates the forest recovery (pine and oak) after fire (wildfire and prescribed fire) under a given period.   
**interface**: assing the type of neighbourhood to each raster cell.  
**land.cover.change**: simulates land-cover transitions (e.g. rural abandonment or forest conversion) following a demand allocation approach.  
**land.dyn.mdl**: loads the spatial state variables, the initialization of model´s parameters, creates the scenario output sub-folder and schedules the processes, e.g., land-cover changes, wildfires, prescribed fire, vegetation dynamics.  
**postfire.rege**: simulates forest regeneration after fire.  
**prescribed.burn**: simulates the use of prescribed fire according to spatial and temporal criteria.
**wildfires**: simulates spatially explicit wildfires and fire suppression.  

## Package installation

Users can download and install the latest stable version of the **medLDM** package from GitHub as follows (required package devtools should be installed/updated first):

```R
devtools::install_github("FirESmart-Project/REMAINS")
```
Additionally, users can have help to run package functions directly as package vignettes, by forcing their inclusion in installation:

```R
devtools::install_github("FirESmart-Project/REMAINS", 
                         build_manual = TRUE,
                         build_vignettes = TRUE)
```

## How to run the REMAINS model

Call the function \code{land.dyn.mdl} to run the **REMAINS** model.

```R
# Clean local enviornment
rm(list = ls())

# Scenario name
scenName = "remains_test"
scenDir = paste0("outputs/", scenName)

# Model's parameters
params = default.params()
params$time.horizon = 10
lcc.demand = data.frame(SmartPlant = round(runif(50, 1, 10)), 
                        AgriConver = round(runif(50, 1, 10)), 
                        RuralAbnd = round(runif(50, 1, 10)))

# Run the model
res = land.dyn.mdl(scenDir = scenDir, is.land.cover.change = TRUE, is.wildfire = TRUE,
                   is.prescribed.burn = TRUE, is.postfire.rege = TRUE, is.forest.recover = TRUE,
                   is.afforestation = TRUE, is.encroachment = TRUE, nrun = 1, save.land = FALSE, 
                   params = params, lcc.demand = lcc.demand)

# Visualize outputs
names(res)
res$land
res$oak.age
res$pine.age
res$unburnt.land
res$unburnt.oak.age
res$unburnt.pine.age
res$lcc
res$afforest
res$encroach
```

## References

Pais, S., Aquilué, N., Brotons, L., Honrado, J. P., Frenandes, P., Regos, A. The REMAINS R-package: Paving the Way for Fire-Landscape Modeling and Management (in prep.)

Pais, S., Aquilué, N., Campos, J., Sil, Â., Marcos, B., Martínez-Freiría, F., Domínguez J., Brotons, L., Honrado, J. P., Regos, A. 2020. Mountain farmland protection and fire-smart management jointly reduce fire hazard and enhance biodiversity and carbon sequestration. Ecosystem Services, 44, 101143. https://doi.org/10.1016/j.ecoser.2020.101143

Campos, J. C., Bernhardt, J., Aquilué, N., Brotons, L., Domínguez J., Lomba, Â., Marcos, B., Martínez-Freiría, F., Moreira, F., Pais, S., Honrado, J. P., Regos, A. 2021. Using fire to enhance rewilding when agricultural policies fail. Science of the Total Environment, 755, 142897. https://doi.org/10.1016/j.scitotenv.2020.142897
