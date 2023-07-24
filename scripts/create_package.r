# R package creation
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html

# Once you have got your documentation completed, you can simply run:
devtools::document()

## Create object x in the data/ folder
usethis::use_data(x, overwrite = T)

## All packages listed in Depends, Imports and LinkingTo must be installed, 
## and their version requirements must be met, otherwise your package cannot be checked. 
## An easy way to install any missing or outdated dependencies is to run 
devtools::install_deps(dependencies = TRUE)

## We can use the devtools::load_all() function which will load your R package into memory
## exposing all the functions and data that we highlighted above. However as soon as you close your R session, 
## the package will no longer be available.
devtools::load_all() 

## To actually install your package, you use the devtools::install() function which installs your R package 
## into your R system library. Then you will be able to load up your package with library("myfirstpackage")
## Along with all the data that comes with the package!
devtools::install(upgrade="never", build_vignettes = FALSE, dependencies = F) 

## To set up your package with Rcpp, run:
usethis::use_rcpp()

## A bundled package is a package that has been compressed into a single file. By convention (from Linux), 
## package bundles in R use the extension .tar.gz and are sometimes referred to as as source tarball. 
## In the rare case that you need to make a bundle from a package you are developing locally, use
devtools::build(vignettes=T)  
  # 25/08/22 vignettes=T returns an error. It fails at line 413:
  # res = IFNmanagement(exampleTreeData, examplePrescriptionData)
  # Likely because the examplePrescriptionData is no longer compatible with the new version of the 
  # IFNmanagement function. Try to change the example data frame to make operative the vignette and the help pages

# check automatically builds and checks a source package, using all known best practices. 
# check_built checks an already built package.
devtools::check(manual=FALSE)

## Create package manual in pdf, with all the functions
devtools::build_manual()


## Vignettes are extremely important to give people a high-level understanding of what your R package can do.  
##To get started with generating a vignette, you can use the devtools::use_vignette() function for this. For instance,
devtools::use_vignette("introduction")
## Note that starting in devtools 2.1.0 the use_vignette function was moved to the usethat package. 
## So if you are using a newer version of devtools, you can run:
usethis::use_vignette("REMAINS")
## This will create a vignette/introduction.Rmd file. This is a vignette template Rmarkdown file that you can 
## then use to fill out steps on how you can use your package.


# You can build all vignettes from the console with devtools::build_vignettes(), but this is rarely useful. 
# Instead use devtools::build() to create a package bundle with the vignettes included. 
# RStudio’s “Build & reload” does not build vignettes to save time. 
# Similarly, devtools::install_github() (and friends) will not build vignettes by default 
# because they’re time consuming and may require additional packages. 
#You can force building with devtools::install_github(build_vignettes = TRUE). 
#This will also install all suggested packages.
devtools::build_vignettes() 
library(medfire)
browseVignettes("medfire")  # no funciona
