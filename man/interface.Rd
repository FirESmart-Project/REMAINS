% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.r
\name{interface}
\alias{interface}
\title{Compute the interface category}
\usage{
interface(land, params)
}
\arguments{
\item{land}{Data frame with the description of the landscape: cell identifier, land-cover type, and
time since last cover-change}

\item{params}{List of the parameters of the model}
}
\value{
Returns a vector with the interface category: Urb = 1, Crp = 2, Shrb = 3, Frst = 4, Oth = 5,
UrbCrp = 6, UrbShrb = 7, UrbFrst = 8, CrpShrb = 9, CrpFrst = 10, and ShrbFrst = 11
}
\description{
Compute the interface category
}
\details{
The interface category is a broad descriptor of the 1 km2 type of neighborhood of each cell that is function
of the distribution of the land-cover types within the neighborhood
}
\examples{
data(landscape)
params = default.params()
res = interface(landscape, params)

}
