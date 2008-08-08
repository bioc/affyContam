\name{setRectRegion}
\alias{setRectRegion}
\alias{setCircRegion}
\alias{getRectRegion}
\alias{getCircRegion}
\title{set a rectangular or circular region in an affybatch to a
 specified set of values}
\description{set a rectangular or circular region in an affybatch to a
 specified set of values}
\usage{
setRectRegion(x, chip=1, xinds=251:350, yinds=251:350, vals=10, valgen=NULL)
setCircRegion(x, chip=1, center=c(350,350), rad=100, vals=10, valgen=NULL)
getRectRegion(x, chip=1, xinds=251:350, yinds=251:350)
getCircRegion(x, chip=1, center=c(350,350), rad=100)
}
\arguments{
  \item{x}{ AffyBatch instance }
  \item{chip}{ sample index }
  \item{xinds}{ x coordinates to be contaminated }
  \item{yinds}{ y coordinates to be contaminated }
  \item{vals}{ values to be assigned to rectangle elements }
  \item{center}{ geometric center of circle to be altered }
  \item{rad}{ radius of circle to be altered, in xy units of the chip addressing system
used by xy2indices in the cdf package}
  \item{cdfname}{ cdfname}
  \item{valgen}{ function of parameter n that generates n values to be inserted in the
altered region }
}
%\details{
%}
\value{
set* functions return AffyBatch instance with intensities modified as requested

get* functions return numeric vectors of intensities as requested.
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
library(affydata)
data(Dilution)
opar = par(no.readonly=TRUE)
par(mfrow=c(2,2))
hist(Dilution, main="original")
image(Dilution[,1], main="original")
#
# we will contaminate in two ways: thin line at fixed low intensity, and 
# circular blob at moderate random intensity
#
ab = setRectRegion(Dilution, 1, xinds=25:30, yinds=1:620,
  vals=10)
ab = setCircRegion(ab, 1, valgen=function(n){
   rnorm(n, 350,50)})
hist(ab, main="chip 1 contaminated by normal")
image(ab[,1], main="chip 1 contaminated")
ex = getCircRegion(Dilution, 1)
length(ex)
ab = setCircRegion(Dilution, 1, vals=pmin(2*ex,65535))
image(ab[,1], main="chip 1 contaminated by doubling")
par(opar)
}
\keyword{ models }
