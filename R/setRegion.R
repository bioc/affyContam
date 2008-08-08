
setRectRegion = function(x, chip=1, xinds = 251:350, yinds = 251:350, vals = 10,
   valgen=NULL) {
  cdfname = paste(annotation(x), "cdf", sep="")
  require(cdfname, character.only=TRUE, quietly=TRUE)
  badco = expand.grid(xinds, yinds)
  indsbad = apply(badco,1,function(x) xy2indices(x[1], x[2], 
    cdf=cdfname))
  if (is.null(valgen)) exprs(x)[indsbad, chip] = vals
  else exprs(x)[indsbad, chip] = valgen(length(indsbad))
  x
}

setCircRegion = function(x, chip=1, center=c(350,350), rad=100,
  vals = 10, valgen=NULL) {
  cdfname = paste(annotation(x), "cdf", sep="")
  require(cdfname, character.only=TRUE, quietly=TRUE)
  xext = seq(center[1]-rad, center[1]+rad)
  yext = seq(center[2]-rad, center[2]+rad)
  badco = expand.grid(xext, yext)
  badco = badco[ (badco[,1] - center[1])^2 + (badco[,2]-center[2])^2 < rad^2, ]
  indsbad = apply(badco,1,function(x) xy2indices(x[1], x[2], 
    cdf=cdfname))
  if (is.null(valgen)) exprs(x)[indsbad, chip] = vals
  else exprs(x)[indsbad, chip] = valgen(length(indsbad))
  x
}

getCircRegion = function(x, chip=1, center=c(350,350), rad=100) {
  cdfname = paste(annotation(x), "cdf", sep="")
  require(cdfname, character.only=TRUE, quietly=TRUE)
  xext = seq(center[1]-rad, center[1]+rad)
  yext = seq(center[2]-rad, center[2]+rad)
  badco = expand.grid(xext, yext)
  badco = badco[ (badco[,1] - center[1])^2 + (badco[,2]-center[2])^2 < rad^2, ]
  indsbad = apply(badco,1,function(x) xy2indices(x[1], x[2], 
    cdf=cdfname))
  return(as.numeric(exprs(x)[indsbad, chip]))
}

getRectRegion = function(x, chip=1, xinds = 251:350, yinds = 251:350) {
  cdfname = paste(annotation(x), "cdf", sep="")
  require(cdfname, character.only=TRUE, quietly=TRUE)
  badco = expand.grid(xinds, yinds)
  indsbad = apply(badco,1,function(x) xy2indices(x[1], x[2], 
    cdf=cdfname))
  return(as.numeric(exprs(x)[indsbad, chip]))
}
