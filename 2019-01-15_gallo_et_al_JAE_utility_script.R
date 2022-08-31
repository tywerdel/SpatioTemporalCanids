package_load<-function(packages = c("reshape2"), quiet=TRUE, verbose=FALSE, warn.conflicts=FALSE){
  
  # download required packages if they're not already
  pkgsToDownload<- packages[!(packages  %in% installed.packages()[,"Package"])]
  if(length(pkgsToDownload)>0)
    install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", quiet=quiet, verbose=verbose)
  
  # then load them
  for(i in 1:length(packages))
    require(packages[i], character.only=T, quietly=quiet, warn.conflicts=warn.conflicts)
}

# Change the dataframe back to an array
df_2_array <- function(my_df = NULL){
  
  require(reshape2)
  my_array <- acast(my_df, species~site~season, value.var = "count")
  dimnames(my_array) <- NULL
  return(my_array)
}

# A modified version of the overlap funciton in package overlap
# Calculates density estimates but returns dataframe instead of auto plotting results
overlap_mod <- function (A, B, xscale = 24, xcenter = c("noon", "midnight"), 
                         linetype = c(1,2), linecol = c("black", "blue"),
                         linewidth = c(1,1), olapcol = "lightgrey", rug = FALSE, 
                         extend = NULL, n.grid = 128, kmax = 3, adjust = 1, ...) 
{
  isMidnt <- match.arg(xcenter) == "midnight"
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale)) 
    1
  else xscale/(2 * pi)
  if (is.null(extend)) {
    xxRad <- seq(0, 2 * pi, length = n.grid)
  }
  else {
    xxRad <- seq(-pi/4, 9 * pi/4, length = n.grid)
  }
  if (isMidnt) 
    xxRad <- xxRad - pi
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  
  return(data.frame(x = xx, densityA = densA, densityB = densB))
}
