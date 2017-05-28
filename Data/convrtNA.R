# In some features, NA is not missing data, but a level.
# Such NA need to be converted to a non-NA level for assigning dummy variables

ConvrtNA <- function(x,newLvlName) {
  # x is a categorical feature
  # add t

  levels(x) <- c(levels(x),newLvlName)
  x[is.na(x)] <- newLvlName
}