prepareMeasureData = function(measure){
  # Cleaning: omit NA values
  measure = na.omit(measure)
  
  # Cleaning: consistent naming convention
  measure$gender = factor(tolower(measure$gender))
  measure$gender[measure$gender=="f"] = "female"
  measure$gender[measure$gender=="m"] = "male"
  measure$units = factor(tolower(measure$units))
  measure$units[measure$units=="inches"] = "in"
  measure$units[measure$units=="inch"] = "in"
  measure$units[measure$units=="\"in\""] = "in"
  measure$units[measure$units=="cm"] = "in"
  
  # Converting cm to inches
  for(row in 1:nrow(measure)){
    if(measure[row,]$units=="cm"){
      measure[row,4:26] <- measure[row,4:26]/2.54
    }
  }
  
  # Scale data
  measure[,4:26] <- scale(measure[,4:26])
  
  return(measure)
}