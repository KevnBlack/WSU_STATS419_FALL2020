prepareMeasureData = function(measure,scale){
  # Cleaning: omit NA rows based measured values, not on $side
  measure = measure[complete.cases(measure[,4:26]),]
  
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
  
  # Scale data if scale = TRUE
  if(scale){
    measure[,4:26] <- scale(measure[,4:26])
    return(measure)
  } else{ # If false, return without scaling
    return(measure)
  }
}


read.file = function(path,scale){
  tryCatch(
    expr = {
      # Open cleaned file if already available
      measure = utils::read.csv(paste0(path.to.secret,"measure-clean.txt"),
                                header = TRUE, quote = "", sep = "|")
      return(measure)
    },
    warning = function(w){
      # If no clean file, open original file
      measure = utils::read.csv(paste0(path.to.secret,"measure-students.txt"),
                                header = TRUE, quote = "", sep = "|")
      
      measure = removeDuplicatesFromDataFrameAllColumns(measure)
      measure = prepareMeasureData(measure,scale)
      
      # Save cleaned data for later
      write.table(measure,paste0(path.to.secret,"measure-clean.txt"),sep="|",quote=FALSE)
      return(measure)
    }
  )    
}