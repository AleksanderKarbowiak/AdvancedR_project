#Analitycal package
library(dplyr)

#returns list of varType = char/num datasets variables names
variablesNames <- function(dataset, varType){
  if(is.numeric(varType)){
    stop('Argument varType is not a character')
  }
  if(varType == 'char'){
    categorical_var_names <- sapply(dataset[,], is.character) %>% which() %>% names()
    
    return(categorical_var_names)
  }
  else if(varType == 'num'){
    numeric_var_names <- sapply(dataset, is.numeric) %>% which() %>% names()
    
    return(numeric_var_names)
  }
  else {
    stop('Argument varType has 2 valid values - num or char')
  }
}

#TIP <<- double assignment arrow creates variable also outside the function

#converts character values from dataset to factors and returns the converted dataset
factorCatVars <- function(dataset){
  
  categorical_var_names <- variablesNames(dataset,'char')
  
  for (variable in categorical_var_names) {
    dataset[[variable]] <- as.factor(dataset[[variable]])
  }
  
  return(dataset)
  
}

#returns the number of numeric variables unqiue values from dataset  
getUniqueNumValues <- function(dataset){
  
  numeric_var_names <- variablesNames(dataset,'num')
  
  uniqueValues <- sapply(dataset[, numeric_var_names], 
                         function(x) 
                           unique(x) %>% 
                           length()) %>% 
    #sort variables by increasing 
    # number of levels in the end
    sort()
  
  #Convert Named Vector to Dataframe
  df <- data.frame(as.list(uniqueValues))
  
  return(df)
}

#returns levels and frequency of variable in dataset
getVarLevels <- function(dataset, varName){
  if(is.character(varName) & varName %in% names(dataset)){
    return(as.data.frame(table(dataset[[varName]])))
  }
  else{
    stop("The dataset does not contain that variable")
  }
}

#!!!!!!!!!!!! MOZE ZROBIC W CPP !!!!!!!!!!!!!!!
#returns number of missing values per column 
countMissingValues <- function(dataset){
  return(colSums(is.na(dataset)) %>% sort())
}

#returns dataset with median imputed missing values for given numeric variable
medianImput <- function(dataset, varName){
  if(is.character(varName) & varName %in% names(dataset) & is.numeric(dataset[,varName])){
    
    median_value <- median(dataset[,varName], na.rm = TRUE)
    dataset[is.na(dataset[,varName])] <- median_value
    
    return(dataset)
  }
  else{
    stop("The dataset does not contain that variable")
  }
}

#calculates mode for given variable
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#returns dataset with mode imputed missing values for given character/factor variable
modeImput <- function(dataset, varName){
  if(is.character(varName) & varName %in% names(dataset) & (is.character(dataset[,varName]) | is.factor(dataset[,varName]))){
    
    mode_value <- getmode(dataset[,varName])
    dataset[is.na(dataset[,varName])] <- mode_value
    
    return(dataset)
  }
  else{
    stop("The dataset does not contain that variable")
  }
}

