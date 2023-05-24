#install.packages("R6")
library(R6)

StateObject <- R6Class("StateObject",
                       private  = list(
                         stateName=NULL,
                         groupByName=NULL,
                         firstAnalyzedValue=NULL,
                         secondAnalyzedValue=NULL,
                         thirdAnalyzedValue=NULL,
                         fourthAnalyzedValue=NULL,
                         isReadyToUse= NA),
                       
                        public = list(
                        
                         initialize = function(stateName, groupByName, firstAnalyzedValue, secondAnalyzedValue, 
                                               thirdAnalyzedValue, fourthAnalyzedValue ){
                         #validity checking 
                         stopifnot(is.character(stateName), is.character(groupByName))   

                         private$stateName = stateName
                         private$groupByName = groupByName
                         private$firstAnalyzedValue = firstAnalyzedValue
                         private$secondAnalyzedValue = secondAnalyzedValue
                         private$thirdAnalyzedValue = thirdAnalyzedValue
                         private$fourthAnalyzedValue = fourthAnalyzedValue
                         
                         if (is.null(firstAnalyzedValue) & is.null(secondAnalyzedValue) & is.null(thirdAnalyzedValue) & is.null(fourthAnalyzedValue))
                         {
                           private$isReadyToUse = FALSE
                         }
                         else
                         {
                           private$isReadyToUse = TRUE
                         }
                         
                         
                         # invisible returning of the object we've created
                         invisible(self)
                         },
                         
                         #Functions that return values of calculated measures. We can also specify measure name
                         #to display it further with associated value.
                         
                         returnFirstMeasure = function(measureName){
                           return(c(measureName, private$firstAnalyzedValue)) #return measure with its name
                         },
                         returnSecondMeasure = function(measureName){
                           return(c(measureName, private$secondAnalyzedValue)) #return measure with its name
                         },
                         returnThirdMeasure = function(measureName){
                           return(c(measureName, private$thirdAnalyzedValue)) #return measure with its name
                         },
                         returnFourthMeasure = function(measureName){
                           return(c(measureName, private$fourthAnalyzedValue)) #return measure with its name
                         }
                       )
  
)


#TESTs
MS <- StateObject$new('Missisipi','Gun crime',3,2,3,2)
TE <- StateObject$new('Texas','Gun crime',NULL,NULL,NULL,NULL)
print(MS)
print(TE)

MS$returnFirstMeasure('Mean assaults in Missisipi')
