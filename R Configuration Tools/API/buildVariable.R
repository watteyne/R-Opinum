buildVariable <- function(targetVariable,inputVariables,inputConstants,inputForms,formula){
  
  calculatedVariableFormulas <- list()
  if(is.null(targetVariable$validFrom) || is.null(targetVariable$validUntil)){
    calculatedVariableFormulas <- data.frame(formula = formula)
  }
  else{
    calculatedVariableFormulas <- data.frame(formula = formula,
                                             validFrom = targetVariable$validFrom,
                                             validUntil = targetVariable$validUntil)
  }
  
  calculatedVariableFormulas$variables <- inputVariables
  calculatedVariableFormulas$entities <- inputForms
  calculatedVariableFormulas$constants <- inputConstants
  
  
  calculated <- list(id = {if(targetVariable$id != 0){targetVariable$id}else{NA}},
                     friendlyName = targetVariable$name,
                     calculatedVariableFormulas = calculatedVariableFormulas)
  
  list(calculated = calculated,
       id = {if(targetVariable$id != 0){targetVariable$id}else{NA}},
       name = targetVariable$name,
       sourceId = targetVariable$sourceId,
       unitId = targetVariable$unitId,
       granularity = targetVariable$granularity,
       granularityTimeBase = targetVariable$granularityTimeBase,
       quantityType = targetVariable$quantityType,
       aggregate = targetVariable$aggregate)
}