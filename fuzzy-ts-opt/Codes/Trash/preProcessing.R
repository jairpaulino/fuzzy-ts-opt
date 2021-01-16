# Normalização de dados
normalize = function(array, x, y){
  #Normalize to [0, 1]
  m = min(array) 
  range = max(array) - m
  norm1 = (array - m) / range
  
  #Then scale to [x,y]
  range2 = y - x
  normalized = (norm1*range2) + x
  return(normalized)
}


