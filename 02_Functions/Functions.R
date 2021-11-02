
rangos_edades <- function(vec){
  
  vector <- c()
  
for(i in 1:length(vec) ) {
  if (vec[[i]] >= 18 & vec[[i]] <=29){
    vector[[i]] <- c(vector, "18-29")
  } else if (vec[[i]] >= 30 & vec[[i]] <=39){
    vector[[i]] <- c(vector, "30-39")
  } else if (vec[[i]] >= 40 & vec[[i]] <=49){
    vector[[i]] <- c(vector, "40-49")
  } else if (vec[[i]] >= 50 & vec[[i]] <=59){
    vector[[i]] <- c(vector, "50-59")
  } else if (vec[[i]] >= 60 & vec[[i]] <=69){
    vector[[i]] <- c(vector, "60-69")
  } else if (vec[[i]] >= 70){
    vector[[i]] <- c(vector, "+70")
  } 
}
  print(paste(vector))
}
