
rangos_edades <- function (colm) {
  
  re <- c()
  
  for (i in 1:length(colm)) {
    
    if (colm[[i]] >= 18 & colm[[i]] <= 29) {
      re <- c(re, "18-29")
    } else if (colm[[i]] >= 30 & colm[[i]] <= 39) {
      re <- c(re, "30-39")
    } else if (colm[[i]] >= 40 & colm[[i]] <= 49) {
      re <- c(re, "40-49")
    } else if (colm[[i]] >= 50 & colm[[i]] <= 59) {
      re <- c(re, "50-59")
    } else if (colm[[i]] >= 60 & colm[[i]] <= 69) {
      re <- c(re, "60-69")
    } else if (colm[[i]] >= 70) {
      re <- c(re, "70+")
    }
  }
  
  re <- as.vector(re)
  
  return(re)
  
}



