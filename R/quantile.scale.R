quantile.scale <- function(df, var.name, center=TRUE, prob=0.9) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  for (i in 1:length(var.name)){
    
    if (center) {
      df[var.name[i]] <- unlist(df[var.name[i]]) - mean(unlist(df[var.name[i]]), na.rm=TRUE)
    } else {
      df[var.name[i]] <- df[var.name[i]]
    }
    
    df[var.name[i]] <- unlist(df[var.name[i]]) / quantile(unlist(df[var.name[i]]), prob=prob)
  }
  return(df)
}