shift.var <- function(df, to.shift, shift.by, shift.size) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------
  
  shift.idx <- 0:(nrow(df)-1)
  group.var <- c(1,diff(as.integer(unlist(df[shift.by]))))
  group.var <- group.var != 0
  shift.idx[which(group.var)] <- NA
  for (j in 1:length(shift.size)) {
    shift.temp <- unlist(df[to.shift])
    for (i in 1:shift.size[j]) {
      shift.temp <- shift.temp[shift.idx]
    }
    df[paste0(to.shift, ".shift", shift.size[j])] <- shift.temp
  }
  return(df)
}