decompose.var <- function(df, var.name, grouping=c("grand", "subject", "subject.run")) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------

  tnames <- row.names(df)
  for (i in 1:length(grouping)) {
    if (grouping[i] == "grand") {
      assign(paste0("m", i), rep(mean(unlist(df[var.name]), na.rm=TRUE), nrow(df)))
    } else {
      id.levels <- unlist(unique(df[grouping[i]]))
      m.temp <- numeric(nrow(df))
      for (j in 1:length(id.levels)) {
        tf <-  df[unlist(df[grouping[i]]) == id.levels[j], ]
        m.temp[unlist(df[grouping[i]]) == id.levels[j]] <- rep(mean(unlist(tf[var.name]), na.rm=TRUE), nrow(tf))
      }
      assign(paste0("m", i), m.temp)
    }
  }

  if (length(grouping) > 1) {
    for (i in 1:(length(grouping)-1)) {
      assign(paste0(var.name, ".", grouping[i+1], ".c"),
             eval(parse(text=paste0("m", i+1, "- m", i))))
    }
  }

  assign(paste0(var.name, ".trial.c"),
         eval(parse(text=paste0("unlist(df[var.name])- m", length(grouping)))))

  t.names <- c(grouping[-1], "trial")
  for (i in 1:length(grouping)) {
    df <- eval(parse(text=paste0("cbind(df, ", var.name, ".", t.names[i], ".c)")))
  }
  rownames(df) <- tnames

  return(df)
}
