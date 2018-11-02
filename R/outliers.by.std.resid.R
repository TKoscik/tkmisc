outliers.by.std.resid <- function (model, data, trim=2.5) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------

  if (missing(data)) {
    data <- switch(class(model)[1],
                   `merModLmerTest`=model@frame,
                   `glmerMod`=model@frame,
                   `brmsfit`=model$data,
                   `lm`=model$x,
                   model.matrix(model))
  }
  data$rstand <- as.vector(scale(resid(model)))
  row.names(data) <- 1:nrow(data)
  data0 <- data
  outliers <- as.numeric(row.names(data[abs(data$rstand) > trim, ]))
  if (length(outliers) > 0) {
    data <- data[-outliers, , drop=TRUE]
  }

  outliers.ls <- numeric(nrow(data0))
  outliers.ls[outliers] <- 1

  return(list(data = data[ , -(ncol(data))],
              data0 = data0,
              n.removed = nrow(data0) - nrow(data),
              percent.removed = (nrow(data0) - nrow(data))/nrow(data0) * 100,
              outliers.logic = outliers.ls))
}
