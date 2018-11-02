outliers.by.subject <- function (data, response, subject, trim = 3) {
  #-------------------------------------------------------------------------------------
  # Copyright (C) 2017 Koscik, Timothy R. All Rights Reserved
  #-------------------------------------------------------------------------------------

  data0 <- data
  sdev = as.data.frame(tapply(data[, response], data[, subject], sd))
  sdev$Subject = row.names(sdev)
  colnames(sdev)[1] = "SD"
  row.names(sdev) = 1:length(sdev$SD)
  sdev$SD = as.numeric(sdev$SD)
  sdev$Subject = as.factor(sdev$Subject)
  row.names(sdev) = 1:length(sdev$SD)
  sdev = na.omit(sdev)
  sdev = sdev[, c(2, 1)]
  data = merge(data, sdev, by.x = subject, by.y = "Subject")

  m = as.data.frame(tapply(data[, response], data[, subject], mean))
  m$Subject = row.names(m)
  colnames(m)[1] = "Mean"
  row.names(m) = 1:length(m$Mean)
  m = m[, c(2, 1)]
  m = na.omit(m)
  data = merge(data, m, by.x = subject, by.y = "Subject")

  data$SD = as.numeric(data$SD)
  data$Mean = as.numeric(data$Mean)

  # data$SD[which(data$SD==0 & data$Mean==0)] <- 1

  Scaled = apply(data[, c(response, "SD", "Mean")], 1, function(row) (row[1] - row[3])/row[2])
  Scaled[which(is.na(Scaled))] <- 0

  data = cbind(data, Scaled)

  outliers = as.numeric(rownames(data[abs(data$Scaled) > trim, ]))
  if (length(outliers) != 0) {
    data = data[-outliers, , drop = TRUE]
  }

  outliers.ls <- numeric(nrow(data))
  outliers.ls[outliers] <- 1

  return(list(data = data[ , -seq(ncol(data)-2, ncol(data), 1)],
              data0 = data0,
              n.removed = nrow(data0) - nrow(data),
              percent.removed = (nrow(data0) - nrow(data))/nrow(data0) * 100,
              outliers.logic = outliers.ls))
}
