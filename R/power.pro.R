power.pro <- function (df, var.adj, var.pro,
                       a.start = 1, b.start = 1, save.params = FALSE) {
  #check inputs
  n.adj <- length(var.adj)
  if (length(var.pro) == 1) {
    var.pro <- rep(var.pro, n.adj)
  } else if (length(var.pro) != n.adj) {
    stop("Length of the vecetor specifying var.pro must either be a single value or match the length of var.adj")
  }
  
  if (length(a.start) == 1) {
    a.start <- rep(a.start, n.adj)
  } else if (length(a.start) != n.adj) {
    stop("Length of the vector specifying a.start must either be a single value or match the length of var.adj")
  }
  
  if (length(b.start) == 1) {
    b.start <- rep(b.start, n.adj)
  } else if (length(b.start) != n.adj) {
    stop("Length of the vecetor specifying b.start must either be a single value or match the length of var.adj")
  }
  
  #run power proportion models
  pwr.params <- vector("list", n.adj)
  for (i in 1:n.adj) {
    FORM <- formula(sprintf("%s ~ a * %s ^ b", var.adj[i], var.pro[i]))
    pwr.params[[i]] <- tryCatch({
      pwr.mdl <- nls(FORM, data = df, start = list(a = a.start[i], b = b.start[i]), 
                     control = nls.control(maxiter = 1000))
      return(summary(pwr.mdl)$coef[1:2, 1])
    }, error = function(e) {
      warning("[POWER.PRO] non-linear model did not work with current parameters. Try altering initial values of a and/or b.")
      return(c(NA,NA))
    })
  }
  
  for (i in 1:n.adj) {
    if (!is.na(pwr.params[[i]][2])) {
      df[, var.adj[i]] <- df[, var.adj[i]]/(df[, var.pro[i]]^pwr.params[[i]][2])
    }
  }
  
  if (save.params) {
    output <- list()
    output$df <- df
    output$a <- unlist(pwr.params)[1,2*n.adjust,2]
    output$b <- unlist(pwr.params)[2,2*n.adjust,2]
  }
  else {
    output <- df
  }
  return(output)
}
