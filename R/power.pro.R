power.pro <- function(df, var.adj, var.pro,
                      a.start=1, b.start=1,
                      save.params=FALSE) {
  
  n.adj <- length(var.adj)
  if (length(var.pro) == 1) {
    var.pro <- rep(var.pro, n.adj)
  }  else if (length(var.pro) != n.adj ) {
    stop("Length of the vecetor specifying var.pro must either be a single value or match the length of var.adj")
  }
  
  a <- numeric(n.adj)
  b <- numeric(n.adj)
  for (i in 1:n.adj) {
    FORM <- formula(sprintf("%s ~ a * %s ^ b", var.adj[i], var.pro[i]))
    pwr.mdl <- tryCatch({
      nls(FORM, data=df, start=list(a=1, b=1), control=nls.control(maxiter=1000))
      a <- summary(pwr.mdl)$coef[1,1]
      b <- summary(pwr.mdl)$coef[2,1]
      df[ ,var.adj[i]] <- df[ ,var.adj[i]] / (df[ ,var.pro[i]] ^ b)
    }, error = function(e) {
      warning("[POWER.PRO] non-linear model did not work with current parameters. Try altering initial values of a and/or b.")
      a <- NA
      b <- NA
    })
  }
  
  
  if (save.params) {
    output <- list()
    output$df <- df
    output$a <- a
    output$b <- b
  } else {
    output <- df
  }
  
  return(output)
}
