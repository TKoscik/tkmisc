power.pro <- function (df, var.adj, var.pro,
                       a.start = 1, b.start = 1,
                       restart.ab = TRUE, max.restart=1000, if.fail=c(1,0),
                       rescale = TRUE,
                       save.params = FALSE) {

  #check inputs
  n.adj <- length(var.adj)
  if (length(var.pro) == 1) {
    var.pro <- rep(var.pro, n.adj)
  } else if (length(var.pro) != n.adj) {
    stop("Length of the vector specifying var.pro must either be a single value or match the length of var.adj")
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
  iter <- rep(0, n.adj)
  
  #run power proportion models
  pwr.params <- vector("list", n.adj)
  for (i in 1:n.adj) {
    FORM <- formula(sprintf("%s ~ a * %s ^ b", var.adj[i], var.pro[i]))
    pwr.params[[i]] <- tryCatch({
      pwr.mdl <- nls(FORM, data = df, start = list(a = a.start[i], b = b.start[i]), 
                     control = nls.control(maxiter = 1000))
      summary(pwr.mdl)$coef[1:2, 1]
    }, error = function(e) {
      if (restart.ab) {
        warning("[POWER.PRO] non-linear model failed with current parameters, randomizing a and b.")
      } else {
        warning("[POWER.PRO] non-linear model failed with current parameters. Try altering initial values of a and/or b.")
      }
      return(c(NA,NA))
    })
    
    if (restart.ab) {
      while (is.na(pwr.params[[i]][1])) {
        iter[i] <- iter[i] + 1
        if (iter[i] > max.restart) {
          pwr.params[[i]] <- if.fail
        } else {
          pwr.params[[i]] <- tryCatch({
            a.start[i] <- runif(1,0,2)
            b.start[i] <- runif(1,0,2)
            pwr.mdl <- nls(FORM, data = df, start = list(a = a.start[i], b = b.start[i]), 
                           control = nls.control(maxiter = 1000))
            summary(pwr.mdl)$coef[1:2, 1]
          }, error = function(e) {
            warning("[POWER.PRO] non-linear model failed with current parameters, randomizing a and b.")
            return(c(NA,NA))
          })
        }
      }
    } else {
      if (is.na(pwr.params[[i]][1])) {
        pwr.params[[i]] <- if.fail
      }
    }
  }
  
  for (i in 1:n.adj) {
    if (!is.na(pwr.params[[i]][2])) {
      orig <- df[, var.adj[i]]
      df[, var.adj[i]] <- orig/(df[, var.pro[i]]^pwr.params[[i]][2])
      if (rescale) {
        df[, var.adj[i]] <- ((df[, var.adj[i]] - mean(df[, var.adj[i]], na.rm=T))/sd(df[, var.adj[i]], na.rm=T)) * sd(orig, na.rm=T) + mean(orig, na.rm=T)
      }
    }
  }
  
  if (save.params) {
    output <- list()
    output$df <- df
    output$a <- unlist(pwr.params)[seq(1,2*n.adj,2)]
    names(output$a) <- var.adj
    output$a.start <- a.start
    names(output$a.start) <- var.adj
    output$b <- unlist(pwr.params)[seq(2,2*n.adj,2)]
    names(output$b) <- var.adj
    output$b.start <- b.start
    names(output$b.start) <- var.adj
    output$iter <- iter
    names(output$iter) <- var.adj
  }
  else {
    output <- df
  }
  return(output)
}
