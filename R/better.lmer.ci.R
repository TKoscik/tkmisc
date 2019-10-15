better.lmer.ci <- function(model, which.effect, xlevels) {
  tempf <- model@frame
  dv <- unlist(strsplit(as.character(model@call), split = "[ ~]"))[2]
  tempf[paste0(dv, ".dummy")] <- tempf[dv]
  temp.ranef <- ranef(model)
  for (j in 1:length(temp.ranef)) {
    which.vars <- names(temp.ranef)[[j]]
    which.vars <- unlist(strsplit(which.vars, split = ":"))
    tempf[paste0("dummy", j)] <- do.call(paste, c(tempf[which.vars], sep = ":"))
    tempf[paste0(dv, ".dummy")] <- tempf[paste0(dv, ".dummy")] - 
      unlist(temp.ranef[[j]])[as.numeric(factor(unlist(tempf[paste0("dummy",j)]),
                                                levels = unique(unlist(tempf[paste0("dummy", j)]))))]
  }
  tempf[dv] <- tempf[paste0(dv, ".dummy")]
  model <<- update(model, . ~ ., data = tempf)
  ef <- as.data.frame(effect(which.effect, model, xlevels=xlevels))
  return(ef)
}
