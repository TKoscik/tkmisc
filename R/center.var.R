center.var <- function(df, to.scale, scale.by) {
  group.var <- unlist(df[scale.by])
  groups <- unlist(unique(df[scale.by]))
  ctr <- numeric(nrow(df))
  m <- numeric(nrow(df))
  for (i in 1:length(groups)) {
    group.idx <- which(group.var==groups[i])
    tf <- df[group.idx, ]
    m.temp <- mean(unlist(tf[to.scale]))
    ctr[group.idx] <- (unname(unlist(tf[to.scale])-m.temp))
    m[group.idx] <- rep(m.temp, nrow(tf))
    #ctr <- c(ctr, (unname(unlist(tf[to.scale])-m.temp)))
    #m <- c(m, rep(m.temp, nrow(tf)))
  }
  df[paste0(to.scale, ".ctr")] <- ctr
  df[paste0(to.scale, ".mean")] <- m
  return(df)
}
