center.var <- function(df, to.scale, scale.by) {
  group.var <- unlist(df[scale.by])
  groups <- unlist(unique(df[scale.by]))
  ctr <- numeric()
  m <- numeric()
  for (i in 1:length(groups)) {
    tf <- df[group.var==groups[i], ]
    m.temp <- mean(unlist(tf[to.scale]))
    ctr <- c(ctr, (unname(unlist(tf[to.scale])-m.temp)))
    m <- c(m, rep(m.temp, nrow(tf)))
  }
  df[paste0(to.scale, ".ctr")] <- ctr
  df[paste0(to.scale, ".mean")] <- m
  return(df)
}
