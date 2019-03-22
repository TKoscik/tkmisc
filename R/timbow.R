timbow <- function(n=10, reverse=F) {
  x <- colorRampPalette(colors=colorRampPalette(c(viridis(5), viridis(5, begin=0.5, direction = -1, option="plasma")[-1]))
  output <- x(n)
  if (reverse) {
    output <- rev(output)
  }
}
