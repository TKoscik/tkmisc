good.color <- function(type = c("cool", "warm", "neon", "neon-bright"), n=21) {
  
  if (length(type) == 3) { type <- "neon-bright" }
  
  if (type == "cool") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#1a237e"))(6),
        colorRampPalette(c("#1a237e", "#42b3d5"))(7)[-1],
        colorRampPalette(c("#42b3d5", "#dcedc8"))(5)[-1],
        colorRampPalette(c("#dcedc8", "#ffffff"))(6)[-1]))(n)
  } else if (type == "warm") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#4d342f"))(4),
        colorRampPalette(c("#4d342f", "#e4521b"))(5)[-1],
        colorRampPalette(c("#e4521b", "#feeb65"))(8)[-1],
        colorRampPalette(c("#feeb65", "#ffffff"))(7)[-1]))(n)
  } else if (type == "neon") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#6a1b9a"))(8),
        colorRampPalette(c("#6a1b9a", "#e85285"))(5)[-1],
        colorRampPalette(c("#e85285", "#ffecb3"))(6)[-1],
        colorRampPalette(c("#ffecb3", "#ffffff"))(5)[-1]))(n)
  } else if (type == "neon-bright") {
    colors <- colorRampPalette("#000000", "#0000ff", "#ff00ff", "#ffff00", "#ffffff")(n)
  }
  return(colors)
}
