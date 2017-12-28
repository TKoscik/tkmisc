good.color <- function(type = c("cool", "warm", "neon", "neon-bright"), n=21) {
  
  if (length(type) == 3) { type <- "neon-bright" }
  
  if (type == "cool") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#dcedc8"))(6),
        colorRampPalette(c("#dcedc8", "#42b3d5"))(5)[-1],
        colorRampPalette(c("#42b3d5", "#1a237e"))(7)[-1],
        colorRampPalette(c("#1a237e", "#ffffff"))(6)[-1]))(n)
  } else if (type == "warm") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#feeb65"))(7),
        colorRampPalette(c("#feeb65", "#e4521b"))(8)[-1],
        colorRampPalette(c("#e4521b", "#4d342f"))(5)[-1],
        colorRampPalette(c("#4d342f", "#ffffff"))(4)[-1]))(n)
  } else if (type == "neon") {
    colors <- colorRampPalette(
      c(colorRampPalette(c("#000000", "#ffecb3"))(5),
        colorRampPalette(c("#ffecb3", "#e85285"))(6)[-1],
        colorRampPalette(c("#e85285", "#6a1b9a"))(5)[-1],
        colorRampPalette(c("#6a1b9a", "#ffffff"))(8)[-1]))(n)
  } else if (type == "neon-bright") {
    colors <- colorRampPalette("#000000", "#0000ff", "#ff00ff", "#ffff00", "#ffffff")(n)
  }
  return(colors)
}
