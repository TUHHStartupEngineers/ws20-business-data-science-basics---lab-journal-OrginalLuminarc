roll2 <- function(faces = 1:6, size = 2) {
  dice <- sample(faces, size, replace = TRUE)
  sum(dice)
}