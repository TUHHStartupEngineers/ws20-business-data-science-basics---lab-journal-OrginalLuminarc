roll2 <- function(faces = 1:6, size = 2) {
  probabilities_vector <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/2)
  dice <- sample(faces, size, replace = TRUE, prob = probabilities_vector)
  sum(dice)
}