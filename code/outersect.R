#https://www.r-bloggers.com/outersect-the-opposite-of-rs-intersect-function/

outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}
