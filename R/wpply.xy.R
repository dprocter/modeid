wapply.xy <- function(x, y, width, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width

  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))

  OUT <- lapply(SEQ2, function(a) FUN(x[a],y[a], ...))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}