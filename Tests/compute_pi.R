compute_pi <- function(x, y, grid = sort(unique(c(x, y)))) {
  Fn <- ecdf(x)
  Gm <- ecdf(y)
  diffs <- sapply(grid, function(u) Gm(u) - Fn(u))
  return(max(diffs))
}