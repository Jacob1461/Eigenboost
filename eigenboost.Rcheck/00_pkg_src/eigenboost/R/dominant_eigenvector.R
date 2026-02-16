domiant_eigenvector <- function(X, w){
  n_row <- nrow(X)
  ws <- w / sum(w)
  sampled_indices <- sample(1:n_row, size = n_row, replace = TRUE, prob = ws)
  X_new <- X[sampled_indices,] |> as.matrix()

  v <- eigen(cov(X_new))$vectors[,1]
  max_index <- which.max(abs(v))
  if (v[max_index] < 0) v <- -v


  e1 <- c(1, rep(0, ncol(X) - 1))
  u <- (v - e1) / sqrt(sum((v - e1)^2))
  diff_vec <- v - e1
  if (sqrt(sum(diff_vec^2)) < 1e-8) {
    u <- NULL
  } else {
    u <- diff_vec / sqrt(sum(diff_vec^2))

  }
  u
}
