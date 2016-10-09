## formula Bollen, 1989 4B.2 page 135
bollen.loglik <- function(N,S,Sigma) {
  Nstar <- N-1
  bll <- -Nstar/2 * (log(det(Sigma)) + sum(diag(S%*%solve(Sigma))))
  return(bll)
}
