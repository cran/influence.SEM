\name{bollen.loglik}
\alias{bollen.loglik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-Likelihood of a sem model (Internal function).
}
\description{
Internal function, called by \code{\link{Likedist}}. 
}
\usage{
bollen.loglik(N, S, Sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{Sample size.
}
  \item{S}{
Observed covariance matrix.
}
  \item{Sigma}{
Model fitted covariance matrix, \eqn{\Sigma(\theta)}. 
}
}
\details{
 The log-likelihood is computed by the function \code{\link{bollen.loglik}} using the formula 4B2 described by Bollen (1989, pag. 135).
}
\value{
Returns the Log-likelihood.
}
\references{
Bollen, K.A. (1989). \emph{Structural Equations with latent
Variables}. New York, NY: Wiley.
}
\author{
Massimiliano Pastore, Gianmarco Altoe'
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
 \code{\link{Likedist}}
}
\examples{
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"
fit0 <- sem(model, data=PDII)
N <- fit0@Data@nobs[[1]]
S <- fit0@SampleStats@cov[[1]]
Sigma <- fitted(fit0)$cov
bollen.loglik(N,S,Sigma)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utility}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
