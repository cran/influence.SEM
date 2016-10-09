\name{Likedist}
\alias{Likedist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood Distance.
}
\description{
A general model-based measure of case influence on model fit is likelihood distance (Cook, 1977, 1986; Cook & Weisberg, 1982) defined as 

\deqn{LD_i=2[L(\hat{\mathbf{\theta}})-L(\hat{\mathbf{\theta}}_{(i)})]}

where \eqn{\hat{\mathbf{\theta}}} and \eqn{\hat{\mathbf{\theta}}_{(i)}} are the \eqn{k \times 1} vectors of estimated model parameters on the original and deleted \eqn{i} samples, respectively, where \eqn{i = 1, \ldots, N}. The subscript (\eqn{i}) indicates that the estimate was computed on the sample excluding case \eqn{i}. \eqn{L(\hat{\mathbf{\theta}})} and \eqn{L(\hat{\mathbf{\theta}}_{(i)})} are the log-likelihoods based on the original and the deleted \eqn{i} samples, respectively.
}
\usage{
Likedist(model, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A description of the user-specified model using the lavaan model syntax. See \code{\link{lavaan}} for more information. 
}
  \item{data}{
A data frame containing the observed variables used in the model. If any variables are declared as ordered factors, this function will treat them as ordinal variables.
}
  \item{\dots}{
Additional parameters for \code{\link{sem}} function.
}
}
\details{
 The log-likelihoods \eqn{L(\hat{\mathbf{\theta}})} and \eqn{L(\hat{\mathbf{\theta}}_{(i)})} are computed by the function \code{\link{bollen.loglik}} using the formula 4B2 described by Bollen (1989, pag. 135).
 
The likelihood distance gives the amount by which the log-likelihood of the full data changes if one were to evaluate it at the reduced-data estimates. The important point is that \eqn{L(\hat{\mathbf{\theta}}_{(i)})} is not the log-likelihood obtained by fitting the model to the reduced data set. It is obtained by evaluating the likelihood function based on the full data set (containing all \eqn{n} observations) at the reduced-data estimates (Schabenberger, 2005).}
\value{
Returns a vector of \eqn{LD_i}.
}
\references{
Bollen, K.A. (1989). \emph{Structural Equations with latent
Variables}. New York, NY: Wiley.

Cook, R.D. (1977). Detection of influential observations in linear regression. \emph{Technometrics}, 19, 15-18. 

Cook, R.D. (1986). Assessment of local influence. \emph{Journal of the Royal Statistical Society B}, 48, 133-169.

Cook, R.D., Weisberg, S. (1986). \emph{Residuals and influence in regressions.} New York, NY: Chapman & Hall.

Pek, J., MacCallum, R.C. (2011). Sensitivity Analysis in Structural Equation Models: Cases and Their Influence. \emph{Multivariate Behavioral Research}, 46, 202-228. 

Schabenberger, O. (2005). Mixed model influence diagnostics. In \emph{SUGI}, 29, 189-29. SAS institute Inc, Cary, NC. 
}
\author{
Massimiliano Pastore, Gianmarco Altoe'
}
\note{
If for observation \eqn{i} model does not converge or yelds a solution with negative estimated variances, the associated value of \eqn{LD_i} is set to \code{NA}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
 \code{\link{bollen.loglik}}
}
\examples{
## not run: this example take several minutes
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"
# fit0 <- sem(model, data=PDII)
# LD <-Likedist(model,data=PDII)
# plot(LD,pch=19,xlab="observations",ylab="Likelihood distances")

## not run: this example take several minutes
## an example in which the deletion of a case yelds a solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# LD <-Likedist(model,data=PDII)
# plot(LD,pch=19,xlab="observations",ylab="Likelihood distances")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
