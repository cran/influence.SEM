\name{genCookDist}
\alias{genCookDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Cook Distance.
}
\description{
Case influence on a vector of parameters may be quantified by generalized Cook's Distance (\eqn{gCD}; Cook 1977, 1986):

\deqn{gCD_i=(\hat{\mathbf{\theta}}-\hat{\mathbf{\theta}}_{(i)})' _a\hat{\mathbf{\Sigma}}(\hat{\mathbf{\theta}}_{(i)})^{-1} (\hat{\mathbf{\theta}}-\hat{\mathbf{\theta}}_{(i)})}

where \eqn{\hat{\mathbf{\theta}}} and \eqn{\hat{\mathbf{\theta}}_{(i)}} are \eqn{l \times 1} vectors of parameter estimates obained from the original and delete \eqn{i} samples, and \eqn{_a\hat{\mathbf{\Sigma}}(\hat{\mathbf{\theta}}_{(i)})} is the estimated asymptotic covariance matrix of the parameter estimates obtained from reduced sample.
}
\usage{
genCookDist(model, data, ...)
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
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a vector of \eqn{gCD_i}.
}
\references{
Cook, R.D. (1977). Detection of influential observations in linear regression. \emph{Technometrics}, 19, 15-18. 

Cook, R.D. (1986). Assessment of local influence. \emph{Journal of the Royal Statistical Society B}, 48, 133-169.

Pek, J., MacCallum, R.C. (2011). Sensitivity Analysis in Structural Equation Models: Cases and Their Influence. \emph{Multivariate Behavioral Research}, 46, 202-228. 
}
\author{
Massimiliano Pastore
}
\note{
If for observation \eqn{i} model does not converge or yelds a solution with negative estimated variances, the associated value of \eqn{gCD_i} is set to \code{NA}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## not run: this example take several minutes
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"
# fit0 <- sem(model, data=PDII)
# gCD <- genCookDist(model,data=PDII)
# plot(gCD,pch=19,xlab="observations",ylab="Cook distance")

## not run: this example take several minutes
## an example in which the deletion of a case produces solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# gCD <- genCookDist(model,data=PDII)
# plot(gCD,pch=19,xlab="observations",ylab="Cook distance")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
