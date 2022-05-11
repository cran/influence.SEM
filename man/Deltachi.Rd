\name{Deltachi}
\alias{Deltachi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chi-square difference.
}
\description{
Quantifies case influence on overall model fit by change in the test statistic 

\deqn{\Delta_{\chi^2_i}=\chi^2-\chi^2_{(i)}}

where \eqn{\chi^2} and \eqn{\chi^2_{(i)}} are the test statistics obtained from original and deleted \eqn{i} samples.
}
\usage{
Deltachi(model, data, ..., scaled = FALSE)
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
  \item{scaled}{
Logical, if \code{TRUE} the function uses the scaled \eqn{\chi^2} (Rosseel, 2013).
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a vector of \eqn{\Delta_{\chi^2_i}}.
}
\references{
Pek, J., MacCallum, R.C. (2011). Sensitivity Analysis in Structural Equation Models: Cases and Their Influence. \emph{Multivariate Behavioral Research}, 46, 202-228.

Rosseel, Y. (2012). lavaan: An R Package for Structural Equation
  Modeling. \emph{Journal of Statistical Software}, 48, 1-36.

Rosseel, Y. (2022). The \code{lavaan} tutorial. URL: \url{https://lavaan.ugent.be/tutorial/}.
}
\author{
Massimiliano Pastore
}
\note{
If for observation \eqn{i} model does not converge or yelds a solution with negative estimated variances, the associated value of \eqn{\Delta_{\chi^2_i}} is set to \code{NA}. 

This function is a particular case of \code{\link{fitinfluence}}, see example below.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~s
%}
\examples{
## not run: this example take several minutes
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"

# fit0 <- sem(model, data=PDII)
# Dchi <- Deltachi(model,data=PDII)
# plot(Dchi,pch=19,xlab="observations",ylab="Delta chisquare")

## not run: this example take several minutes
## an example in which the deletion of a case yelds a solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# Dchi <- Deltachi(model,data=PDII)
# plot(Dchi,pch=19,xlab="observations",ylab="Delta chisquare",main="Deltachi function")

## the case that produces negative estimated variances
# sem(model,data=PDII[-which(is.na(Dchi)),])

## same results 
# Dchi <- fitinfluence("chisq",model,data=PDII)$Dind$chisq
# plot(Dchi,pch=19,xlab="observations",ylab="Delta chisquare",main="fitinfluence function")

}
\keyword{multivariate}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
