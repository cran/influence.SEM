\name{fitinfluence}
\alias{fitinfluence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Case influence on model fit. 
}
\description{
This function evaluate the case's effect on a user-defined fit index. 
}
\usage{
fitinfluence(index, model, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{
A model fit index. 
}
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
For each case evaluate the influence on one or more fit indices: the difference between the chosen fit index calculated for the SEM target model \eqn{M} and the same index computed for the SEM model \eqn{M_{(i)}} excluding case \eqn{i}.
}
\value{
Returns a list: 
 \item{Dind}{a data.frame of case influence.}
 \item{Oind}{observed fit indices.}
}
\references{
Pek, J., MacCallum, R.C. (2011). Sensitivity Analysis in Structural Equation Models: Cases and Their Influence. \emph{Multivariate Behavioral Research}, 46, 202-228. 
}
\author{
Massimiliano Pastore
}
\note{
If for observation \eqn{i} model does not converge or yelds a solution with negative estimated variances, the associated value of influence is set to \code{NA}. 
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

## fit0 <- sem(model, data=PDII)
## FI <- fitinfluence("cfi",model,data=PDII)
## plot(FI$Dind,pch=19)

## not run: this example take several minutes
## an example in which the deletion of a case yelds a solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

## fit0 <- sem(model, data=PDII)
## FI <- fitinfluence(c("tli","rmsea"),model,PDII)
## explore.influence(FI$Dind$tli)
## explore.influence(FI$Dind$rmsea)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
