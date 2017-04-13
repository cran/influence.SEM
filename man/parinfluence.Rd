\name{parinfluence}
\alias{parinfluence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Case influence on model parameters.
}
\description{
Computes direction of change in parameter estimates with \deqn{\Delta \hat{\theta}_{ji}=\frac{\hat{\theta}_j-\hat{\theta}_{j(i)}}{[VAR(\hat{\theta}_{j(i)})]^{1/2}}}
where \eqn{\hat{\theta}_j} and \eqn{\hat{\theta}_{j(i)}} are the parameter estimates obtained from original and deleted \eqn{i} samples.
}
\usage{
parinfluence(parm, model, data, cook = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parm}{
Single parameter or vector of parameters. 
}
  \item{model}{
A description of the user-specified model using the lavaan model syntax. See \code{\link{lavaan}} for more information. 
}
  \item{data}{
A data frame containing the observed variables used in the model. If any variables are declared as ordered factors, this function will treat them as ordinal variables.
}
  \item{cook}{
Logical, if \code{TRUE} returns generalized Cook's Distance computed as \eqn{[\Delta \hat{\theta}_{ji}]^2}.   
}
  \item{\dots}{
Additional parameters for \code{\link{sem}} function.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a list: 
\item{gCD }{Generalized Cook's Distance, if \code{cook=TRUE}.}
\item{Dparm }{Direction of change in parameter estimates.}
\item{THi }{Parameter estimates.}
}
\references{
Pek, J., MacCallum, R.C. (2011). Sensitivity Analysis in Structural Equation Models: Cases and Their Influence. \emph{Multivariate Behavioral Research}, 46, 202-228. 
}
\author{
Massimiliano Pastore
}
\note{
If for observation \eqn{i} model does not converge or yelds a solution with negative estimated variances or NA parameter values, the associated values of \eqn{\Delta \hat{\theta}_{ji}} are set to \code{NA}. 
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
# PAR <- c("F1=~y2","F1=~y3","F1=~y4")
# LY <- parinfluence(PAR,model,PDII)
# str(LY)
# explore.influence(LY$Dparm[,1])

## not run: this example take several minutes
## an example in which the deletion of a case yelds a solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# PAR <- c("F2=~y2","F2=~y3","F2=~y4")
# LY <- parinfluence(PAR,model,PDII)

## not run: this example take several minutes
## dealing with ordinal data
data(Q)
model <- "
 F1 =~ it1+it2+it3+it4+it5+it6+it7+it8+it9+it10
"

# fit0 <- sem(model, data=Q, ordered=colnames(Q))
# LY <- parinfluence("F1=~it4",model,Q,ordered=colnames(Q))
# explore.influence(LY$Dparm[,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
