\name{explore.influence}
\alias{explore.influence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explores case influence.
}
\description{
It explores case influence. Cases with extreme values of the considered measure of influence are reported. Extreme values are determined using the boxplot criterion (Tukey, 1977) or user-defined cut-offs. Cases for which deletion leads to a model that does not converge or yelds a solution with negative estimated variances are also reported. In addition, explore.influence provides a graphical representation of case influence.
}
\usage{
explore.influence(x, cut.offs = 'default', plot = 'TRUE', cook = 'FALSE')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector containing the influence of each case as returned by
\code{\link{Deltachi}}, \code{\link{fitinfluence}}, \code{\link{genCookDist}}, \code{\link{Likedist}} or \code{\link{parinfluence}} functions.
}
  \item{cut.offs}{
A vector of two numeric elements containing the lower and the upper cut-offs to be considered. If \code{default}, the cut-offs are calculated according to the boxplot criterion for outliers (see also, \code{cook}).
}
  \item{plot}{
If \code{TRUE} (the default) a graphical representation of case influence is given.
}
  \item{cook}{
If \code{TRUE}, \code{x} is interpreted as a vector containing Cook's distances, and so the lower cut-off is forced to be greater or equal to zero.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with the following components:
 \item{n }{number of cases.}
 \item{cook }{logical, indicating if \code{x} is treated as a vector of Cook's distances.}
 \item{cut.low }{the lower cut-off.}
 \item{cut.upp }{the upper cut-off.}
 \item{not.allowed }{a vector containing cases with negative variance or not converging models.}
 \item{less.cut.low}{a vector containing cases with influence value less than the lower cut-off.}
 \item{greater.cut.low}{a vector containing cases with influence value greater than the upper cut-off.}
}
\references{
Tukey, J. W. (1977). \emph{Exploratory data analysis}. Reading, MA: Addison-Wesley. 
}
\author{
Gianmarco Altoe'
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data("PDII")
model <- "
F1 =~ y1+y2+y3+y4
"
fit0 <- sem(model, data=PDII,std.lv=TRUE)
gCD <- genCookDist(model,data=PDII,std.lv=TRUE)
explore.influence(gCD,cook=TRUE)

##
## not run: this example take several minutes
model <- "
F1 =~ x1+x2+x3
F2 =~ y1+y2+y3+y4
F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# FI <- fitinfluence('rmsea',model,PDII)
# explore.influence(FI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
