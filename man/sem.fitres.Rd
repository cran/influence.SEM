\name{sem.fitres}
\alias{sem.fitres}
\alias{obs.fitres}
\alias{lat.fitres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fitted values and residuals}
\description{
It calculates the expected values and the residuals of a sem model.
}
\usage{
sem.fitres(object)
obs.fitres(object)
lat.fitres(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{lavaan}.}
}
\details{
The main  function, \code{sem.fitres()}, calls one of the other two routines depending on the type of the model. If model does not contain latent variables, \code{sem.fitres()} calls the function \code{obs.fitres()}, otherwise calls the function \code{lat.fitres()}.

The functions \code{obs.fitres()} and \code{lat.fitres()} are internal functions, do not use it directly.
}
\value{
Returns a data frame containing:
1) The observed model variables; 2) The expected values on dependent variables (indicated with \code{hat.}); 3) The residuals on dependent variables (indicated with \code{e.})
}
%\references{
%Bollen, K. A. (1989). \emph{Structural equations with latent variables}. New York: Wiley%.}
\author{Massimiliano Pastore}
\note{ 
In order to compute more interpretable fitted values and residuals, model is forced to have \code{meanstrucure = TRUE} and \code{std.lv = TRUE}.
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"

fit0 <- sem(model, data=PDII)
out <- sem.fitres(fit0)
head(out)

par(mfrow=c(2,2))
plot(e.y1~hat.y1,data=out)
plot(e.y2~hat.y2,data=out)
plot(e.y3~hat.y3,data=out)
plot(e.y4~hat.y4,data=out)

qqnorm(out$e.y1); qqline(out$e.y1)
qqnorm(out$e.y2); qqline(out$e.y2)
qqnorm(out$e.y3); qqline(out$e.y3)
qqnorm(out$e.y4); qqline(out$e.y4)
}
\keyword{utility}
