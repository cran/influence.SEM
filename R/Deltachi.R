Deltachi <-
function(model,data,...,scaled=FALSE) {
  fit0 <- sem(model, data, ...)
  
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs)
  
  if (scaled) {
    Chi0 <- inspect(fit0,"fit")["chisq.scaled"]
  } else {
    Chi0 <- inspect(fit0,"fit")["chisq"]
  }
  Dchi <- NULL
  for (i in 1:nrow(data)) {
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (class(fit)=="try-error") {
      Dchi <- c(Dchi,NA)
    } else {  
      if ((length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))|(!fit@Fit@converged)) {
        Dchi <- c(Dchi,NA)
      } else {
        if (scaled) {
          Chii <- inspect(fit,"fit")["chisq.scaled"]
        } else {
          Chii <- inspect(fit,"fit")["chisq"]
        } 
        Dchi <- c(Dchi,Chi0-Chii)      
      }
    }
  } 
  return(Dchi)  
}
