fitinfluence <-
function(index,model,data,...) {
  fit0 <- sem(model, data, ...)
  ind0 <- inspect(fit0,"fit")[index]
  Dind <- NULL
  
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs)
  
  for (i in 1:nrow(data)) {
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (class(fit)=="try-error") {
      Dind <- c(Dind,NA)
    } else {
      if ((length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))|(!fit@Fit@converged)) {
        Dind <- c(Dind,NA)
      } else {
        indi <- inspect(fit,"fit")[index]
        Dind <- c(Dind,ind0-indi)        
      }  
    }
  }
  return(Dind)
}

