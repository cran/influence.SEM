genCookDist <-
function(model,data,...) {
  fit0 <- sem(model, data, ...)
  th0 <- coef(fit0)[!grepl("|t",names(coef(fit0)),fixed=TRUE)]
  gCD <- NULL
  
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs)
  
  has.tcltk <- requireNamespace("tcltk")
  if (has.tcltk) 
    pb <- tcltk::tkProgressBar("genCookDist", "Inspecting case ", 0, nrow(data))
  
  gCD <- sapply(1:nrow(data),function(i){  
    if (has.tcltk) 
      tcltk::setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", i,"of",nrow(data))))
    
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (inherits(fit,"try-error")) {
      NA
    } else {
      if ((length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))|(!fit@Fit@converged)) {
        NA
      } else {
        thi <- coef(fit)[!grepl("|t",names(coef(fit)),fixed=TRUE)]
        S <- try(vcov(fit),TRUE)
        
        if (inherits(S,"try-error")) {
          NA
        } else {
          S <- solve(S[!grepl("|t",rownames(S),fixed=TRUE),
                       !grepl("|t",colnames(S),fixed=TRUE)])
          CDi <- t(th0-thi)%*%S%*%(th0-thi)
                  
        }
      }
    }
  })
  
  if (has.tcltk) close(pb)
  return(gCD)
}
