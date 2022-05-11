#rm(list=ls())
#load("~/lavori/Rdevel/influence.SEM2.0/data/PDII.rda")
#source('~/lavori/Rdevel/influence.SEM2.2/R/bollen.loglik.R')
#source('~/lavori/Rdevel/influence.SEM2.0/R/genCookDist.R')
#model <- "
#F1 =~ y1+y2+y3+y4
#"
#fit0 <- sem(model, data=PDII)

Likedist <-
function(model,data,...) {
  fit0 <- sem(model, data, ...)
  S <- fit0@SampleStats@cov[[1]]
  #L0 <- logLik(fit0)
  L0 <- bollen.loglik(fit0@Data@nobs[[1]],S,fitted(fit0)$cov)
  LD <- NULL
  
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs)
  
  has.tcltk <- requireNamespace("tcltk")
  if (has.tcltk) 
    pb <- tcltk::tkProgressBar("Likedist", "Inspecting case ", 0, nrow(data))
  
  #for (i in 1:nrow(data)) {
  LD <- sapply(1:nrow(data),function(i){  
    
    if (has.tcltk) 
      tcltk::setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", i,"of",nrow(data))))
    
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (inherits(fit,"try-error")) {
      NA
    } else {
      if ((length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))|(!fit@Fit@converged)) {
        NA
      } else {
        #Li <- logLik(fit)
        Li <- bollen.loglik(fit0@Data@nobs[[1]],S,fitted(fit)$cov)
        2*(L0-Li)
      }
    }
  }) 
  
  if (has.tcltk) close(pb)
  return(LD)
}

#LD <-Likedist(model,data=PDII)
#GD <- genCookDist(model,data=PDII)
#plot(LD,GD)
