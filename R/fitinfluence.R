#rm(list=ls())
#source("/home/el/lavori/Rdevel/influence.SEM/test.funzioni.R")
#data(PDII,package="influence.SEM")
#model <- "
#  F1 =~ x1+x2+x3
#  F2 =~ y1+y2+y3+y4
#  F3 =~ y5+y6+y7+y8
#"
#fit0 <- sem(model, data=PDII)
#index <- "cfi"

fitinfluence <- function (index, model, data, ...) 
{
  fit0 <- sem(model, data, ...)
  #fit0 <- sem(model, data)
  (ind0 <- inspect(fit0, "fit")[index])
  Dind <- Oind <- NULL
  (LPT <- parTable(fit0))
  (var.idx <- which(LPT$op == "~~" & LPT$lhs == LPT$rhs))
  
  has.tcltk <- requireNamespace("tcltk")
  if (has.tcltk) 
    pb <- tcltk::tkProgressBar("fitinfluence", "Inspecting case ", 0, nrow(data))
  
  #for (i in 1:nrow(data)) {
  Dind <- sapply(1:nrow(data),function(i){
    if (has.tcltk) {
      tcltk::setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", 
                                                           i, "of", nrow(data))))
    }
    fit <- try(sem(model, data[-i, ], ...), TRUE)
    #fit <- try(sem(model, data[-i, ]), TRUE)
    
    if (class(fit) == "try-error") {
      if (length(index)==1) {
        NA
      } else {
        rep(NA,length(index))
      }
    } else {
      if ((length(var.idx) > 0L && any(fit@Fit@est[var.idx] < 0)) | (!fit@Fit@converged)) {
        if (length(index)==1) {
          NA
        } else {
          rep(NA,length(index))
        }
      } else {
        indi <- inspect(fit, "fit")[index]
        if (length(index)==1) {
          ind0 - indi
        } else {
           ind0 - indi
        }
      }
    }
    
  })
  #}
  
  if (length(index)>1) {
    Dind <- t(Dind)
    (rownames(Dind) <- 1:nrow(data))
    Dind <- data.frame(case=1:nrow(data),Dind)
  } else {
    Dind <- data.frame(case=1:nrow(data),Dind)
    colnames(Dind) <- c("case",index)
  }
  
  if (has.tcltk) close(pb)
  return(list(Dind=Dind,Oind=ind0))
}

#A <- fitinfluence("nnfi",model,data=PDII,std.lv=TRUE)
#fitinfluence(c("cfi","rmsea"),model,data=PDII,std.lv=TRUE,estimator="MLM")
#str(A)
