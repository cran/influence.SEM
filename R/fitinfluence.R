#rm(list=ls())
#load("~/lavori/Rdevel/dataTest/studytwo.rda")
#data <- Y; index <- "cfi"
fitinfluence <- function (index, model, data, ...) 
{
  fit0 <- sem(model, data, ...)
  #fit0 <- sem(model, data)
  ind0 <- inspect(fit0, "fit")[index]
  Dind <- Oind <- NULL
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op == "~~" & LPT$lhs == LPT$rhs)
  has.tcltk <- requireNamespace("tcltk")
  if (has.tcltk) 
    pb <- tcltk::tkProgressBar("fitinfluence", "Inspecting case ", 0, nrow(data))
  for (i in 1:nrow(data)) {
    if (has.tcltk) 
      tcltk::setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", 
                                                    i, "of", nrow(data))))
    fit <- try(sem(model, data[-i, ], ...), TRUE)
    #fit <- try(sem(model, data[-i, ]), TRUE)
    
    if (class(fit) == "try-error") {
      if (length(index)==1) {
        Dind <- c(Dind, NA)  
      } else {
        Dind <- rbind(Dind,NA)
      }
    } else {
      if ((length(var.idx) > 0L && any(fit@Fit@est[var.idx] < 0)) | (!fit@Fit@converged)) {
        if (length(index)==1) {
          Dind <- c(Dind, NA)
        } else {
          Dind <- rbind(Dind,NA)
        }
      } else {
        indi <- inspect(fit, "fit")[index]
        if (length(index)==1) {
          Dind <- c(Dind, ind0 - indi)  
        } else {
          Dind <- rbind(Dind, ind0 - indi)  
        }
      }
    }
  }
  Dind <- cbind(1:nrow(data),Dind)
  rownames(Dind) <- 1:nrow(data) 
  Dind <- as.data.frame(Dind)
  colnames(Dind) <- c("case",index)
  
  if (has.tcltk) 
    close(pb)
  return(list(Dind=Dind,Oind=ind0))
}

#A <- fitinfluence("nnfi",model,data=Y,std.lv=TRUE)
#A <- fitinfluence(c("cfi","rmsea"),model,data=X,std.lv=TRUE,estimator="MLM")
#str(A)
