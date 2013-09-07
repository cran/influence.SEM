#rm(list=ls())
#library(lavaan)
#main <- "~/lavori/FDApsy/"
#datadir <- paste(main,"data/",sep="")
#outdir <- paste(datadir,"outputR011/",sep="")
#source(paste(main,"Rcode/models.R",sep=""))
#modvar <- c(paste("aep",1:12,".x",sep=""),paste("aep",1:12,".y",sep=""))

#load(paste(datadir,"aggiustato2013.rda",sep=""))
#Y1 <- na.omit(X[X$vers.y=="V1",])
#n1 <- nrow(Y1)
#Y3 <- na.omit(X[X$vers.y=="V3",])
#n3 <- nrow(Y3)
#X <- rbind(Y1,Y3)
#load(paste(outdir,"mg00.rda",sep=""))
#parm <- c("F10~~F10","F10~~F11","F11~~F11","F11~~K","K~~K")

#model <- Mf01k
#data <- X
#fit0 <- sem(model,data)
#(th0 <- coef(fit0)[parm])

#Dparm <- NULL
#(LPT <- parTable(fit0))
#(var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs))

#i <- 88
#for (i in 1:nrow(data)) {
#  fit <- try(sem(model,data[-i,]),TRUE)
#  
#  if (class(fit)=="try-error") {
#    Dparm <- rbind(Dparm,rep(NA,length(parm)))
#  } else {
#    if ((!fit@Fit@converged)|(length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))) {
#      Dparm <- rbind(Dparm,rep(NA,length(parm)))
#    } else {
#      (thi <- coef(fit)[parm])
#      (S <- try(vcov(fit)[parm,parm],TRUE))
#      
#      if (class(S)=="try-error") {
#        Dparm <- rbind(Dparm,rep(NA,length(parm)))
#      } else {
#        if (length(parm)>1) {
#          (S <- sqrt(diag(S)))  
#        } else {
#          (S <- sqrt(S))  
#        }
#        Di <- (th0-thi)/S
#        Dparm <- rbind(Dparm,Di)                
#      }
#    }
#  }
#}

#boxplot(Dparm)

parinfluence <-
function(parm,model,data,...) {
  fit0 <- sem(model, data, ...)
  th0 <- coef(fit0)[parm]
  Dparm <- NULL; THi <- NULL
  
  LPT <- parTable(fit0)
  var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs)
  
  for (i in 1:nrow(data)) {
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (class(fit)=="try-error") {
      Dparm <- rbind(Dparm,rep(NA,length(parm)))
      THi <- rbind(THi,rep(NA,length(parm)))
    } else {
      if ((!fit@Fit@converged)|(length(var.idx)>0L && any(fit@Fit@est[var.idx]<0))) {
        Dparm <- rbind(Dparm,rep(NA,length(parm)))
        THi <- rbind(THi,rep(NA,length(parm)))
      } else {
        thi <- coef(fit)[parm]; THi <- rbind(THi,thi)
        S <- try(vcov(fit)[parm,parm],TRUE)
        
        if (class(S)=="try-error") {
          Dparm <- rbind(Dparm,rep(NA,length(parm)))
        } else {
          if (length(parm)>1) {
            (S <- sqrt(diag(S)))  
          } else {
            (S <- sqrt(S))  
          }
          Di <- (th0-thi)/S
          Dparm <- rbind(Dparm,Di)                
        }
      }
    }
  }
  return(list(Dparm=Dparm,THi=THi))
}

#PH <- parinfluence(par,Mf01k,X)