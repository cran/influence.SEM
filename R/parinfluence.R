#rm(list=ls())
#library(lavaan)
#main <- "~/2013_09/lavori/arcuri/berti/"
#datadir <- paste(main,"data/",sep="")
#source(paste(main,"Rcode/models.R",sep=""))
#X <- foreign::read.spss(paste(datadir,"DEPURATI.sav",sep=""),to.data.frame=TRUE)
#X <- X[-which(X$RAMOD>=4.25),]
#X <- X[1:80,]

#i <- 1; modvar <- NULL
#for (j in seq(9,54,by=9)) {
#  item <- paste("HITi",i:j,sep="")
#  modvar <- c(modvar,item)
#  (i <- j+1)
#}
#modvar <- modvar[-c(4,13,20,27,31,38,45,51,1,9,16,24,34,41,48)]

#parm <- "opposiz~~aggress"
#model <- M03
#data <- X

#load(paste(datadir,"m03R03.rda",sep=""))
#fit0 <- m03
#fit0 <- sem(model,data=X)

parinfluence <-
function(parm,model,data,cook=FALSE,...) {
  fit0 <- sem(model, data, ...)
  (th0 <- coef(fit0)[parm])
  Dparm <- NULL; THi <- NULL
  
  (LPT <- parTable(fit0))
  ## tolgo le soglie e le intercette 
  ## creano problemi con i dati ordinali nel 
  ## test successivo any(fit@Fit@est[var.idx]<0))
  (LPT <- LPT[which((LPT$op!="|")&(LPT$op!="~1")),])
  
  (var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs))
  
  has.tcltk <- require("tcltk")
  if (has.tcltk) 
    pb <- tkProgressBar("", "Inspecting case ", 0, nrow(data))
    
  for (i in 1:nrow(data)) {
    
    if (has.tcltk) 
      setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", i,"of",nrow(data))))
    
    fit <- try(sem(model,data[-i,],...),TRUE)
    #fit <- try(sem(model,data[-i,]),TRUE)
    
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
  
  if (has.tcltk) close(pb)
  
  if (cook) {
    gCD <- Dparm^2
    return(list(gCD=gCD,Dparm=Dparm,THi=THi))
  } else {
    return(list(Dparm=Dparm,THi=THi))
  }
  
}

#PH <- parinfluence(par,Mf01k,X)