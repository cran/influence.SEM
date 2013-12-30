#rm(list=ls())
#library(lavaan)
#main <- "~/lavori/Rdevel/"
#datadir <- paste(main,"data/",sep="")
#load(paste(datadir,"inluence.SEM.rda",sep=""))
#fit0 <- sem(model, data=X, ordered=colnames(X),std.lv=TRUE)

#data(Q,package="influence.SEM")
#model <- "
# F1 =~ it1+it2+it3+it4+it5+it6+it7+it8+it9+it10
#"
#fit0 <- sem(model, data=Q, ordered=colnames(Q))
#i <- 29
#parm <- "F1=~it4"
#data <- Q[1:30,]

parinfluence <-
function(parm,model,data,cook=FALSE,...) {
  fit0 <- sem(model, data, ...)
  (th0 <- coef(fit0)[parm])
  Dparm <- NULL; THi <- NULL
  
  (LPT <- parTable(fit0))
  ## tolgo le soglie e le intercette 
  ## creano problemi con i dati ordinali nel 
  ## test successivo any(fit@Fit@est[var.idx]<0))
  #(LPT <- LPT[which((LPT$op!="|")&(LPT$op!="~1")),])
  (var.idx <- which(LPT$op=="~~" & LPT$lhs==LPT$rhs))
  
  has.tcltk <- require("tcltk")
  if (has.tcltk) 
    pb <- tkProgressBar("", "Inspecting case ", 0, nrow(data))
    
  for (i in 1:nrow(data)) {
    
    if (has.tcltk) 
      setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", i,"of",nrow(data))))
    
    fit <- try(sem(model,data[-i,],...),TRUE)
    #fit <- try(sem(model,data=Q[-i,],ordered=colnames(Q)),TRUE)
    
    if (class(fit)=="try-error") {
      Dparm <- rbind(Dparm,rep(NA,length(parm)))
      THi <- rbind(THi,rep(NA,length(parm)))
    } else {
      
      #cat(paste("riga ",i,":",fit@Fit@converged),"\n")
      #cat(paste("riga ",i,":",paste(round(fit@Fit@est[var.idx],2),collapse=",")),"\n")
      #cat(paste("riga ",i,":",length(which(is.na(fit@Fit@est[var.idx])))),"\n")
      #if (length(which(is.na(fit@Fit@est[var.idx]))>0)) cat("ECCOLO","\n")
      
      if (length(which(is.na(fit@Fit@est[var.idx]))>0)) {
        fit@Fit@est[var.idx] <- ifelse(is.na(fit@Fit@est[var.idx]),-1,fit@Fit@est[var.idx])
        #cat(paste("modifica:",paste(round(fit@Fit@est[var.idx],2),collapse=",")),"\n")
      }
      #cat("    ","\n")
      
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

#PH <- parinfluence(parm,model,X,ordered=colnames(X),std.lv=TRUE)

#data(Q,package="influence.SEM")
#model <- "
# F1 =~ it1+it2+it3+it4+it5+it6+it7+it8+it9+it10
#"

#fit0 <- sem(model, data=Q, ordered=colnames(Q))
#LY <- parinfluence("F1=~it4",model,Q[1:30,],ordered=colnames(Q))
#LY

#fit <- sem(model, data=Q[c(1:28,30),], ordered=colnames(Q))
