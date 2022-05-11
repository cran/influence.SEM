#rm(list=ls())
#source("/home/el/lavori/Rdevel/influence.SEM/test.funzioni.R")

parinfluence <-
function(parm,model,data,cook=FALSE,...) {
  fit0 <- sem(model, data, ...)
  #fit0 <- sem(model, data)
    
  (E <- parameterEstimates(fit0))
  (E$parm <- paste(E$lhs,E$op,E$rhs,sep=""))
  (th0 <- E$est[E$parm%in%parm])
  
  ## controllo dei parametri che non ci sono nella VCOV
  if (sum(colnames(vcov(fit0))%in%parm)!=length(parm)) {
    warning(paste("Dparm estimates not available for the following parameters:",
                  paste(parm[!(parm%in%(colnames(vcov(fit0))))],collapse=", ")))
    parmS <- parm[(colnames(vcov(fit0))%in%parm)]
    ### ristrutturo S se parmS!=parm
    S2 <- diag(NA,length(parm))
    colnames(S2) <- rownames(S2) <- parm  
  } else {
    parmS <- parm
  }
  
  Dparm <- NULL; THi <- NULL
  ## tolgo le soglie e le intercette 
  ## creano problemi con i dati ordinali nel 
  ## test successivo any(fit@Fit@est[var.idx]<0))
  #(LPT <- LPT[which((LPT$op!="|")&(LPT$op!="~1")),])
  (var.idx <- which(E$op=="~~" & E$lhs==E$rhs))
  
  has.tcltk <- requireNamespace("tcltk")
  if (has.tcltk) 
    pb <- tcltk::tkProgressBar("parinfluence", "Inspecting case ", 0, nrow(data))
    
  Dparm <- sapply(1:nrow(data),function(i){  
    if (has.tcltk) 
      tcltk::setTkProgressBar(pb, i, label = sprintf(paste("Inspecting case", i,"of",nrow(data))))
    
    fit <- try(sem(model,data[-i,],...),TRUE)
    
    if (inherits(fit,"try-error")) {
      rep(NA,length(parm))
    } else {
      
      (LPT <- parameterEstimates(fit))
      LPT$parm <- paste(LPT$lhs,LPT$op,LPT$rhs,sep="")
      
      if (length(which(is.na(LPT$est[var.idx])))>0) {
        LPT$est[var.idx] <- ifelse(is.na(LPT$est[var.idx]),-1,LPT$est[var.idx])
      }
      
      if ((!fit@Fit@converged)|(length(var.idx)>0L && any(LPT$est[var.idx]<0))) {
        Dparm <- rbind(Dparm,rep(NA,length(parm)))
        rep(NA,length(parm))
      } else {
        thi <- LPT$est[LPT$parm%in%parm]; THi <- rbind(THi,thi)
        S <- try(vcov(fit)[parmS,parmS],TRUE)
        
        if (inherits(S,"try-error")) {
          Dparm <- rbind(Dparm,rep(NA,length(parm)))
          rep(NA,length(parm))
        } else {
          ## gestisce il caso parmS!=parm
          if (exists("S2")) {
            S2[parmS,parmS] <- S
            S <- S2            
          }
          if (length(parm)>1) {
            (S <- sqrt(diag(S)))  
          } else {
            (S <- sqrt(S))  
          }
          Dparm <- (th0-thi)/S
        }
      }
    }
  })
  
  if (has.tcltk) close(pb)
  Dparm <- t(Dparm)
  
  if (cook) {
    gCD <- Dparm^2
    return(list(gCD=gCD,Dparm=Dparm)) 
  } else {
    return(list(Dparm=Dparm)) 
  }
  
}


#data("PDII",package="influence.SEM")
#model <- "
#  F1 =~ y1+y2+y3+y4
#"
#PAR <- c("F1=~y2","F1=~y3","F1=~y4")
#TH <- parinfluence(PAR,model,data=PDII,ordered=colnames(PDII))
#TH
#Mpar <- parinfluence(PAR,model,X)
