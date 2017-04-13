lat.fitres <- function(object) {
  fit <- object
  ## estraggo parametri
  (MI <- inspect(fit,"est")$nu) # means 
  (LA <- inspect(fit,"est")$lambda) # factor loadings
  (BE <- inspect(fit,"est")$beta) # structural coefficients
  
  ## factor scores
  (ETA <- predict(fit))

  ## dati originali
  (X2 <- fit@Data@X[[1]])
  colnames(X2) <- fit@Data@ov.names[[1]]
  
  ## valori attesi
  (hatY <- matrix(as.vector(MI),nrow=nrow(ETA),ncol=nrow(LA),byrow=TRUE) + ETA %*% t(LA))
  colnames(hatY) <- paste("hat.",colnames(hatY),sep="")
  if (!is.null(BE)) {
    (hatETA <- ETA %*% t(BE))
    etavar <- colnames(hatETA)[!(apply(hatETA,2,sum)==0)]
    hatETA <- hatETA[,etavar]
    colnames(hatETA) <- paste("hat.",colnames(hatETA),sep="")
  }
  
  ## stima dei residui
  resY <- X2-hatY
  colnames(resY) <- paste("e.",colnames(resY),sep="")
  if (!is.null(BE)) {
    resETA <- ETA[,etavar] - hatETA
    colnames(resETA) <- paste("e.",colnames(resETA),sep="")
  }
  
  X2 <- data.frame(X2)
  if (!is.null(BE)) {
    X2 <- cbind(X2,hatY,resY,hatETA,resETA)
  } else {
    X2 <- cbind(X2,hatY,resY)
  }
  return(X2)
}
