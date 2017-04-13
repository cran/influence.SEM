obs.fitres <- function(object) {
  fit <- object
  yvar <- setdiff(fit@Data@ov.names[[1]],fit@Data@ov.names.x[[1]])
  
  ## estraggo intercette e Beta
  (MX <- inspect(fit,"est")$alpha)
  (BE <- inspect(fit,"est")$beta)
  
  ## dati originali
  (X2 <- fit@Data@X[[1]])
  colnames(X2) <- fit@Data@ov.names[[1]]
  
  # stima dei valori attesi
  (MX <- matrix(as.vector(MX),nrow=nrow(X2),ncol=ncol(X2),byrow = TRUE))
  (hatY <- MX + (X2 %*% t(BE)))
  hatY <- data.frame(hatY[,yvar])
  colnames(hatY) <- paste("hat.",yvar,sep="")
  
  ## stima dei residui
  resY <- X2[,yvar]-hatY
  colnames(resY) <- paste("e.",yvar,sep="")
  
  X2 <- data.frame(X2)
  X2 <- cbind(X2,hatY,resY)
  return(X2)
}
