sem.fitres <- function(object) {
  fit <- object
  # controllo che ci sia la media
  if (!fit@Options$meanstructure) {
    fit <- update(fit,meanstructure=TRUE)
  }
  # controllo che le latenti siano standardizzate
  if (!fit@Options$std.lv) {
    fit <- update(fit,std.lv=TRUE,auto.fix.first=FALSE)
  }
  
  # controllo se ci sono variabili latenti
  (modelvar <- unique(unlist(fit@Model@dimNames[[1]])))
  if (length(modelvar)==length(fit@Data@ov.names[[1]])) {
    # non ci sono latenti
    X2 <- obs.fitres(fit)
  } else {
    # ci sono latenti
    X2 <- lat.fitres(fit)
  }
  return(X2)
}
