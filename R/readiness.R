

readiness <- function(options) {
  results <- list(reason = NULL,
                 ready = TRUE,
                 report = FALSE)

  if (!is.something(options$endo)) {
    results$ready   <- FALSE
    results$report  <- TRUE
    results$reason  <- "Please select at least one endogenous independent variable"
    return(results)
    
  }  
  if (!is.something(options$iv)) {
    results$ready   <- FALSE
    results$report  <- TRUE
    results$reason  <- "Please select at least one instrumental variable"
    return(results)
    
  }  
  
  return(results)
}
