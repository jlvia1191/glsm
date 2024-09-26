plot.glsm <- function(x, type = c("scatter", "probability", "Logit", "odds"), title = NULL, xlab = NULL, ylab = NULL, color = "red", size = 1.5, shape = 19, ...) {
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }
  
  type <- match.arg(type)
  
  formula <- x$call$formula
  
  v.i <- as.character(all.vars(formula)[-1])
  v.d <- as.character(all.vars(formula)[1])
  
  if(type == "scatter") {
    plots <- list()
    for (var in v.i) {
      p <- ggplot(x$data, aes(x = .data[[var]], y = .data[[v.d]])) +
        geom_point(size = size, shape = shape, color = color) +
        labs(x = ifelse(is.null(xlab), var, xlab),
             y = ifelse(is.null(ylab), v.d, ylab),
             title = ifelse(is.null(title), paste("Scatterplot", v.d, "versus", var), title))
      plots[[var]] <- p
    }
    return(plots)
  }
  
  stop("Invalid plot type. Valid types are 'scatter', 'probability', 'Logit', and 'odds'.")
}