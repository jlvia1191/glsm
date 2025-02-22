#' confint Method for glsm
#'
#' @title  Confidence Intervals for \code{glsm}  Objects
#' @description Provides a confint method for \code{glsm}  objects.
#'
#' @param object The type of prediction required. The default is on the scale of the linear predictors. The alternative \code{response} gives the predicted probabilities.
#' @param parm calculate confidence intervals for the coefficients
#' @param level  It gives the desired confidence level for the confidence interval. For example, a default value is level = 0.95, which will generate a 95% confidence interval."
#' The alternative \code{response} gives the predicted probabilities.
#' @param ... further arguments passed to or from other methods.
#' @return  \code{glsm} returns an object of class "\code{glsm}".
#'
#'  An object of class "\code{glsm}" is a list containing at least the
#'  following components:
#'
#' \item{object}{a \code{glsm} object}
#' \item{parm}{calculate confidence intervals for the coefficients.}
#' \item{level}{confidence levels}
#' \item{\dots}{Additional arguments to be passed to methods.}
#'
#' @encoding UTF-8
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
#' @references [1] LLinás, H. J. (2006). Precisiones en la teoría de los modelos logísticos. Revista Colombiana de Estadística, 29(2), 239–265. https://revistas.unal.edu.co/index.php/estad/article/view/29310
#' @references [2] Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied Logistic Regression, 3rd ed., New York: Wiley.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992). Statistical Models in S. Wadsworth & Brooks/Cole.
#'
#' @author Jorge Villalba Acevedo [cre, aut], (Universidad Tecnológica de Bolívar, Cartagena-Colombia).
#'
#' @examples
#'
#' # library(glsm)
#' #Datos de ejemplo
#' # AGE <- c(20, 23, 24, 25, 25, 26, 26, 28, 28, 29, 30, 30, 30, 30, 30, 30, 30, 32, 33, 33)
#' # CHD <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
#' # data <- data.frame(CHD, AGE)
#' #Ejemplo de uso de una función en el paquete `glsm`
#' # model <- glsm(CHD ~ AGE, data = data)
#' # summary(model)


#' @export
#' @exportS3Method confint glsm
#'
confint.glsm <- function(object, parm, level = 0.95, ...) {
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }

  # Comprobación del valor de 'level'
  if ((length(level) != 1L) || is.na(level) || (level <= 0) || (level >= 1)) {
    stop("'level' must be a single number between 0 and 1")
  }

  # Comprobación de 'parm' si está especificado
  if (!missing(parm)) {
    if (!is.character(parm)) {
      stop("`parm` must be a character string.")
    }
    if (!all(parm %in% names(object$coef))) {
      stop(paste("The coefficient", parm, "is not present in the model."))
    }
  }

  alpha <- 1 - level
  Z <- qnorm(1 - alpha / 2)

  # Calcular intervalos de confianza para todos los parámetros
  if (missing(parm)) {
    li <- object$coefficients - Z * object$Std.Error
    ls <- object$coefficients + Z * object$Std.Error

    ret <- cbind(li, ls)
    colnames(ret) <- c(paste0("lower ", 100 * (1 - level) / 2, "%"), paste0("upper ", 100 * (1 - (1 - level) / 2), "%"))

    sal <- list(confint = ret, OR = exp(ret[-1, ]), level = level * 100)
  } else {
    # Intervalos de confianza para un coeficiente específico
    if (is.na(object$Std.Error[parm])) {
      stop(paste("The standard error for the coefficient", parm, "is NA."))
    }

    li <- object$coef[parm] - Z * object$Std.Error[parm]
    ls <- object$coef[parm] + Z * object$Std.Error[parm]

    ret <- cbind(li, ls)
    colnames(ret) <- c(paste0("lower ", 100 * (1 - level) / 2, "%"), paste0("upper ", 100 * (1 - (1 - level) / 2), "%"))

    # Si el coeficiente es el intercepto, no calculamos el OR
    if (parm == "(Intercept)") {
      sal <- list(confint = ret, OR = "-", level = level * 100)
    } else {
      sal <- list(confint = ret, OR = exp(ret), level = level * 100)
    }
  }

  class(sal) <- "confint.glsm"
  return(sal)
}

#' @export
print.confint.glsm <- function(x, ...) {
  cat(x$level, "% confidence intervals for coefficients:\n", sep = "")
  print(x$confint)

  cat("\n", x$level, "% confidence intervals for Odds Ratios:\n", sep = "")
  print(x$OR)
}
