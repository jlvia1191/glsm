% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glsm.R
\encoding{UTF-8}
\name{glsm}
\alias{glsm}
\title{Estimation of the log Likelihood of the Saturated Model when the response variable \code{Y} takes one of R>1 values.}
\usage{
glsm(formula, data, ref = NaN)
}
\arguments{
\item{formula}{An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.}

\item{data}{An optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which \code{glsm()} is called.}

\item{ref}{An optional function (default multinomial).}
}
\value{
\code{glsm} returns an object of class "\code{glsm}".

An object of class "\code{glsm}" is a list containing at least the
following components:

\item{coefficients}{Vector of coefficients estimations (intercepts and slopes).}

\item{coef}{Vector of coefficients estimations (intercepts and slopes).}

\item{Std.Error}{Vector of the coefficients’s standard error (intercepts and slopes).}

\item{ExpB}{Vector with the exponential of the coefficients (intercepts and slopes).}

\item{Wald}{Value of the Wald statistic (with chi-squared distribution).}

\item{DF}{Degree of freedom for the Chi-squared distribution.}

\item{P.value}{P-value calculated with the Chi-squared distribution. }

\item{Log_Lik_Complete}{Estimation of the log likelihood in the complete model.}

\item{Log_Lik_Null}{Estimation of the log likelihood in the null model.}

\item{Log_Lik_Logit}{Estimation of the log likelihood in the logistic model.}

\item{Log_Lik_Saturate}{Estimation of the log likelihood in the saturate model.}

\item{Populations}{Number of populations in the saturated model.}

\item{Dev_Null_vs_Logit }{Value of the test statistic  (Hypothesis: null vs logistic models).}

\item{Dev_Logit_vs_Complete}{Value of the test statistic  (Hypothesis:  logistic vs complete models).}

\item{Dev_Logit_vs_Saturate}{Value of the test statistic  (Hypothesis: logistic vs saturated models).}

\item{Df_Null_vs_Logit }{Degree of freedom for the test statistic’s distribution (Hypothesis: null vs logistic models).}

\item{Df_Logit_vs_Complete }{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}

\item{Df_Logit_vs_Saturate}{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}

\item{P.v_Null_vs_Logit}{P-value for the hypothesis test: null vs logistic models.}

\item{P.v_Logit_vs_Complete }{P-value for the hypothesis test:  logistic vs complete models.}

\item{P.v_Logit_vs_Saturate}{P-value for the hypothesis test: logistic vs saturated models.}

\item{Logit_r}{A matrix of the log odds, taking as a reference the r level of the \code{Y}.}

\item{p_hat_complete}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the complete model and without the logistic model).}

\item{p_hat_null}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the null model and without the logistic model).}

\item{p_rj}{Matrix with the estimation of each \code{prj}, the probability that the outcome variable takes the value r, given the \code{jth} population (estimated with the logistic model).}

\item{odd}{Vector with the values of the odd in each \code{jth} population.}

\item{OR}{Vector with the values of the odd ratio for each coefficient of the variables.}

\item{z_rj}{Vector with the values of each \code{Zrj} (the sum of the observations in the \code{jth} population).}

\item{n_j}{Vector with the \code{nj} (the number of the observations in each \code{jth} population).}

\item{p_rj_tilde}{Vector with the estimation of each \code{prj}, the probability that the outcome variable takes the value r, given the \code{jth} population (estimated with the saturated model and without estimate the logistic parameters).}

\item{v_rj}{Vector with the variance of the Bernoulli variables in the \code{jth} population and category \code{r}.}

\item{m_rj}{Vector with the expected values of \code{Zj} in the \code{jth} population and category \code{r}.}

\item{V_rj}{Vector with the variances of \code{Zj} in the \code{jth} population and category \code{r}.}

\item{V}{Variance and covariance matrix of \code{Z}, the vector that contains all the \code{Zj}.}

\item{S_p}{Score vector in the saturated model.}

\item{I_p}{Information matrix in the saturated model.}

\item{Zast_j}{Vector with the values of the standardized variable of \code{Zj}.}

\item{mcov}{Variance and covariance matrix for coefficient estimates.}

\item{mcor}{Correlation matrix for coefficient estimates.}

\item{Esm}{Data frame with estimates in the saturated model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zrj}, \code{prj} and Log-Likelihood \code{Lj_tilde}.}

\item{Elm}{Data frame with estimates in the logistic model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zrj}, \code{pj}, Log-Likelihood \code{Lj}, \code{Logit_prj} and the variance of logit (\code{var.logit}).}

\item{call}{It displays the original call that was used to fit the model lsm.}
}
\description{
When the response variable \code{Y} takes one of R>1 values, the function \code{glsm()} calculates, among others, the values of the maximum likelihood estimates (ML-estimations) of the  corresponding parameters  in the null, complete, saturated and logistic models and also the estimations of the log likelihood in each of this models. The models null and complete are described by Llinas, Arteta and Tilano (2016, ISSN:2389-8976) in sections 2.2 and 2.3.
The saturated model is characterized in section 3 by Orozco, Llinas and Fonseca (2020, ISSN:2389-8976) through the assumptions 1 and 2. Finally,  the logistic model and its assumptions are explained in section 5.
Additionally, based on asymptotic theory for these ML-estimations and the score vector, the function \code{glsm()} calculates the values of the approximations for different deviations -2 log L, where L is the likelihood function. Based on these approximations, the function obtains the values of statistics for several hypothesis tests (each with an asymptotic chi-squared distribution): Null vs Logit, Logit vs Complete and Logit vs Saturated.
With the function \code{glsm()}, it is possible calculate confidence intervals for the logistic parameters and for the corresponding odds ratio.
The asymptotic theory was developed for the case of independent, non-identically distributed variables.  If \code{Y} is polychotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{glsm()} because it works very well for all \code{K}, the number of explanatory variables.
}
\details{
Estimation of the log Likelihood of the Saturated Model the response variable \code{Y} takes one of R>1 values.

An expression of the form \code{y ~ model} is interpreted as a specification that the response \code{y} is modelled by a linear predictor specified symbolically by \code{model} (systematic component). Such a model consists of a series of terms separated by \code{+} operators. The terms themselves consist of variable and factor names separated by \code{:} operators. Such a term is interpreted as the interaction of all the variables and factors appearing in the term. Here, \code{y} is the outcome variable (binary or dichotomous: its values are 0 or 1).
}
\examples{

# library(glsm)
# library(repmis)
# source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
# Datos <- hsbdemo
# modelo <- glsm(prog ~ ses + gender, data=Datos, ref = "academic")
# modelo

}
\references{
\code{[1]}  Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied Logistic Regression, 3rd ed., New York: Wiley.

\code{[2]} LLinás, H. J. (2006). Precisiones en la teoría de los modelos logísticos. Revista Colombiana de Estadística, 29(2), 239–265. https://revistas.unal.edu.co/index.php/estad/article/view/29310

\code{[3]} Llinás, H., & Carreño, C. (2012). The Multinomial Logistic Model for the Case in which the Response Variable Can Assume One of Three Levels and Related Models. Revista Colombiana de Estadística, 35(1), 131-138.

\code{[4]} Orozco-Acosta, E., LLinás-Solano, H., & Fonseca-Rodríguez, J. (2020). Convergence theorems in multinomial saturated and logistic models. Revista Colombiana de Estadística, 43(2), 211-231.

\code{[5]} Solano, H. L., Charris, M. A., & Hernández, J. T. (2016). El modelo de regresión logística para el caso en que la variable de respuesta puede asumir uno de tres niveles: estimaciones, pruebas de hipótesis y selección de modelos. Revista de Matemática: Teoría y Aplicaciones, 23(1), 173-197.
}
\author{
Jorge Villalba (Universidad Tecnológica de Bolívar, Cartagena-Colombia; autor y creador),
Humberto Llinas (Universidad del Norte, Barranquilla-Colombia; autor),
Jorge Borja (Universidad del Norte, Barranquilla-Colombia; autor),
Jorge Tilano (Universidad del Norte, Barranquilla-Colombia; autor).
}
