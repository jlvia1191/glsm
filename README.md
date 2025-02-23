
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `glsm()`

<!-- badges: start -->
<!-- badges: end -->

# Welcome to the *glsm* package!

When the response variable takes one of R\>1 values, the function
calculates, among others, the values of the maximum likelihood estimates
(ML-estimations) of the corresponding parameters in the null, complete,
saturated and logistic models and also the estimations of the log
likelihood in each of this models. The models null and complete are
described by Llinas, Arteta and Tilano (2016, ISSN:2389-8976) in
sections 2.2 and 2.3. The saturated model is characterized in section 3
by Orozco, Llinas and Fonseca (2020, ISSN:2389-8976) through the
assumptions 1 and 2. Finally, the logistic model and its assumptions are
explained in section 5. Additionally, based on asymptotic theory for
these ML-estimations and the score vector, the function calculates the
values of the approximations for different deviations -2 log L, where L
is the likelihood function. Based on these approximations, the function
obtains the values of statistics for several hypothesis tests (each with
an asymptotic chi-squared distribution): Null vs Logit, Logit vs
Complete and Logit vs Saturated. With the function , it is possible
calculate confidence intervals for the logistic parameters and for the
corresponding odds ratio. The asymptotic theory was developed for the
case of independent, non-identically distributed variables. If is
polychotomous and the data are grouped in populations, it is recommended
to use the function because it works very well for all , the number of
explanatory variables.

## Details

The saturated model is characterized by the assumptions 1 and 2
presented in section 2.3 by Llinas (2006, ISSN:2389-8976).

## Installation

You can install the development version of glsm like so:

``` r
# install.packages("devtools")

remotes::install_github("jlvia1191/glsm", force = TRUE)

devtools::install_guihub("jlvia1191/glsm", force = TRUE)

```

## Example Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(glsm)
library(repmis)
source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
#> Downloading data from: https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false
#> SHA-1 hash of the downloaded data file is:
#> e20031220da78c71140cd312a7f198c0d9cf9105
#> [1] "hsbdemo"
Datos <- hsbdemo
modelo <- glsm(prog ~ ses + gender, data=Datos, ref = "academic")
modelo
#> 
#> Call:
#> glsm(formula = prog ~ ses + gender, data = Datos, ref = "academic")
#> 
#> Populations in Saturate Model: 6
#> 
#> Coefficients: 
#>                         Coef(B) Std.Error    Exp(B)
#> (Intercept):general  -1.6547758 0.4175354 0.1911349
#> (Intercept):vocation -1.8469099 0.4478055 0.1577238
#> seslow:general        1.4118732 0.5064763 4.1036349
#> seslow:vocation       1.3534875 0.5548849 3.8709018
#> sesmiddle:general     0.7550865 0.4561360 2.1277956
#> sesmiddle:vocation    1.4430949 0.4709456 4.2337787
#> gendermale:general    0.2216624 0.3716764 1.2481500
#> gendermale:vocation   0.1098969 0.3599743 1.1161630
#> 
#> Log Likelihood: 
#>          Estimation
#> Complete     0.0000
#> Null      -204.0967
#> Logit     -195.5208
#> Saturate  -194.5159
```

## References

\[1\] Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied
Logistic Regression, 3rd ed., New York: Wiley.

\[2\] LLinás, H. J. (2006). Precisiones en la teoría de los modelos
logísticos. Revista Colombiana de Estadística, 29(2), 239–265.
<https://revistas.unal.edu.co/index.php/estad/article/view/29310>

\[3\] Llinás, H., & Carreño, C. (2012). The Multinomial Logistic Model
for the Case in which the Response Variable Can Assume One of Three
Levels and Related Models. Revista Colombiana de Estadística, 35(1),
131-138.

\[4\] Orozco-Acosta, E., LLinás-Solano, H., & Fonseca-Rodríguez, J.
(2020). Convergence theorems in multinomial saturated and logistic
models. Revista Colombiana de Estadística, 43(2), 211-231.

\[5\] Solano, H. L., Charris, M. A., & Hernández, J. T. (2016). El
modelo de regresión logística para el caso en que la variable de
respuesta puede asumir uno de tres niveles: estimaciones, pruebas de
hipótesis y selección de modelos. Revista de Matemática: Teoría y
Aplicaciones, 23(1), 173-197.

## Author(s)

Jorge Luis Villalba Acevedo \[cre, aut\], Universidad Tecnológica de
Bolívar, Cartagena-Colombia.\\ Humberto Llinas Solano \[aut\],
Universidad del Norte, Barranquilla-Colombia \\ Jorge Borja \[aut\],
Universidad del Norte, Barranquilla-Colombia \\ Jorge Tilano \[aut\],
Universidad del Norte, Barranquilla-Colombia.

------------------------------------------------------------------------

If you found any ERRORS or have SUGGESTIONS, please report them to my
email. Thanks.
