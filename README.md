
# `glsm()`


Welcome to the *glsm* package!
=============================

When the response variable \code{Y} takes one of R>1 values, the function \code{glsm()} calculates, among others, the values of the maximum likelihood estimates (ML-estimations) of the  corresponding parameters  in the null, complete, saturated and logistic models and also the estimations of the log likelihood in each of this models. The models null and complete are described by Llinas, Arteta and Tilano (2016, ISSN:2389-8976) in sections 2.2 and 2.3. The saturated model is characterized in section 3 by Orozco, Llinas and Fonseca (2020, ISSN:2389-8976) through the assumptions 1 and 2. Finally,  the logistic model and its assumptions are explained in section 5. Additionally, based on asymptotic theory for these ML-estimations and the score vector, the function \code{lsm()} calculates the values of the approximations for different deviations -2 log L, where L is the likelihood function. Based on these approximations, the function obtains the values of statistics for several hypothesis tests (each with an asymptotic chi-squared distribution): Null vs Logit, Logit vs Complete and Logit vs Saturated. With the function \code{glsm()}, it is possible calculate confidence intervals for the logistic parameters and for the corresponding odds ratio. The asymptotic theory was developed for the case of independent, non-identically distributed variables.  If \code{Y} is polychotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{lsm()} because it works very well for all \code{K}, the number of explanatory variables.

Details
-------

The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).


Installation
------------

```{r}
remotes::install_github("jlvia1191/glsm", force = TRUE)
library(glsm)

```



Example Usage
-------------

Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:


```{r}
 library(glsm)
 library(repmis)
 source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
 Datos <- hsbdemo
 modelo <- glsm(prog ~ ses + gender, data=Datos, ref = "academic")
 modelo
```


References
----------

[1]  Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.

[2]  Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.

[3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.


Author(s)
---------


Jorge Luis Villalba Acevedo [cre, aut], Universidad Tecnológica de Bolívar, Cartagena-Colombia.\\ Humberto Llinas Solano [aut], Universidad del Norte, Barranquilla-Colombia \\ Jorge Borja [aut], Universidad del Norte, Barranquilla-Colombia \\ Jorge Tilano [aut], Universidad del Norte, Barranquilla-Colombia. 

----

If you found any ERRORS or have SUGGESTIONS, please report them to my email. Thanks. 
