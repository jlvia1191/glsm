
# `glsm()`

<!-- badges: start -->

**Release:**[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/lsm)](https://cran.r-project.org/package=lsm) ![CRAN Monthly Downloads](https://cranlogs.r-pkg.org/badges/last-month/lsm) ![CRAN
Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/lsm)


**Development:** [![Project Status: Active - The project has reached a
stable, usable state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->


Welcome to the *lsm* package!
=============================

When the values of the outcome variable Y are either 0 or 1, the function calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If is dichotomous and the data are grouped in J populations, it is recommended to use the function because it works very well for all .

Details
-------

The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).


Installation
------------

```{r}
install.packages("glsm")
library(glsm)

```



Example Usage
-------------

Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:


```{r}
library(lsm)

  AGE <- c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,30,32,33,33)
  CHD <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  
  data <- data.frame (CHD,  AGE )
  lsm(CHD ~ AGE , family=binomial, data)
  
  ## For more ease, use the following notation.
  
  lsm(y~., data)
```

 # Other case.

``` {r}
   y <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1)
  x1 <- c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11)
 
  data <- data.frame (y, x1)
  ELAINYS <-lsm(y ~ x1, family=binomial, data)
  summary(ELAINYS)
```

 # Other case.

```{r}

  y <- as.factor(c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))
  x1 <- as.factor(c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11))
 
  data <- data.frame (y, x1)
  ELAINYS1 <-lsm(y ~ x1, family=binomial, data)
  confint(ELAINYS1)
```


References
----------

[1]  Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.

[2]  Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.

[3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.


Author(s)
---------


Humberto Llinas Solano [aut], Universidad del Norte, Barranquilla-Colombia \\ Omar Fabregas Cera [aut], Universidad del Norte, Barranquilla-Colombia \\ Jorge Villalba Acevedo [cre, aut], Universidad Tecnológica de Bolívar, Cartagena-Colombia.

----

If you found any ERRORS or have SUGGESTIONS, please report them to my email. Thanks. 
