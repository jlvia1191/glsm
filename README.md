
# `glsm()`


Welcome to the *glsm* package!
=============================

When the values of the outcome variable Y are either 0 or 1, the function calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If is dichotomous and the data are grouped in J populations, it is recommended to use the function because it works very well for all .

Details
-------

The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).


Installation
------------

```{r}
remotes::install_github("jlvia1191/glsm")
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
