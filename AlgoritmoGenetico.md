Proyecto Final - Algoritmo Genetico
================
Anna Casillas
Agosto 3,2020

# Algoritmo Genetico

Equipo:

  - Anna Karen Casillas
  - Josue Emmnanuel Gomez
  - Luis Francisco Gonzalez

## Variables y Dependencias Iniciales

Aqui se especifican las variables generales necesarias para todo el
proyecto

``` r
#Modificar variable para especificar directorio del Proyecto Final
user.path <- "/Users/akcasill/Documents/analisisDatos/proyecto/mcc-analisisdatos-final"

local.path <- paste(user.path ,"/data",sep = "")
local.path.imgs <- paste(user.path ,"/imgs",sep = "")
```

Aqui se especifican todas las dependencias que se utilizaran en el
proyecto

``` r
#Dependencies
#install.packages("png")
library(png)
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
source("http://www.sthda.com/upload/rquery_cormat.r")
col<- colorRampPalette(c("blue", "white", "red"))(20)
#install.packages("factoextra")
#install.packages("FactoMineR")
library(FactoMineR)
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

Cargamos los datos

``` r
setwd(local.path)
load("alumnos.training.R")
```

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
