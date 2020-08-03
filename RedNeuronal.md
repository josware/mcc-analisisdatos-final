Proyecto Final - Red neuronal
================
Anna Casillas
Agosto 2,2020

# Red Neuronal

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

``` r
#install.packages('neuralnet')
library(neuralnet)
```

Cargamos los datos

``` r
setwd(local.path)
load("alumnos.training.R")
#alumnos.training
head(alumnos.training,3)
```

    ##   genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 1      2        60.09373         35.18746              70.28119           18
    ## 2      2        59.07874         33.15747              67.23621           17
    ## 3      2        53.14335         21.28669              60.00000           15
    ##   evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 1                        4            16    0   0.8750000  12.47364  12.51429
    ## 2                        4            15    0   0.8567708  13.13827  13.10560
    ## 3                        4            13    0   0.8424479  12.53885  12.51326
    ##   prom.pagos prom.uso.biblio uso.biblio prom.uso.platf uso.platf
    ## 1          2        15.50000          2          38.50         2
    ## 2          0        18.16667          3          42.75         3
    ## 3          0        15.25000          1          35.50         1
    ##   prom.apartado.libros cambio.carrera
    ## 1             1.250000              0
    ## 2             1.333333              0
    ## 3             1.083333              0

``` r
load("alumnos.test.R")
#alumnos.test
head(alumnos.test,3)
```

    ##    genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 5       2        61.47273         37.94545              74.41818           18
    ## 15      2        63.70695         42.41390              81.12085           19
    ## 17      2        55.22528         25.45056              60.00000           16
    ##    evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 5                         4            16    0   0.8437500  12.44557  12.39884
    ## 15                        4            17    0   0.8841146  14.14316  14.05269
    ## 17                        4            14    0   0.8893229  11.90611  11.88356
    ##    prom.pagos prom.uso.biblio uso.biblio prom.uso.platf uso.platf
    ## 5       2.000        15.75000          2       36.16667         2
    ## 15      2.000        26.58333          6       63.75000         6
    ## 17      1.875        13.00000          0       30.75000         0
    ##    prom.apartado.libros cambio.carrera
    ## 5             1.0833333              0
    ## 15            1.8333333              0
    ## 17            0.9166667              0

## Formula 1

## Neural Net

## Testing

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
