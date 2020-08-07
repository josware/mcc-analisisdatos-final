Proyecto Final - Analisis de Datos
================
Casillas, A., González, L., Gómez, J.
July 31,2020

# Integracion de datos

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
```

## Datos - Proyecto Integrador

En General toda la informacion importada es sobre los 9 semestres de una
carrera promedio con 6 materias cada uno.

### Perfil Alumnos

Definición Valores

  - Genero: 2 Hombre, 1 Mujer.
  - admision.letras: Calificación Examen Admisión Español
  - admision.numeros: Calificación Examen Admisión Matemáticas
  - promedio.preparatoria: Calificación Promedio Preparatoria  
  - edad.ingreso: Edad, variable numérica  
  - evalucion.socioeconomica: 1 más privilegiado, 4 menos privilagiado
  - nota.conducta: Calificación subjetiva. 20 es el maximo de mejor
    conduta

<!-- end list -->

``` r
setwd(local.path)
load("perfilAlumnos.R")
class(perfil.alumnos)
```

    ## [1] "data.frame"

``` r
str(perfil.alumnos)
```

    ## 'data.frame':    1000 obs. of  7 variables:
    ##  $ genero                  : int  2 2 2 1 2 2 2 2 1 2 ...
    ##  $ admision.letras         : num  60.1 59.1 53.1 57 61.5 ...
    ##  $ admision.numeros        : num  35.2 33.2 21.3 29 37.9 ...
    ##  $ promedio.preparatoria   : num  70.3 67.2 60 61 74.4 ...
    ##  $ edad.ingreso            : num  18 17 15 16 18 18 15 17 14 17 ...
    ##  $ evalucion.socioeconomica: int  4 4 4 4 4 4 4 4 4 4 ...
    ##  $ nota.conducta           : num  16 15 13 14 16 16 13 15 12 15 ...

``` r
summary(perfil.alumnos)
```

    ##      genero      admision.letras admision.numeros promedio.preparatoria
    ##  Min.   :1.000   Min.   :44.94   Min.   : 4.878   Min.   : 60.00       
    ##  1st Qu.:1.000   1st Qu.:56.61   1st Qu.:28.226   1st Qu.: 60.00       
    ##  Median :2.000   Median :59.98   Median :34.970   Median : 69.95       
    ##  Mean   :1.595   Mean   :60.06   Mean   :35.114   Mean   : 72.25       
    ##  3rd Qu.:2.000   3rd Qu.:63.64   3rd Qu.:42.275   3rd Qu.: 80.91       
    ##  Max.   :2.000   Max.   :77.71   Max.   :70.411   Max.   :100.00       
    ##   edad.ingreso   evalucion.socioeconomica nota.conducta  
    ##  Min.   :11.00   Min.   :1.000            Min.   : 9.00  
    ##  1st Qu.:16.00   1st Qu.:3.000            1st Qu.:14.00  
    ##  Median :17.00   Median :4.000            Median :15.00  
    ##  Mean   :17.53   Mean   :3.466            Mean   :15.53  
    ##  3rd Qu.:19.00   3rd Qu.:4.000            3rd Qu.:17.00  
    ##  Max.   :25.00   Max.   :4.000            Max.   :20.00

``` r
head(perfil.alumnos,1)
```

    ##   genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 1      2        60.09373         35.18746              70.28119           18
    ##   evalucion.socioeconomica nota.conducta
    ## 1                        4            16

``` r
perfil.alumnos$genero <- factor(perfil.alumnos$genero)
perfil.alumnos$evalucion.socioeconomica <- factor(perfil.alumnos$evalucion.socioeconomica)
perfil.alumnos$edad.ingreso <- factor(perfil.alumnos$edad.ingreso)
str(perfil.alumnos)
```

    ## 'data.frame':    1000 obs. of  7 variables:
    ##  $ genero                  : Factor w/ 2 levels "1","2": 2 2 2 1 2 2 2 2 1 2 ...
    ##  $ admision.letras         : num  60.1 59.1 53.1 57 61.5 ...
    ##  $ admision.numeros        : num  35.2 33.2 21.3 29 37.9 ...
    ##  $ promedio.preparatoria   : num  70.3 67.2 60 61 74.4 ...
    ##  $ edad.ingreso            : Factor w/ 15 levels "11","12","13",..: 8 7 5 6 8 8 5 7 4 7 ...
    ##  $ evalucion.socioeconomica: Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ nota.conducta           : num  16 15 13 14 16 16 13 15 12 15 ...

``` r
summary(perfil.alumnos)
```

    ##  genero  admision.letras admision.numeros promedio.preparatoria  edad.ingreso
    ##  1:405   Min.   :44.94   Min.   : 4.878   Min.   : 60.00        17     :200  
    ##  2:595   1st Qu.:56.61   1st Qu.:28.226   1st Qu.: 60.00        18     :167  
    ##          Median :59.98   Median :34.970   Median : 69.95        19     :166  
    ##          Mean   :60.06   Mean   :35.114   Mean   : 72.25        16     :140  
    ##          3rd Qu.:63.64   3rd Qu.:42.275   3rd Qu.: 80.91        20     :113  
    ##          Max.   :77.71   Max.   :70.411   Max.   :100.00        15     : 98  
    ##                                                                 (Other):116  
    ##  evalucion.socioeconomica nota.conducta  
    ##  1: 56                    Min.   : 9.00  
    ##  2:107                    1st Qu.:14.00  
    ##  3:152                    Median :15.00  
    ##  4:685                    Mean   :15.53  
    ##                           3rd Qu.:17.00  
    ##                           Max.   :20.00  
    ## 

### Asistencias Totales

Definicion de Valores

  - 2 -\> Asistencia
  - 1 -\> retardo
  - 0 -\> falta

<!-- end list -->

``` r
setwd(local.path)
load("AsistenciasTotales.R") #cargamos los datos de asistencias totales
class(asistencias.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(asistencias.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(asistencias.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(asistencias.totales[[1]]) # el tamano de cada matriz 32 x 54
```

    ## [1] 32 54

Asistencias del alumnos 1 en los primeros 2 semestres (1 año - 12
materias)

``` r
asistencias.totales[[1]][1:32,1:12]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
    ##  [1,]    2    2    2    2    2    2    1    2    2     2     2     2
    ##  [2,]    2    2    2    2    2    0    2    2    2     2     2     2
    ##  [3,]    2    2    2    2    2    2    2    0    2     2     2     1
    ##  [4,]    2    2    2    2    2    2    2    0    2     2     2     2
    ##  [5,]    2    1    2    1    2    1    2    2    2     2     2     2
    ##  [6,]    2    1    2    2    2    0    0    0    2     2     1     2
    ##  [7,]    1    2    2    1    2    0    2    2    2     2     2     2
    ##  [8,]    2    2    2    0    2    1    1    1    2     2     2     2
    ##  [9,]    2    2    2    0    1    2    2    0    2     2     2     2
    ## [10,]    2    2    2    2    2    2    2    0    0     2     2     2
    ## [11,]    2    2    2    1    2    2    2    2    2     2     2     2
    ## [12,]    2    2    2    2    2    0    2    2    2     2     2     2
    ## [13,]    2    2    2    2    2    2    0    2    2     2     2     2
    ## [14,]    2    2    2    0    2    2    2    0    2     2     2     2
    ## [15,]    2    2    2    2    2    1    2    2    2     2     2     2
    ## [16,]    2    2    2    2    2    2    2    1    2     2     2     2
    ## [17,]    2    1    2    0    2    2    2    2    2     2     2     2
    ## [18,]    0    2    2    2    2    2    2    2    1     2     2     2
    ## [19,]    2    2    2    0    2    2    1    2    2     2     2     2
    ## [20,]    2    2    2    1    2    1    2    0    2     2     2     2
    ## [21,]    1    2    2    1    2    0    2    2    2     2     2     2
    ## [22,]    2    2    2    0    2    1    2    2    2     2     2     2
    ## [23,]    2    2    2    2    2    2    1    0    2     2     2     2
    ## [24,]    2    2    2    2    2    2    1    2    2     2     2     2
    ## [25,]    2    2    2    2    2    1    1    0    2     2     2     2
    ## [26,]    2    2    2    1    2    2    2    1    2     2     2     2
    ## [27,]    2    2    2    2    2    2    2    2    2     2     2     2
    ## [28,]    2    2    2    1    1    0    2    0    2     2     2     2
    ## [29,]    2    1    2    2    2    2    1    0    1     2     2     2
    ## [30,]    2    2    2    2    1    2    2    1    2     2     2     2
    ## [31,]    2    2    2    2    2    2    0    2    2     2     2     2
    ## [32,]    2    2    2    2    2    2    2    0    1     2     2     2

Obtenemos las asistencias por semestre:

``` r
asistencias.totales.semestre1 <- list()
asistencias.totales.semestre2 <- list()
for(i in 1:length(asistencias.totales)){
  asistencias.totales.semestre1[[i]] <- asistencias.totales[[i]][,1:6]
  asistencias.totales.semestre2[[i]] <- asistencias.totales[[i]][,7:12]
}
```

Ejemplo de la asistencia del alumno 1 en el semestre 1

``` r
asistencias.totales.semestre1[[1]]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6]
    ##  [1,]    2    2    2    2    2    2
    ##  [2,]    2    2    2    2    2    0
    ##  [3,]    2    2    2    2    2    2
    ##  [4,]    2    2    2    2    2    2
    ##  [5,]    2    1    2    1    2    1
    ##  [6,]    2    1    2    2    2    0
    ##  [7,]    1    2    2    1    2    0
    ##  [8,]    2    2    2    0    2    1
    ##  [9,]    2    2    2    0    1    2
    ## [10,]    2    2    2    2    2    2
    ## [11,]    2    2    2    1    2    2
    ## [12,]    2    2    2    2    2    0
    ## [13,]    2    2    2    2    2    2
    ## [14,]    2    2    2    0    2    2
    ## [15,]    2    2    2    2    2    1
    ## [16,]    2    2    2    2    2    2
    ## [17,]    2    1    2    0    2    2
    ## [18,]    0    2    2    2    2    2
    ## [19,]    2    2    2    0    2    2
    ## [20,]    2    2    2    1    2    1
    ## [21,]    1    2    2    1    2    0
    ## [22,]    2    2    2    0    2    1
    ## [23,]    2    2    2    2    2    2
    ## [24,]    2    2    2    2    2    2
    ## [25,]    2    2    2    2    2    1
    ## [26,]    2    2    2    1    2    2
    ## [27,]    2    2    2    2    2    2
    ## [28,]    2    2    2    1    1    0
    ## [29,]    2    1    2    2    2    2
    ## [30,]    2    2    2    2    1    2
    ## [31,]    2    2    2    2    2    2
    ## [32,]    2    2    2    2    2    2

Ejemplo de la asistencia del alumno 1 en el semestre 2

``` r
asistencias.totales.semestre2[[1]]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6]
    ##  [1,]    1    2    2    2    2    2
    ##  [2,]    2    2    2    2    2    2
    ##  [3,]    2    0    2    2    2    1
    ##  [4,]    2    0    2    2    2    2
    ##  [5,]    2    2    2    2    2    2
    ##  [6,]    0    0    2    2    1    2
    ##  [7,]    2    2    2    2    2    2
    ##  [8,]    1    1    2    2    2    2
    ##  [9,]    2    0    2    2    2    2
    ## [10,]    2    0    0    2    2    2
    ## [11,]    2    2    2    2    2    2
    ## [12,]    2    2    2    2    2    2
    ## [13,]    0    2    2    2    2    2
    ## [14,]    2    0    2    2    2    2
    ## [15,]    2    2    2    2    2    2
    ## [16,]    2    1    2    2    2    2
    ## [17,]    2    2    2    2    2    2
    ## [18,]    2    2    1    2    2    2
    ## [19,]    1    2    2    2    2    2
    ## [20,]    2    0    2    2    2    2
    ## [21,]    2    2    2    2    2    2
    ## [22,]    2    2    2    2    2    2
    ## [23,]    1    0    2    2    2    2
    ## [24,]    1    2    2    2    2    2
    ## [25,]    1    0    2    2    2    2
    ## [26,]    2    1    2    2    2    2
    ## [27,]    2    2    2    2    2    2
    ## [28,]    2    0    2    2    2    2
    ## [29,]    1    0    1    2    2    2
    ## [30,]    2    1    2    2    2    2
    ## [31,]    0    2    2    2    2    2
    ## [32,]    2    0    1    2    2    2

Obtenemos promedio de Asistencia por materia

Aqui es visible que el alumno uno en la materia1 asistio un 93.75%

``` r
asistencias.alumnos.semestre1 <- matrix(1:6000, nrow=1000, ncol=6)
asistencias.alumnos.semestre2 <- matrix(1:6000, nrow=1000, ncol=6)
for(i in 1:length(asistencias.totales)){
  for(j in 1:6){
     asistencias.alumnos.semestre1[i,j] <- (sum(asistencias.totales.semestre1[[i]][,j])/32)/2
     asistencias.alumnos.semestre2[i,j] <- (sum(asistencias.totales.semestre2[[i]][,j])/32)/2
  }
}

#Muestra de los primeros  6 alumnos
asistencias.alumnos.semestre1[1:6,1:6]
```

    ##        [,1]     [,2]     [,3]     [,4]     [,5]    [,6]
    ## [1,] 0.9375 0.937500 1.000000 0.703125 0.953125 0.71875
    ## [2,] 0.9375 0.703125 1.000000 0.906250 0.750000 0.68750
    ## [3,] 0.9375 0.703125 1.000000 0.906250 0.687500 0.56250
    ## [4,] 0.9375 0.937500 1.000000 0.906250 0.781250 0.93750
    ## [5,] 0.9375 0.703125 0.859375 0.906250 0.953125 0.56250
    ## [6,] 0.9375 0.765625 1.000000 0.906250 0.781250 0.56250

``` r
asistencias.alumnos.semestre2[1:6,1:6]
```

    ##          [,1]    [,2]     [,3]     [,4]     [,5]     [,6]
    ## [1,] 0.796875 0.56250 0.921875 1.000000 0.984375 0.984375
    ## [2,] 0.843750 0.56250 0.921875 1.000000 0.984375 0.984375
    ## [3,] 0.843750 0.96875 0.921875 0.609375 0.984375 0.984375
    ## [4,] 0.843750 0.84375 0.828125 1.000000 0.984375 0.984375
    ## [5,] 0.843750 0.56250 0.828125 1.000000 0.984375 0.984375
    ## [6,] 0.796875 0.96875 0.921875 1.000000 0.984375 0.890625

Obtenemos los data frames para cada semestre de asistencia

Ejemplo de Etiquetado: AM1 = Asistencia Materia 1

``` r
#Semestre 1
rownames(asistencias.alumnos.semestre1) <- 1:1000
asistencias.semestre1.df <- as.data.frame(asistencias.alumnos.semestre1)
colnames(asistencias.semestre1.df) <- c('AM1','AM2','AM3','AM4','AM5','AM6') # Ejemplo: AM1 = Asistencia Materia 1
str(asistencias.semestre1.df)
```

    ## 'data.frame':    1000 obs. of  6 variables:
    ##  $ AM1: num  0.938 0.938 0.938 0.938 0.938 ...
    ##  $ AM2: num  0.938 0.703 0.703 0.938 0.703 ...
    ##  $ AM3: num  1 1 1 1 0.859 ...
    ##  $ AM4: num  0.703 0.906 0.906 0.906 0.906 ...
    ##  $ AM5: num  0.953 0.75 0.688 0.781 0.953 ...
    ##  $ AM6: num  0.719 0.688 0.562 0.938 0.562 ...

``` r
summary(asistencias.semestre1.df)
```

    ##       AM1              AM2              AM3              AM4        
    ##  Min.   :0.9375   Min.   :0.7031   Min.   :0.5156   Min.   :0.5156  
    ##  1st Qu.:0.9375   1st Qu.:0.9375   1st Qu.:0.8594   1st Qu.:0.7031  
    ##  Median :0.9375   Median :0.9375   Median :1.0000   Median :0.9062  
    ##  Mean   :0.9375   Mean   :0.8896   Mean   :0.9185   Mean   :0.8288  
    ##  3rd Qu.:0.9375   3rd Qu.:0.9375   3rd Qu.:1.0000   3rd Qu.:0.9062  
    ##  Max.   :0.9375   Max.   :0.9375   Max.   :1.0000   Max.   :0.9062  
    ##       AM5              AM6        
    ##  Min.   :0.6875   Min.   :0.5625  
    ##  1st Qu.:0.7812   1st Qu.:0.7188  
    ##  Median :0.9531   Median :0.9375  
    ##  Mean   :0.8972   Mean   :0.8598  
    ##  3rd Qu.:0.9531   3rd Qu.:0.9375  
    ##  Max.   :0.9531   Max.   :0.9375

``` r
#asistencias.semestre1.df
head(asistencias.semestre1.df,3)
```

    ##      AM1      AM2 AM3      AM4      AM5     AM6
    ## 1 0.9375 0.937500   1 0.703125 0.953125 0.71875
    ## 2 0.9375 0.703125   1 0.906250 0.750000 0.68750
    ## 3 0.9375 0.703125   1 0.906250 0.687500 0.56250

``` r
#Semestre 2
rownames(asistencias.alumnos.semestre2) <- 1:1000
asistencias.semestre2.df <- as.data.frame(asistencias.alumnos.semestre2)
colnames(asistencias.semestre2.df) <- c('AM7','AM8','AM9','AM10','AM11','AM12') # Ejemplo: AM1 = Asistencia Materia 1
str(asistencias.semestre2.df)
```

    ## 'data.frame':    1000 obs. of  6 variables:
    ##  $ AM7 : num  0.797 0.844 0.844 0.844 0.844 ...
    ##  $ AM8 : num  0.562 0.562 0.969 0.844 0.562 ...
    ##  $ AM9 : num  0.922 0.922 0.922 0.828 0.828 ...
    ##  $ AM10: num  1 1 0.609 1 1 ...
    ##  $ AM11: num  0.984 0.984 0.984 0.984 0.984 ...
    ##  $ AM12: num  0.984 0.984 0.984 0.984 0.984 ...

``` r
summary(asistencias.semestre2.df)
```

    ##       AM7              AM8              AM9              AM10       
    ##  Min.   :0.7031   Min.   :0.5625   Min.   :0.7812   Min.   :0.6094  
    ##  1st Qu.:0.7969   1st Qu.:0.9375   1st Qu.:0.8984   1st Qu.:1.0000  
    ##  Median :0.8438   Median :0.9688   Median :0.9219   Median :1.0000  
    ##  Mean   :0.8160   Mean   :0.9005   Mean   :0.8903   Mean   :0.9358  
    ##  3rd Qu.:0.8438   3rd Qu.:0.9688   3rd Qu.:0.9219   3rd Qu.:1.0000  
    ##  Max.   :0.8438   Max.   :0.9688   Max.   :0.9219   Max.   :1.0000  
    ##       AM11             AM12       
    ##  Min.   :0.7812   Min.   :0.7188  
    ##  1st Qu.:0.9844   1st Qu.:0.9844  
    ##  Median :0.9844   Median :0.9844  
    ##  Mean   :0.9462   Mean   :0.9374  
    ##  3rd Qu.:0.9844   3rd Qu.:0.9844  
    ##  Max.   :0.9844   Max.   :0.9844

``` r
#asistencias.semestre2.df
head(asistencias.semestre2.df,3)
```

    ##        AM7     AM8      AM9     AM10     AM11     AM12
    ## 1 0.796875 0.56250 0.921875 1.000000 0.984375 0.984375
    ## 2 0.843750 0.56250 0.921875 1.000000 0.984375 0.984375
    ## 3 0.843750 0.96875 0.921875 0.609375 0.984375 0.984375

Dataframe para el promedio de asistencias totales por semestre y en el
año del alumno

``` r
asistencias.alumnos.semestres <- matrix(1:2000, nrow=1000, ncol=2)
asistencias.totales.df <-  matrix(1:1000, nrow=1000, ncol=1) #para las totales Anual
for(i in 1:length(asistencias.totales)){
   asistencias.alumnos.semestres[i,1] <- sum(asistencias.alumnos.semestre1[i,1:6])/6
   asistencias.alumnos.semestres[i,2] <- sum(asistencias.alumnos.semestre2[i,1:6])/6
   asistencias.totales.df[i] <- sum(asistencias.alumnos.semestres[i,1:2])/2
}

#Muestra del promedio de asistencia de los dos semestres de los primeros dos alumnos
asistencias.alumnos.semestres[1:2,1:2]
```

    ##           [,1]      [,2]
    ## [1,] 0.8750000 0.8750000
    ## [2,] 0.8307292 0.8828125

Si tomamos el alumno 1 vemos que en promedio en el primer semestre el
alumnos asistio 87.5 % a clases.

``` r
rownames(asistencias.alumnos.semestres) <- 1:1000
asistencias.semestres.df <- as.data.frame(asistencias.alumnos.semestres)
colnames(asistencias.semestres.df) <- c("Asist.Sem1","Asist.Sem2")
str(asistencias.semestres.df)
```

    ## 'data.frame':    1000 obs. of  2 variables:
    ##  $ Asist.Sem1: num  0.875 0.831 0.799 0.917 0.82 ...
    ##  $ Asist.Sem2: num  0.875 0.883 0.885 0.914 0.867 ...

``` r
summary(asistencias.semestres.df)
```

    ##    Asist.Sem1       Asist.Sem2    
    ##  Min.   :0.6536   Min.   :0.7161  
    ##  1st Qu.:0.8568   1st Qu.:0.8770  
    ##  Median :0.9089   Median :0.9193  
    ##  Mean   :0.8886   Mean   :0.9044  
    ##  3rd Qu.:0.9453   3rd Qu.:0.9505  
    ##  Max.   :0.9453   Max.   :0.9505

``` r
#asistencias.semestres.df
head(asistencias.semestres.df,3)
```

    ##   Asist.Sem1 Asist.Sem2
    ## 1  0.8750000  0.8750000
    ## 2  0.8307292  0.8828125
    ## 3  0.7994792  0.8854167

``` r
#Para el Total
rownames(asistencias.totales.df) <- 1:1000
asistencias.totales.df <- as.data.frame(asistencias.totales.df)
colnames(asistencias.totales.df) <- c("Asist.Total")
str(asistencias.totales.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ Asist.Total: num  0.875 0.857 0.842 0.915 0.844 ...

``` r
summary(asistencias.totales.df)
```

    ##   Asist.Total    
    ##  Min.   :0.7161  
    ##  1st Qu.:0.8646  
    ##  Median :0.9115  
    ##  Mean   :0.8965  
    ##  3rd Qu.:0.9362  
    ##  Max.   :0.9479

``` r
#asistencias.totales.df
head(asistencias.totales.df,3)
```

    ##   Asist.Total
    ## 1   0.8750000
    ## 2   0.8567708
    ## 3   0.8424479

### Resultados Examenes

Definicion de los Valores:

  - Mejor Calificacion 20
  - Menos Calificacion 1

<!-- end list -->

``` r
setwd(local.path)
load("ResultadosExamenes.R") # cargamos resultados totales de los Examenes
class(resultados.examenes.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(resultados.examenes.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(resultados.examenes.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(resultados.examenes.totales[[1]]) # el tamano de cada matriz 2 x 54
```

    ## [1]  2 54

Ejemplo del alumno 1000: se tuvieron 2 examenes por
    materia.

``` r
resultados.examenes.totales[[1000]]
```

    ##          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
    ## [1,] 11.79653 14.73953 11.50412 16.34320 11.60064 12.81880 17.95564 12.39889
    ## [2,] 12.11637 16.80950 13.42255 14.03578 13.05566 13.81293 15.59098 11.62347
    ##         [,9]    [,10]    [,11]    [,12]    [,13]    [,14]    [,15]    [,16]
    ## [1,] 11.6648 12.52243 11.83175 14.27744 13.13097 15.01613 16.40846 13.04933
    ## [2,] 11.0727 11.92031 11.00155 17.27110 11.73841 16.55131 14.78018 11.73235
    ##         [,17]    [,18]    [,19]    [,20]    [,21]    [,22]    [,23]    [,24]
    ## [1,] 11.46515 17.29159 11.35139 13.63256 13.35834 15.21711 12.72981 11.87772
    ## [2,] 13.90514 16.84080 12.45209 13.30560 11.75734 15.89896 11.66922 11.67467
    ##         [,25]    [,26]    [,27]    [,28]    [,29]    [,30]    [,31]    [,32]
    ## [1,] 12.24836 14.06637 13.91525 14.11430 11.29904 11.29635 12.56647 12.51752
    ## [2,] 13.08429 15.15791 11.25127 14.35348 11.72271 12.46470 13.84987 12.78443
    ##         [,33]     [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
    ## [1,] 12.33782  8.223843 13.56977 16.48951 12.64892 15.59962 12.27918 13.05075
    ## [2,] 12.18395 10.992702 11.03248 16.70199 11.23651 14.08107 11.61902 13.61871
    ##         [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]
    ## [1,] 18.42698 13.74442 12.45511 16.97367 16.53349 14.73739 17.90785 12.73750
    ## [2,] 19.94246 13.81123 13.73230 15.13149 15.27015 14.97345 15.49566 11.10149
    ##         [,49]    [,50]    [,51]    [,52]    [,53]    [,54]
    ## [1,] 12.09710 13.12618 17.10390 14.64808 12.73757 13.91061
    ## [2,] 12.46356 12.31298 14.80188 17.87997 11.35046 13.00235

Obtendremos el dataframe para el promedio de los examenes

``` r
promedio.examenes.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) {
  promedio.examenes.mtx[i] <- mean(resultados.examenes.totales[[i]][,1:12]) 
}
rownames(promedio.examenes.mtx) <- 1:1000
promedio.examenes.df <- as.data.frame(promedio.examenes.mtx)
colnames(promedio.examenes.df) <- c("prom.exam")
str(promedio.examenes.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.exam: num  12.5 13.1 12.5 14.2 12.4 ...

``` r
summary(promedio.examenes.df)
```

    ##    prom.exam    
    ##  Min.   :11.54  
    ##  1st Qu.:12.60  
    ##  Median :13.33  
    ##  Mean   :13.43  
    ##  3rd Qu.:14.12  
    ##  Max.   :16.23

``` r
#promedio.examenes.df
head(promedio.examenes.df,3)
```

    ##   prom.exam
    ## 1  12.51429
    ## 2  13.10560
    ## 3  12.51326

### Resultado de Trabajos

Definicion de los Valores:

  - Mejor Calificacion 20
  - Menos Calificacion 1

<!-- end list -->

``` r
setwd(local.path)
load("ResultadoTrabajos.R")# cargamos resultados totales de los Trabajos
class(resultados.trabajos.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(resultados.trabajos.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(resultados.trabajos.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(resultados.trabajos.totales[[1]]) # el tamano de cada matriz 4 x 54
```

    ## [1]  4 54

si tomamos el primer alumno podemos ver que fueron evaluados 4 trabajos
por
    materia.

``` r
resultados.trabajos.totales[[1]]
```

    ##          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]      [,7]     [,8]
    ## [1,] 11.79653 11.55465 11.50412 16.34320 11.60064 16.42507 10.944546 12.39889
    ## [2,] 12.11637 13.10712 13.42255 14.03578 13.05566 17.75057  7.988727 11.62347
    ## [3,] 12.71856 12.71998 12.15483 15.17496 13.75063 15.05741  6.578489 13.39897
    ## [4,] 13.72462 11.50416 11.98320 15.10950 11.85320 15.52038  6.348743 12.95561
    ##          [,9]    [,10]    [,11]    [,12]    [,13]    [,14]    [,15]    [,16]
    ## [1,] 11.66480 12.52243 11.83175 11.20808 16.84129 11.76210 16.40846 13.04933
    ## [2,] 11.07270 11.92031 11.00155 13.45333 14.98455 12.91348 14.78018 11.73235
    ## [3,] 11.62136 12.28072 12.53183 13.82787 15.55854 13.87157 17.86583 12.35033
    ## [4,] 11.64720 13.07931 11.04214 11.80815 14.36553 12.65764 16.60362 11.68831
    ##         [,17]    [,18]    [,19]    [,20]    [,21]    [,22]    [,23]    [,24]
    ## [1,] 14.62020 19.64579 11.35139 17.51009 9.930575 11.91283 12.72981 11.87772
    ## [2,] 17.87352 19.42040 12.45209 17.07413 7.262228 12.42422 11.66922 11.67467
    ## [3,] 15.87305 19.93167 12.95362 15.11585 9.496262 13.98058 11.99569 13.11267
    ## [4,] 17.10728 18.15722 11.20519 16.11665 6.922304 12.56196 13.13217 12.55669
    ##         [,25]    [,26]    [,27]    [,28]    [,29]    [,30]    [,31]    [,32]
    ## [1,] 12.24836 11.04978 13.91525 11.08573 14.39872 11.29635 12.56647 12.51752
    ## [2,] 13.08429 11.86843 11.25127 11.26511 14.96361 12.46470 13.84987 12.78443
    ## [3,] 11.44640 13.62348 13.62161 12.42310 14.41286 12.09211 12.28074 13.42624
    ## [4,] 13.69216 13.39977 11.98769 13.65613 15.30234 12.26186 12.27329 13.18646
    ##         [,33]    [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
    ## [1,] 12.33782 12.33431 17.42636 12.86713 16.19856 12.19972 15.70557 13.05075
    ## [2,] 12.18395 13.99562 14.04330 13.02650 14.31535 11.06080 14.82536 13.61871
    ## [3,] 12.45119 13.65468 16.21143 13.40687 16.59519 11.43672 14.43796 13.07035
    ## [4,] 13.75663 11.71528 17.80082 11.78092 15.98741 12.13254 14.31883 11.34781
    ##         [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]
    ## [1,] 14.85396 13.74442 15.94015 13.23025 12.90012 14.73739 13.93089 12.73750
    ## [2,] 17.88491 13.81123 17.64306 11.84861 11.95261 14.97345 12.12175 11.10149
    ## [3,] 16.31275 11.85842 14.23070 12.52280 11.72277 16.33599 13.28451 11.00815
    ## [4,] 14.56832 13.49134 16.82159 12.72999 12.13524 15.38252 13.46747 12.13199
    ##         [,49]    [,50]    [,51]    [,52]    [,53]    [,54]
    ## [1,] 15.46280 16.83491 13.32793 14.64808 12.73757 13.91061
    ## [2,] 15.95142 15.75064 11.60141 17.87997 11.35046 13.00235
    ## [3,] 15.07924 14.80002 11.89485 15.38590 11.27255 13.06866
    ## [4,] 16.42729 17.06826 13.95523 14.08316 12.83598 11.58436

ahora obtendremos el dataframe para el promedio de los trabajos

``` r
promedio.trabajos.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) {
  promedio.trabajos.mtx[i] <- mean(resultados.trabajos.totales[[i]][,1:12]) 
}
rownames(promedio.trabajos.mtx) <- 1:1000
promedio.trabajos.df <- as.data.frame(promedio.trabajos.mtx)
colnames(promedio.trabajos.df) <- c("prom.trab")
str(promedio.trabajos.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.trab: num  12.5 13.1 12.5 14.2 12.4 ...

``` r
summary(promedio.trabajos.df)
```

    ##    prom.trab    
    ##  Min.   :11.55  
    ##  1st Qu.:12.62  
    ##  Median :13.35  
    ##  Mean   :13.46  
    ##  3rd Qu.:14.15  
    ##  Max.   :16.22

``` r
#promedio.trabajos.df
head(promedio.trabajos.df,3)
```

    ##   prom.trab
    ## 1  12.47364
    ## 2  13.13827
    ## 3  12.53885

### Uso de Biblioteca

Uso físico y virtual. vector. 1000 Matrices, número de veces que asistio
a la biblioteca por materia.

``` r
setwd(local.path)
load("UsoBiblioteca.R")# cargamos usos de biblioteca
class(uso.biblioteca.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(uso.biblioteca.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(uso.biblioteca.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(uso.biblioteca.totales[[1]]) # el tamano de cada matriz 1 x 54
```

    ## [1]  1 54

Seleccionamos los datos del primer alumnos, podemos ver que es la
frecuencia de visitas, por lo que tendremos que
    redondear.

``` r
uso.biblioteca.totales[[1]]
```

    ##          [,1]     [,2]     [,3]   [,4]     [,5]     [,6]     [,7]     [,8]
    ## [1,] 12.65509 11.84882 11.68042 33.787 12.00214 34.09402 2.977819 14.66295
    ##          [,9]    [,10]   [,11]    [,12]    [,13]    [,14]    [,15]   [,16]
    ## [1,] 12.21601 15.07478 12.7725 10.69361 35.65484 12.54034 34.03171 16.8311
    ##         [,17]    [,18]    [,19]    [,20]   [,21]    [,22]    [,23]    [,24]
    ## [1,] 27.32576 68.22896 11.17131 38.16282 2.57223 13.04277 15.76604 12.92574
    ##         [,25]    [,26]   [,27]    [,28]    [,29]    [,30]    [,31]    [,32]
    ## [1,] 14.16118 10.16592 19.7175 10.28575 26.49522 10.98783 15.22158 15.05841
    ##        [,33]    [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
    ## [1,] 14.4594 14.44769 37.84886 16.22378 33.24461 13.99906 31.39589 16.83582
    ##         [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]
    ## [1,] 28.20236 19.14806 32.27557 17.43418 16.33373 27.76523 19.76962 15.79166
    ##         [,49]    [,50]    [,51]   [,52]    [,53]    [,54]
    ## [1,] 30.48549 35.63091 17.75975 27.4303 15.79189 19.70203

Obtendremos el dataframe para el uso de biblioteca.

``` r
promedio.uso.biblio.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) {
  redondeo <- matrix(1:12,nrow=1,ncol=12)
  for(j in 1:12){
    redondeo[j] <- round(uso.biblioteca.totales[[i]][,j],0)
  }
  #print(redondeo)
  promedio.uso.biblio.mtx[i] <- mean(redondeo) 
}
rownames(promedio.uso.biblio.mtx) <- 1:1000
promedio.uso.biblio.df <- as.data.frame(promedio.uso.biblio.mtx)
colnames(promedio.uso.biblio.df) <- c("prom.uso.biblio")
str(promedio.uso.biblio.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.uso.biblio: num  15.5 18.2 15.2 25.4 15.8 ...

``` r
summary(promedio.uso.biblio.df)
```

    ##  prom.uso.biblio
    ##  Min.   :11.58  
    ##  1st Qu.:16.75  
    ##  Median :20.92  
    ##  Mean   :21.67  
    ##  3rd Qu.:25.67  
    ##  Max.   :39.67

``` r
#promedio.uso.biblio.df
head(promedio.uso.biblio.df,3)
```

    ##   prom.uso.biblio
    ## 1        15.50000
    ## 2        18.16667
    ## 3        15.25000

Para determinar el uso significativo de ese promedio de uso. Es decir si
el uso es 12 esta muy bien, seria el maximo. Si seleccionamos el primer
alumno vemos que solo visito 2 veces de manera significativa la
biblioteca.

``` r
suma <- 0
for (i in 1:1000) {
  suma <- suma + round(sum(uso.biblioteca.totales[[i]][,1:12]))
}
suma
```

    ## [1] 258812

``` r
uso.anual<- 258
uso.por.materia <- 258/12 #nos servira como umbral
uso.por.materia 
```

    ## [1] 21.5

``` r
uso.biblioteca.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) { # determinaremos cuantas veces la uso de manera significativa el alumno
  uso.biblioteca.mtx[i] <- sum(uso.biblioteca.totales[[i]][,1:12]>uso.por.materia)
}
uso.biblioteca.mtx[[1]]
```

    ## [1] 2

``` r
rownames(uso.biblioteca.mtx) <- 1:1000
uso.biblioteca.df <- as.data.frame(uso.biblioteca.mtx)
colnames(uso.biblioteca.df) <- c("uso.biblio")
str(uso.biblioteca.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ uso.biblio: int  2 3 1 7 2 5 11 6 1 7 ...

``` r
summary(uso.biblioteca.df)
```

    ##    uso.biblio    
    ##  Min.   : 0.000  
    ##  1st Qu.: 2.000  
    ##  Median : 5.000  
    ##  Mean   : 5.042  
    ##  3rd Qu.: 8.000  
    ##  Max.   :12.000

``` r
#uso.biblioteca.df
head(uso.biblioteca.df,3)
```

    ##   uso.biblio
    ## 1          2
    ## 2          3
    ## 3          1

``` r
uso.biblioteca.df$uso.biblio <- as.numeric(uso.biblioteca.df$uso.biblio)
str(uso.biblioteca.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ uso.biblio: num  2 3 1 7 2 5 11 6 1 7 ...

``` r
summary(uso.biblioteca.df)
```

    ##    uso.biblio    
    ##  Min.   : 0.000  
    ##  1st Qu.: 2.000  
    ##  Median : 5.000  
    ##  Mean   : 5.042  
    ##  3rd Qu.: 8.000  
    ##  Max.   :12.000

### Uso de la Plataforma

Redondear, vector.

``` r
setwd(local.path)
load("UsoPlataforma.R") #cargamos uso de la plataforma
class(uso.plataforma.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(uso.plataforma.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(uso.plataforma.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(uso.plataforma.totales[[1]]) # el tamano de cada matriz 1 x 54
```

    ## [1]  1 54

Seleccionamos los datos del primer alumnos, podemos ver que es la
frecuencia de visitas a la plataforma, por lo que tendremos que
redondear.

``` r
uso.plataforma.totales[[1]]
```

    ##          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
    ## [1,] 32.79653 32.55465 32.50412 79.29002 32.60064 80.31341 5.944546 33.39889
    ##         [,9]    [,10]    [,11]    [,12]    [,13]   [,14]   [,15]    [,16]
    ## [1,] 32.6648 33.52243 32.83175 32.20808 85.51612 32.7621 80.1057 34.04933
    ##         [,17]    [,18]    [,19]    [,20]    [,21]    [,22]    [,23]    [,24]
    ## [1,] 57.75254 191.1448 32.35139 93.87607 4.930575 32.91283 33.72981 32.87772
    ##         [,25]    [,26]    [,27]    [,28]    [,29]    [,30]    [,31]    [,32]
    ## [1,] 33.24836 32.04978 34.91525 32.08573 54.98405 32.29635 33.56647 33.51752
    ##         [,33]    [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
    ## [1,] 33.33782 33.33431 92.82952 33.86713 77.48204 33.19972 71.31963 34.05075
    ##         [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]   [,48]
    ## [1,] 60.67453 34.74442 74.25188 34.23025 33.90012 59.21743 34.93089 33.7375
    ##         [,49]    [,50]    [,51]    [,52]    [,53]    [,54]
    ## [1,] 68.28495 85.43635 34.32793 58.10099 33.73757 34.91061

Obtendremos el dataframe para el uso de la plataforma.

``` r
promedio.uso.plataforma.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) {
  redondeo <- matrix(1:12,nrow=1,ncol=12)
  for(j in 1:12){
    redondeo[j] <- round(uso.plataforma.totales[[i]][,j],0)
  }
  #print(redondeo)
  promedio.uso.plataforma.mtx[i] <- mean(redondeo) 
}
promedio.uso.plataforma.mtx[[1]]
```

    ## [1] 38.5

``` r
rownames(promedio.uso.plataforma.mtx) <- 1:1000
promedio.uso.plataforma.df <- as.data.frame(promedio.uso.plataforma.mtx)
colnames(promedio.uso.plataforma.df) <- c("prom.uso.platf")
str(promedio.uso.plataforma.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.uso.platf: num  38.5 42.8 35.5 58.5 36.2 ...

``` r
summary(promedio.uso.plataforma.df)
```

    ##  prom.uso.platf 
    ##  Min.   :28.17  
    ##  1st Qu.:39.08  
    ##  Median :48.62  
    ##  Mean   :50.94  
    ##  3rd Qu.:60.29  
    ##  Max.   :96.83

``` r
#promedio.uso.plataforma.df
head(promedio.uso.plataforma.df,3)
```

    ##   prom.uso.platf
    ## 1          38.50
    ## 2          42.75
    ## 3          35.50

``` r
promedio.uso.plataforma.df$prom.uso.platf <- as.numeric(promedio.uso.plataforma.df$prom.uso.platf)
str(promedio.uso.plataforma.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.uso.platf: num  38.5 42.8 35.5 58.5 36.2 ...

``` r
summary(promedio.uso.plataforma.df)
```

    ##  prom.uso.platf 
    ##  Min.   :28.17  
    ##  1st Qu.:39.08  
    ##  Median :48.62  
    ##  Mean   :50.94  
    ##  3rd Qu.:60.29  
    ##  Max.   :96.83

Para determinar el uso significativo de ese promedio de uso. Es decir si
el uso es 12 esta muy bien, seria el maximo. Si seleccionamos el primer
alumno vemos que solo uso 2 veces de manera significativa la plataforma.

``` r
suma <- 0
for (i in 1:1000) {
  suma <- suma + round(sum(uso.plataforma.totales[[i]][,1:12]))
}
suma
```

    ## [1] 611490

``` r
uso.anual<- 612
uso.por.materia <- 612/12 #nos servira como umbral
uso.por.materia 
```

    ## [1] 51

``` r
uso.plataforma.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) { # determinaremos cuantas veces la uso de manera significativa el alumno
  uso.plataforma.mtx[i] <- sum(uso.plataforma.totales[[i]][,1:12]>uso.por.materia)
}
uso.plataforma.mtx[[1]]
```

    ## [1] 2

``` r
rownames(uso.plataforma.mtx) <- 1:1000
uso.plataforma.df <- as.data.frame(uso.plataforma.mtx)
colnames(uso.plataforma.df) <- c("uso.platf")
str(uso.plataforma.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ uso.platf: int  2 3 1 7 2 5 11 6 1 7 ...

``` r
summary(uso.plataforma.df)
```

    ##    uso.platf     
    ##  Min.   : 0.000  
    ##  1st Qu.: 2.000  
    ##  Median : 5.000  
    ##  Mean   : 5.042  
    ##  3rd Qu.: 8.000  
    ##  Max.   :12.000

``` r
#uso.plataforma.df
head(uso.plataforma.df,3)
```

    ##   uso.platf
    ## 1         2
    ## 2         3
    ## 3         1

``` r
uso.plataforma.df$uso.platf <- as.numeric(uso.plataforma.df$uso.platf)
str(uso.plataforma.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ uso.platf: num  2 3 1 7 2 5 11 6 1 7 ...

``` r
summary(uso.plataforma.df)
```

    ##    uso.platf     
    ##  Min.   : 0.000  
    ##  1st Qu.: 2.000  
    ##  Median : 5.000  
    ##  Mean   : 5.042  
    ##  3rd Qu.: 8.000  
    ##  Max.   :12.000

### Apartado de Libros

Numero de libros/articulos digitales que se apartaron por materia.

``` r
setwd(local.path)
load("ApartadoDeLibros.R")# cargamos apartado de libros
class(separacion.libros.totales) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(separacion.libros.totales) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(separacion.libros.totales[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(separacion.libros.totales[[1]]) # el tamano de cada matriz 1 x 54
```

    ## [1]  1 54

``` r
separacion.libros.totales[[1]]
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
    ## [1,]    1    1    1    3    1    3    0    1    1     1     1     1     3     1
    ##      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
    ## [1,]     3     1     2     5     1     3     0     1     1     1     1     1
    ##      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
    ## [1,]     1     1     2     1     1     1     1     1     3     1     3     1
    ##      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
    ## [1,]     2     1     2     1     2     1     1     2     1     1     2     3
    ##      [,51] [,52] [,53] [,54]
    ## [1,]     1     2     1     1

Obtendremos el dataframe para el promedio del apartado de libros.

``` r
separacion.libros.totales.mtx <- matrix(1:1000,nrow=1000,ncol=1)
for (i in 1:1000) {
  separacion.libros.totales.mtx[i] <- mean(separacion.libros.totales[[i]][,1:12]) 
}
rownames(separacion.libros.totales.mtx) <- 1:1000
apartado.libros.df <- as.data.frame(separacion.libros.totales.mtx)
colnames(apartado.libros.df) <- c("prom.apartado.libros")
str(apartado.libros.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.apartado.libros: num  1.25 1.33 1.08 1.92 1.08 ...

``` r
summary(apartado.libros.df)
```

    ##  prom.apartado.libros
    ##  Min.   :0.8333      
    ##  1st Qu.:1.2500      
    ##  Median :1.5000      
    ##  Mean   :1.5826      
    ##  3rd Qu.:1.9167      
    ##  Max.   :2.8333

``` r
#apartado.libros.df
head(apartado.libros.df,3)
```

    ##   prom.apartado.libros
    ## 1             1.250000
    ## 2             1.333333
    ## 3             1.083333

### Becas

Definicion: \* 1 Becado

  - 0 No becado

<!-- end list -->

``` r
setwd(local.path)
load("Becas.R") #cargamos Becas
class(distribucion.becas) # para saber de que tipo es, en este caso es un vector numerico
```

    ## [1] "numeric"

``` r
length(distribucion.becas) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(distribucion.becas[[1]]) # para obtener que tipo es el objecto dentro del vector que es un numero
```

    ## [1] "numeric"

``` r
sum(distribucion.becas[[1]]) 
```

    ## [1] 0

``` r
becas.alumnos <- matrix(1:1000, nrow=1000, ncol=1)

for(i in 1:1000){
    becas.alumnos[i] <- distribucion.becas[i]
}
rownames(becas.alumnos)  <- 1:1000
becas.alumnos.df <- as.data.frame(becas.alumnos)
colnames(becas.alumnos.df) <- c('beca')
becas.alumnos.df$beca <- as.factor(becas.alumnos.df$beca)
#becas.alumnos.df
head(becas.alumnos.df,3)
```

    ##   beca
    ## 1    0
    ## 2    0
    ## 3    0

### Historial de Pagos

Definicion:

  - 2 pago a tiempo
  - 1 pago tarde
  - 0 no pago

<!-- end list -->

``` r
setwd(local.path)
load("HistorialPagos.R") # cargamos pagos
class(registro.pagos) # para saber de que tipo es, en este caso es una lista
```

    ## [1] "list"

``` r
length(registro.pagos) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(registro.pagos[[1]]) # para obtener que tipo es el objecto dentro de la lista, es matriz, por lo tanto tenemos una lista de matrices
```

    ## [1] "matrix"

``` r
dim(registro.pagos[[1]]) # la matriz es de una tamano de 4x9
```

    ## [1] 4 9

Es decir cada alumno realiza 4 pagos por semestre, por lo que nosotros
solo estaremos interesados en las primeras dos columnas.

``` r
registro.pagos[[1]]
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    2    2    2    2    2    1    0    2    2
    ## [2,]    2    2    2    2    2    0    2    2    2
    ## [3,]    2    2    2    2    2    2    2    2    2
    ## [4,]    2    2    2    2    2    2    2    2    2

Obtendremos el promedio de los pagos respecto un umbral Obtendremos el
datframe para pagos

``` r
promedio.pagos.mtx <- matrix(,nrow=0,ncol=1)
for (i in 1:1000) {
  datos.pagos <- registro.pagos[[i]]
  pagos.vct <- vector()
  pagos.vct[1] <- mean(datos.pagos[,1:2]) 
  promedio.pagos.mtx <- rbind(promedio.pagos.mtx,
                              pagos.vct)
}
class(promedio.pagos.mtx)
```

    ## [1] "matrix"

``` r
rownames(promedio.pagos.mtx) <- 1:1000
#podremos un umbral
promedio.pagos.mtx[promedio.pagos.mtx < 1.875] <- 0
```

``` r
promedio.pagos.df <- as.data.frame(promedio.pagos.mtx)
colnames(promedio.pagos.df) <- c("prom.pagos")
str(promedio.pagos.df)
```

    ## 'data.frame':    1000 obs. of  1 variable:
    ##  $ prom.pagos: num  2 0 0 2 2 0 0 2 2 2 ...

``` r
summary(promedio.pagos.df)
```

    ##    prom.pagos   
    ##  Min.   :0.000  
    ##  1st Qu.:0.000  
    ##  Median :1.875  
    ##  Mean   :1.415  
    ##  3rd Qu.:2.000  
    ##  Max.   :2.000

``` r
#promedio.pagos.df
head(promedio.pagos.df,3)
```

    ##   prom.pagos
    ## 1          2
    ## 2          0
    ## 3          0

### Cambio de Carrera

Definicion:

  - 1 se cambio, estuvo en una carrera anterior y ya lleva los dos
    semestres que estamos evaluando.
  - 0 no sa ha cambiado

Esta es muy importate porque indica inestabilidad en el alumno,debe
considerarse como que ya deserto una vez

``` r
setwd(local.path)
load("CambioCarrera.R") # cargamos Cambio de carrera
head(cambio.carrera)
```

    ## [1] 0 0 0 0 0 0

``` r
class(cambio.carrera) # para saber de que tipo es, en este caso es un vector numerico
```

    ## [1] "numeric"

``` r
length(cambio.carrera) # tamano del objecto, en este caso son 1000 Alumnos
```

    ## [1] 1000

``` r
class(cambio.carrera[[1]]) # para obtener que tipo es el objecto dentro del vector que es un numero
```

    ## [1] "numeric"

``` r
sum(cambio.carrera[[1]]) 
```

    ## [1] 0

``` r
cambio.carrera.mtx <- matrix(1:1000, nrow=1000, ncol=1)

for(i in 1:1000){
   cambio.carrera.mtx[i] <- cambio.carrera[i]
}
rownames(cambio.carrera.mtx) <- 1:1000
cambio.carrera.df <- as.data.frame(cambio.carrera.mtx)
colnames(cambio.carrera.df) <- c('cambio.carrera')
cambio.carrera.df$cambio.carrera <- as.factor(cambio.carrera.df$cambio.carrera)
#cambio.carrera.df
head(cambio.carrera.df,3)
```

    ##   cambio.carrera
    ## 1              0
    ## 2              0
    ## 3              0

\#\#Integracion de los datos

Juntamos todos los datos en un solo dataframe integrado.

``` r
datos.integrados.df <- cbind.data.frame(perfil.alumnos,
                                     becas.alumnos.df,
                              asistencias.totales.df,
                              promedio.trabajos.df,
                              promedio.examenes.df,
                              promedio.pagos.df,
                              #promedio.uso.biblio.df,
                              uso.biblioteca.df,
                              #promedio.uso.plataforma.df,
                              uso.plataforma.df,
                              apartado.libros.df,
                              cambio.carrera.df)
str(datos.integrados.df)
```

    ## 'data.frame':    1000 obs. of  16 variables:
    ##  $ genero                  : Factor w/ 2 levels "1","2": 2 2 2 1 2 2 2 2 1 2 ...
    ##  $ admision.letras         : num  60.1 59.1 53.1 57 61.5 ...
    ##  $ admision.numeros        : num  35.2 33.2 21.3 29 37.9 ...
    ##  $ promedio.preparatoria   : num  70.3 67.2 60 61 74.4 ...
    ##  $ edad.ingreso            : Factor w/ 15 levels "11","12","13",..: 8 7 5 6 8 8 5 7 4 7 ...
    ##  $ evalucion.socioeconomica: Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ nota.conducta           : num  16 15 13 14 16 16 13 15 12 15 ...
    ##  $ beca                    : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Asist.Total             : num  0.875 0.857 0.842 0.915 0.844 ...
    ##  $ prom.trab               : num  12.5 13.1 12.5 14.2 12.4 ...
    ##  $ prom.exam               : num  12.5 13.1 12.5 14.2 12.4 ...
    ##  $ prom.pagos              : num  2 0 0 2 2 0 0 2 2 2 ...
    ##  $ uso.biblio              : num  2 3 1 7 2 5 11 6 1 7 ...
    ##  $ uso.platf               : num  2 3 1 7 2 5 11 6 1 7 ...
    ##  $ prom.apartado.libros    : num  1.25 1.33 1.08 1.92 1.08 ...
    ##  $ cambio.carrera          : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...

``` r
summary(datos.integrados.df)
```

    ##  genero  admision.letras admision.numeros promedio.preparatoria  edad.ingreso
    ##  1:405   Min.   :44.94   Min.   : 4.878   Min.   : 60.00        17     :200  
    ##  2:595   1st Qu.:56.61   1st Qu.:28.226   1st Qu.: 60.00        18     :167  
    ##          Median :59.98   Median :34.970   Median : 69.95        19     :166  
    ##          Mean   :60.06   Mean   :35.114   Mean   : 72.25        16     :140  
    ##          3rd Qu.:63.64   3rd Qu.:42.275   3rd Qu.: 80.91        20     :113  
    ##          Max.   :77.71   Max.   :70.411   Max.   :100.00        15     : 98  
    ##                                                                 (Other):116  
    ##  evalucion.socioeconomica nota.conducta   beca     Asist.Total    
    ##  1: 56                    Min.   : 9.00   0:837   Min.   :0.7161  
    ##  2:107                    1st Qu.:14.00   1:163   1st Qu.:0.8646  
    ##  3:152                    Median :15.00           Median :0.9115  
    ##  4:685                    Mean   :15.53           Mean   :0.8965  
    ##                           3rd Qu.:17.00           3rd Qu.:0.9362  
    ##                           Max.   :20.00           Max.   :0.9479  
    ##                                                                   
    ##    prom.trab       prom.exam       prom.pagos      uso.biblio    
    ##  Min.   :11.55   Min.   :11.54   Min.   :0.000   Min.   : 0.000  
    ##  1st Qu.:12.62   1st Qu.:12.60   1st Qu.:0.000   1st Qu.: 2.000  
    ##  Median :13.35   Median :13.33   Median :1.875   Median : 5.000  
    ##  Mean   :13.46   Mean   :13.43   Mean   :1.415   Mean   : 5.042  
    ##  3rd Qu.:14.15   3rd Qu.:14.12   3rd Qu.:2.000   3rd Qu.: 8.000  
    ##  Max.   :16.22   Max.   :16.23   Max.   :2.000   Max.   :12.000  
    ##                                                                  
    ##    uso.platf      prom.apartado.libros cambio.carrera
    ##  Min.   : 0.000   Min.   :0.8333       0:900         
    ##  1st Qu.: 2.000   1st Qu.:1.2500       1:100         
    ##  Median : 5.000   Median :1.5000                     
    ##  Mean   : 5.042   Mean   :1.5826                     
    ##  3rd Qu.: 8.000   3rd Qu.:1.9167                     
    ##  Max.   :12.000   Max.   :2.8333                     
    ## 

``` r
#datos.integrados.df
head(datos.integrados.df,3)
```

    ##   genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 1      2        60.09373         35.18746              70.28119           18
    ## 2      2        59.07874         33.15747              67.23621           17
    ## 3      2        53.14335         21.28669              60.00000           15
    ##   evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 1                        4            16    0   0.8750000  12.47364  12.51429
    ## 2                        4            15    0   0.8567708  13.13827  13.10560
    ## 3                        4            13    0   0.8424479  12.53885  12.51326
    ##   prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 1          2          2         2             1.250000              0
    ## 2          0          3         3             1.333333              0
    ## 3          0          1         1             1.083333              0

Guardamos los Datos para el siguiente paso

``` r
setwd(local.path)
save(datos.integrados.df, file="datos.integrados.R")
getwd()
```

    ## [1] "/Users/akcasill/Documents/analisisDatos/proyecto/mcc-analisisdatos-final/data"

``` r
load("datos.integrados.R")
#datos.integrados.df
head(datos.integrados.df,3)
```

    ##   genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 1      2        60.09373         35.18746              70.28119           18
    ## 2      2        59.07874         33.15747              67.23621           17
    ## 3      2        53.14335         21.28669              60.00000           15
    ##   evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 1                        4            16    0   0.8750000  12.47364  12.51429
    ## 2                        4            15    0   0.8567708  13.13827  13.10560
    ## 3                        4            13    0   0.8424479  12.53885  12.51326
    ##   prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 1          2          2         2             1.250000              0
    ## 2          0          3         3             1.333333              0
    ## 3          0          1         1             1.083333              0

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
