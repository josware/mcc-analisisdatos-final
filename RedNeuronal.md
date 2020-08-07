Proyecto Final - Red neuronal
================
Casillas, A., González, L., Gómez, J.
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
    ## 1      2        60.09373         35.18746              70.28119            8
    ## 2      2        59.07874         33.15747              67.23621            7
    ## 3      2        53.14335         21.28669              60.00000            5
    ##   evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 1                        4            16    1   0.8750000  12.47364  12.51429
    ## 2                        4            15    1   0.8567708  13.13827  13.10560
    ## 3                        4            13    1   0.8424479  12.53885  12.51326
    ##   prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera desercion
    ## 1          2          2         2             1.250000              1         1
    ## 2          0          3         3             1.333333              1         0
    ## 3          0          1         1             1.083333              1         0

``` r
load("alumnos.test.R")
#alumnos.test
head(alumnos.test,3)
```

    ##    genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 5       2        61.47273         37.94545              74.41818            8
    ## 15      2        63.70695         42.41390              81.12085            9
    ## 17      2        55.22528         25.45056              60.00000            6
    ##    evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 5                         4            16    1   0.8437500  12.44557  12.39884
    ## 15                        4            17    1   0.8841146  14.14316  14.05269
    ## 17                        4            14    1   0.8893229  11.90611  11.88356
    ##    prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 5       2.000          2         2            1.0833333              1
    ## 15      2.000          6         6            1.8333333              1
    ## 17      1.875          0         0            0.9166667              1
    ##    desercion
    ## 5          1
    ## 15         1
    ## 17         0

``` r
str(alumnos.training)
```

    ## 'data.frame':    700 obs. of  17 variables:
    ##  $ genero                  : num  2 2 2 1 2 2 2 1 2 1 ...
    ##  $ admision.letras         : num  60.1 59.1 53.1 57 61.9 ...
    ##  $ admision.numeros        : num  35.2 33.2 21.3 29 38.9 ...
    ##  $ promedio.preparatoria   : num  70.3 67.2 60 61 75.8 ...
    ##  $ edad.ingreso            : num  8 7 5 6 8 5 7 4 7 10 ...
    ##  $ evalucion.socioeconomica: num  4 4 4 4 4 4 4 4 4 4 ...
    ##  $ nota.conducta           : num  16 15 13 14 16 13 15 12 15 18 ...
    ##  $ beca                    : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Asist.Total             : num  0.875 0.857 0.842 0.915 0.876 ...
    ##  $ prom.trab               : num  12.5 13.1 12.5 14.2 13.5 ...
    ##  $ prom.exam               : num  12.5 13.1 12.5 14.2 13.4 ...
    ##  $ prom.pagos              : num  2 0 0 2 0 0 2 2 2 2 ...
    ##  $ uso.biblio              : num  2 3 1 7 5 11 6 1 7 1 ...
    ##  $ uso.platf               : num  2 3 1 7 5 11 6 1 7 1 ...
    ##  $ prom.apartado.libros    : num  1.25 1.33 1.08 1.92 1.92 ...
    ##  $ cambio.carrera          : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ desercion               : num  1 0 0 0 1 0 0 0 0 0 ...

## Formula 1

``` r
formula1 <- desercion ~ .
```

## Neural Net

``` r
set.seed(1234)
neural.net.1 <- neuralnet(formula = formula1, 
                          data  = alumnos.training,
                          hidden = c(16,5,5), #numero de capas iniciales, medias y finales 
                          threshold = 0.005, #el threshold del perceptron para arrojar una salida iniciamos con 0.01
                          stepmax = 1e+07, #la cantidad de veces q lo queremos correr
                          lifesign = "full") # para ver q hace R
```

    ## hidden: 16, 5, 5    thresh: 0.005    rep: 1/1    steps:    1000  min thresh: 2.2874452913864
    ##                                                            2000  min thresh: 0.70078147423752
    ##                                                            3000  min thresh: 0.288531220202367
    ##                                                            4000  min thresh: 0.144983401045894
    ##                                                            5000  min thresh: 0.0806792077585934
    ##                                                            6000  min thresh: 0.0806792077585934
    ##                                                            7000  min thresh: 0.051076982634546
    ##                                                            8000  min thresh: 0.051076982634546
    ##                                                            9000  min thresh: 0.0381220549349857
    ##                                                           10000  min thresh: 0.0336865964409138
    ##                                                           11000  min thresh: 0.0303958500690598
    ##                                                           12000  min thresh: 0.0283519251699042
    ##                                                           13000  min thresh: 0.0236574506486298
    ##                                                           14000  min thresh: 0.0236574506486298
    ##                                                           15000  min thresh: 0.0230561185735342
    ##                                                           16000  min thresh: 0.0209547728419911
    ##                                                           17000  min thresh: 0.0189598268623448
    ##                                                           18000  min thresh: 0.0189598268623448
    ##                                                           19000  min thresh: 0.0172315032837201
    ##                                                           20000  min thresh: 0.0172315032837201
    ##                                                           21000  min thresh: 0.0172315032837201
    ##                                                           22000  min thresh: 0.0153660200742216
    ##                                                           23000  min thresh: 0.0153564010282735
    ##                                                           24000  min thresh: 0.0150164760740895
    ##                                                           25000  min thresh: 0.0138377918041906
    ##                                                           26000  min thresh: 0.0138377918041906
    ##                                                           27000  min thresh: 0.0132671447343269
    ##                                                           28000  min thresh: 0.0113784871928501
    ##                                                           29000  min thresh: 0.0113784871928501
    ##                                                           30000  min thresh: 0.0113784871928501
    ##                                                           31000  min thresh: 0.0113784871928501
    ##                                                           32000  min thresh: 0.0107589767196449
    ##                                                           33000  min thresh: 0.0107589767196449
    ##                                                           34000  min thresh: 0.0102393007070116
    ##                                                           35000  min thresh: 0.00998479939878145
    ##                                                           36000  min thresh: 0.00974240390737268
    ##                                                           37000  min thresh: 0.00974240390737268
    ##                                                           38000  min thresh: 0.00974240390737268
    ##                                                           39000  min thresh: 0.00974240390737268
    ##                                                           40000  min thresh: 0.00974240390737268
    ##                                                           41000  min thresh: 0.00857469466769486
    ##                                                           42000  min thresh: 0.00857469466769486
    ##                                                           43000  min thresh: 0.00799417918026434
    ##                                                           44000  min thresh: 0.00799417918026434
    ##                                                           45000  min thresh: 0.00785451657527817
    ##                                                           46000  min thresh: 0.00785451657527817
    ##                                                           47000  min thresh: 0.00783611025837647
    ##                                                           48000  min thresh: 0.00783611025837647
    ##                                                           49000  min thresh: 0.00780621948448942
    ##                                                           50000  min thresh: 0.00725570393341569
    ##                                                           51000  min thresh: 0.00629047139819468
    ##                                                           52000  min thresh: 0.0061575600782334
    ##                                                           53000  min thresh: 0.0057030827418928
    ##                                                           54000  min thresh: 0.00566493292314878
    ##                                                           55000  min thresh: 0.00566493292314878
    ##                                                           56000  min thresh: 0.00549677052713114
    ##                                                           57000  min thresh: 0.00549677052713114
    ##                                                           58000  min thresh: 0.00514546550359983
    ##                                                           59000  min thresh: 0.00514546550359983
    ##                                                           60000  min thresh: 0.00514546550359983
    ##                                                           61000  min thresh: 0.00514546550359983
    ##                                                           62000  min thresh: 0.00514546550359983
    ##                                                           63000  min thresh: 0.00514546550359983
    ##                                                           64000  min thresh: 0.00514546550359983
    ##                                                           64978  error: 0.00125  time: 2.38 mins

## Grafica de la Red Neuronal

``` r
class(neural.net.1)
```

    ## [1] "nn"

``` r
plot(neural.net.1)
```

## Testing

probando con alumnos.test

``` r
nn.result <- compute(neural.net.1, alumnos.test)
attributes(nn.result)
```

    ## $names
    ## [1] "neurons"    "net.result"

``` r
#nn.result$net.result
head(nn.result$net.result,5)
```

    ##            [,1]
    ## 5  9.994300e-01
    ## 15 1.008656e+00
    ## 17 8.102918e-06
    ## 27 8.102918e-06
    ## 29 2.103015e-01

## Matríz de confusión

``` r
rounded.prediction <- round(nn.result$net.result)
error <- vector(mode="numeric", length=nrow(rounded.prediction))
compare.output <-  cbind(alumnos.test$desercion, rounded.prediction, error)
colnames(compare.output) <- c("entrada","prediccion","error")
compare.output <- as.data.frame(compare.output)

mzc.true.positives <- 0
mzc.true.negatives <- 0
mzc.false.positives <- 0
mzc.false.negatives <- 0

for(i in 1:200){

        if (compare.output[i,]$entrada == 1){
                
                if(compare.output[i,]$prediccion == 1){
                        mzc.true.positives <- mzc.true.positives + 1
                }
                else{
                        mzc.false.positives <- mzc.false.positives + 1
                }
        }
        else{
                if(compare.output[i,]$prediccion == 0){
                        mzc.true.negatives <- mzc.true.negatives + 1
                }
                else{
                         mzc.false.negatives <- mzc.false.negatives + 1
                }
                
        }
}

print(paste("True positives:",mzc.true.positives))
```

    ## [1] "True positives: 65"

``` r
print(paste("True negatives:",mzc.true.negatives))
```

    ## [1] "True negatives: 130"

``` r
print(paste("False positives:",mzc.false.positives))
```

    ## [1] "False positives: 2"

``` r
print(paste("False negatives:",mzc.false.negatives))
```

    ## [1] "False negatives: 3"

## Accuracy

``` r
#Accuracy = (TP + TN) / (TP + TN + FP + FN)
mzc.accuracy <- (mzc.true.positives + mzc.true.negatives) /
        (mzc.true.positives + mzc.true.negatives + mzc.false.positives + mzc.false.negatives)
print(paste("Accuracy= ", (mzc.accuracy * 100), "%", sep = ""))
```

    ## [1] "Accuracy= 97.5%"

## Identificacion de desertores

``` r
setwd(local.path)
load("alumnos.nuevos.R")
head(alumnos.nuevos, 5)
```

    ##     genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 14       2        64.93722         44.87445              84.81167            9
    ## 39       1        58.37728         31.75456              65.13184            7
    ## 81       1        62.96414         40.92828              78.89242            9
    ## 113      2        57.67933         30.35865              63.03798            7
    ## 117      2        65.19962         45.39924              85.59885           10
    ##     evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 14                         4            17    1   0.8828125  12.36401  12.39782
    ## 39                         4            15    1   0.9296875  13.08116  13.06243
    ## 81                         2            17    2   0.7851562  12.53885  12.51326
    ## 113                        4            15    1   0.9309896  13.58561  13.59659
    ## 117                        4            18    1   0.9479167  15.08562  15.07159
    ##     prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 14       2.000          2         2             1.166667              1
    ## 39       2.000          3         3             1.333333              1
    ## 81       0.000          1         1             1.083333              1
    ## 113      2.000          6         6             1.666667              1
    ## 117      1.875         10        10             2.166667              1

``` r
str(alumnos.nuevos)
```

    ## 'data.frame':    100 obs. of  16 variables:
    ##  $ genero                  : num  2 1 1 2 2 2 2 1 2 2 ...
    ##  $ admision.letras         : num  64.9 58.4 63 57.7 65.2 ...
    ##  $ admision.numeros        : num  44.9 31.8 40.9 30.4 45.4 ...
    ##  $ promedio.preparatoria   : num  84.8 65.1 78.9 63 85.6 ...
    ##  $ edad.ingreso            : num  9 7 9 7 10 7 5 10 5 10 ...
    ##  $ evalucion.socioeconomica: num  4 4 2 4 4 4 4 2 4 4 ...
    ##  $ nota.conducta           : num  17 15 17 15 18 15 13 18 13 18 ...
    ##  $ beca                    : num  1 1 2 1 1 1 1 2 1 1 ...
    ##  $ Asist.Total             : num  0.883 0.93 0.785 0.931 0.948 ...
    ##  $ prom.trab               : num  12.4 13.1 12.5 13.6 15.1 ...
    ##  $ prom.exam               : num  12.4 13.1 12.5 13.6 15.1 ...
    ##  $ prom.pagos              : num  2 2 0 2 1.88 ...
    ##  $ uso.biblio              : num  2 3 1 6 10 3 1 2 8 9 ...
    ##  $ uso.platf               : num  2 3 1 6 10 3 1 2 8 9 ...
    ##  $ prom.apartado.libros    : num  1.17 1.33 1.08 1.67 2.17 ...
    ##  $ cambio.carrera          : num  1 1 1 1 1 1 1 1 1 1 ...

## Evaluar con Red Neuronal

``` r
nuevos.result <- compute(neural.net.1, alumnos.nuevos)
attributes(nuevos.result)
```

    ## $names
    ## [1] "neurons"    "net.result"

``` r
nuevos.result$net.result <- round(nuevos.result$net.result)
#nuevos.result$net.result
head(nuevos.result$net.result,5)
```

    ##     [,1]
    ## 14     0
    ## 39     0
    ## 81     1
    ## 113    0
    ## 117    0

``` r
class(nuevos.result$net.result)
```

    ## [1] "matrix"

``` r
#str(nuevos.result)
alumnos.desertores.df <- as.data.frame(nuevos.result[[2]])
desercion <- vector(mode="numeric", length = nrow(alumnos.nuevos))

desercion <- alumnos.desertores.df$V1

alumnos.desertores.df <- cbind(alumnos.nuevos, desercion)
#alumnos.desertores.df
head(alumnos.desertores.df,5)
```

    ##     genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 14       2        64.93722         44.87445              84.81167            9
    ## 39       1        58.37728         31.75456              65.13184            7
    ## 81       1        62.96414         40.92828              78.89242            9
    ## 113      2        57.67933         30.35865              63.03798            7
    ## 117      2        65.19962         45.39924              85.59885           10
    ##     evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 14                         4            17    1   0.8828125  12.36401  12.39782
    ## 39                         4            15    1   0.9296875  13.08116  13.06243
    ## 81                         2            17    2   0.7851562  12.53885  12.51326
    ## 113                        4            15    1   0.9309896  13.58561  13.59659
    ## 117                        4            18    1   0.9479167  15.08562  15.07159
    ##     prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 14       2.000          2         2             1.166667              1
    ## 39       2.000          3         3             1.333333              1
    ## 81       0.000          1         1             1.083333              1
    ## 113      2.000          6         6             1.666667              1
    ## 117      1.875         10        10             2.166667              1
    ##     desercion
    ## 14          0
    ## 39          0
    ## 81          1
    ## 113         0
    ## 117         0

Obtenemos la correlacion con
pearson

``` r
cor.matrix.desertores <- cor(alumnos.desertores.df, method = "pearson", use = "complete.obs")
corrplot(cor.matrix.desertores)
```

![](figure/imgs/README_figs/README-unnamed-chunk-14-1.png)<!-- -->

``` r
a<- alumnos.desertores.df[alumnos.desertores.df$desercion == 1,]
print("Rows and columns")
```

    ## [1] "Rows and columns"

``` r
print(dim(a))
```

    ## [1] 38 17

``` r
alumnos.desertores.df <- a
```

## Seleccionar alumnos que se predicen van a reprobar

Guardamos los desertores para el siguiente paso

``` r
setwd(local.path)
save(alumnos.desertores.df, file="alumnos.desertores.df.R")
```

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
