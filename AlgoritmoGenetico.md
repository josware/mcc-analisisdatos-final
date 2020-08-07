Proyecto Final - Algoritmo Genetico
================
Casillas, A., González, L., Gómez, J.
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

``` r
#install.packages("genalg")
library("genalg")
```

Cargamos los datos

``` r
setwd(local.path)
load("alumnos.desertores.df.R")
```

``` r
#alumnos.desertores.df
head(alumnos.desertores.df,5)
```

    ##     genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 81       1        62.96414         40.92828              78.89242            9
    ## 123      2        59.84130         34.68260              69.52390            7
    ## 185      2        63.09651         41.19303              79.28954            9
    ## 245      2        62.54559         40.09119              77.63678            9
    ## 302      2        62.95205         40.90409              78.85614            9
    ##     evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 81                         2            17    2   0.7851562  12.53885  12.51326
    ## 123                        4            15    1   0.8750000  12.39447  12.32973
    ## 185                        4            17    1   0.7578125  12.25492  12.22629
    ## 245                        4            17    1   0.9479167  15.16722  15.11197
    ## 302                        4            17    1   0.9088542  12.89660  12.79655
    ##     prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 81       0.000          1         1             1.083333              1
    ## 123      0.000          3         3             1.166667              1
    ## 185      2.000          0         0             1.000000              1
    ## 245      0.000         10        10             2.416667              1
    ## 302      1.875          5         5             1.583333              1
    ##     desercion
    ## 81          1
    ## 123         1
    ## 185         1
    ## 245         1
    ## 302         1

``` r
nrow(alumnos.desertores.df)
```

    ## [1] 38

## Funciones de soporte

``` r
getBest <- function(inTree) {
  outBest <- inTree$population[inTree$evaluations == min(inTree$best),]
  return (outBest)
}  

printStats <- function(inBest) {
  out.sep <- "--------------------------------------------------------------------------------"
  out.items.usd.ammount <- inBest %*% items$usd.ammount
  out.stayingpoints  <- inBest %*% items$stayingpoints 
  print(out.sep)
  print("Vector: ")
  print(inBest)
  print(paste("Items USD sum: ", out.items.usd.ammount))
  print(paste("Puntos de estadía de este vector: ", out.stayingpoints))
  print(out.sep)
  } 
```

## Función optimizada

Definimos la funcion y sus reglas.

``` r
#Constantes / constraints
desercion.size <- nrow(alumnos.desertores.df)
budget.limit.10k <- 10000

#Vectores
v.item <- c("beca","bienevales","mentoria","psicologo","evento",
         "asesor","remediales", "visita.empresa","extratemporaneo", "cheertalk", 
         "viaje")
v.s.p <- c(90,95,85,70,50,60,80,100,80,85,70)
v.usd = c(500, 100, 200, 400, 50,250, 2500, 50, 100, 50,10)

#Inicializar 
items <- data.frame(item= v.item,
                    stayingpoints = v.s.p,
                    usd.ammount = v.usd)
#Igualar dimensión
for(i in 1:(desercion.size-1)){
  items <- rbind(items,data.frame(item= v.item,
                    stayingpoints = v.s.p,
                    usd.ammount = v.usd))  
  
  #Si fuera por columna:
  #  items <- cbind(items, v.item, v.s.p , v.usd)
}

fitness.presupuesto.desercion <- function(x) {

    items.usd.ammount <- x %*% items$usd.ammount
    items.s.p <- x %*% items$stayingpoints

  if (items.usd.ammount > budget.limit.10k ) 
  {
    return(0)
  }
  else
  {
    partial.result <- items.s.p
    # #"beca","bienevales","mentoria","psicologo","evento","asesor","remediales", "visita.empresa","extratemporaneo", "cheertalk", "viaje"
    # #   1        2           3           4          5        6          7              8                  9               10        11
    # #"beca","bienevales","mentoria","psicologo","evento","asesor","remediales", "visita.empresa","extratemporaneo", "cheertalk", "viaje"
    # #   12        13           14           15          16        17          18       19                 20               21       22
    
    alumnos.desertores.df[i,]
    for(i in 1:desercion.size){
      start <- 11*i-10
      end <- 11*i
      
      #Regla 1 Si ya tiene Beca
      if(alumnos.desertores.df[i,]$beca + x[start] > 2){
        partial.result <- items.s.p - 90
      }
      
      
      #Regla 2 Si Bienevales Bit 2 (start+1 = 2)
      if(alumnos.desertores.df[i,]$evalucion.socioeconomica + x[start+1] < 4){
        partial.result <- items.s.p - 20
      }else{
        if(alumnos.desertores.df[i,]$Asist.Total < 0.6){
          partial.result <- items.s.p + 20
        }else{
           partial.result <- items.s.p + 10
        }
      }
      
      #Regla 3 mentoria Bit 2 (start+2 = bit 3)
      if(alumnos.desertores.df[i,]$prom.exam + x[start+2] > 10){
        papartial.result <- items.s.p - 8
      }
      #Regla 4 Consultoría Psicológica Bit 4 (start+3 = bit 4)
      if(alumnos.desertores.df[i,]$evalucion.socioeconomica + x[start+3] < 4){
        papartial.result <- items.s.p - 30
      }
      #Regla 5 Boleto Evento integración Bit 5 (start+4 = bit 5)
      if(alumnos.desertores.df[i,]$nota.conducta + x[start+4] < 17){
        papartial.result <- items.s.p + 10
      }
      
      #Regla 6 Asesoria individual 
      if(mean(alumnos.desertores.df[i,]$prom.exam + alumnos.desertores.df[i,]$prom.trab) < 10){
        if(x[start+3] == 0){
          partial.result <- items.s.p + 60
        }else{
          partial.result <- items.s.p - 60
        }
      }
      
      #Reglas 7 Cursos remediales
      if(alumnos.desertores.df[i,]$promedio.preparatoria < 80){
         if(x[start+4] == 0){
          partial.result <- items.s.p + 80
        }else{
          partial.result <- items.s.p - 80
        }
      }
      
      #Regla 8 Visita a emepresa
      if(alumnos.desertores.df[i,]$cambio.carrera + x[start+5] > 2){
         partial.result <- items.s.p + 100
      }
      
      if(alumnos.desertores.df[i,]$cambio.carrera + x[start+5] <= 2){
         partial.result <- items.s.p - 100
      }
      
      #Regla 9 examen extra temp bit 9
      if(alumnos.desertores.df[i,]$prom.exam + x[start+8] > 12){
        partial.result <- items.s.p - v.s.p[9]
      }
      #Regla 10 platica motivacional 10 (start+1 = 2)
      if(alumnos.desertores.df[i,]$Asist.Total + x[start+9] > 90){
        partial.result <- items.s.p - v.s.p[10]
      }
      #Regla 11 mentoria altas calificaciones Bit 11 (start+10 = bit 11)
      if(alumnos.desertores.df[i,]$prom.exam + x[start+10] > 15){
        partial.result <- items.s.p - v.s.p[11]
      }
      
      
      
    }#Fin de reglas
    return (-items.s.p)
  }
}
#items
head(items,5)
```

    ##         item stayingpoints usd.ammount
    ## 1       beca            90         500
    ## 2 bienevales            95         100
    ## 3   mentoria            85         200
    ## 4  psicologo            70         400
    ## 5     evento            50          50

``` r
ga.features <- length(v.item)
ga.features
```

    ## [1] 11

Usamos nuestra funcion fit para obtener los mejores cromosomas para el
presupuesto. Obtenermos el Mejor.

``` r
set.seed(2020)
#alumnos.desertores.df
head(alumnos.desertores.df,5)
```

    ##     genero admision.letras admision.numeros promedio.preparatoria edad.ingreso
    ## 81       1        62.96414         40.92828              78.89242            9
    ## 123      2        59.84130         34.68260              69.52390            7
    ## 185      2        63.09651         41.19303              79.28954            9
    ## 245      2        62.54559         40.09119              77.63678            9
    ## 302      2        62.95205         40.90409              78.85614            9
    ##     evalucion.socioeconomica nota.conducta beca Asist.Total prom.trab prom.exam
    ## 81                         2            17    2   0.7851562  12.53885  12.51326
    ## 123                        4            15    1   0.8750000  12.39447  12.32973
    ## 185                        4            17    1   0.7578125  12.25492  12.22629
    ## 245                        4            17    1   0.9479167  15.16722  15.11197
    ## 302                        4            17    1   0.9088542  12.89660  12.79655
    ##     prom.pagos uso.biblio uso.platf prom.apartado.libros cambio.carrera
    ## 81       0.000          1         1             1.083333              1
    ## 123      0.000          3         3             1.166667              1
    ## 185      2.000          0         0             1.000000              1
    ## 245      0.000         10        10             2.416667              1
    ## 302      1.875          5         5             1.583333              1
    ##     desercion
    ## 81          1
    ## 123         1
    ## 185         1
    ## 245         1
    ## 302         1

``` r
#ga.tree$population[ga.tree$evaluations == min(ga.tree$best),]
ga.tree <- rbga.bin(size = (ga.features * desercion.size), popSize = 100,
                    mutationChance = .01,
                    elitism = 1, iters = 15,
                    evalFunc = fitness.presupuesto.desercion,
                    verbose = T
                    )
```

    ## Testing the sanity of parameters...
    ## Not showing GA settings...
    ## Starting with random values in the given domains...
    ## Starting iteration 1 
    ## Calucating evaluation values... .................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 416 mutations applied
    ## Starting iteration 2 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 407 mutations applied
    ## Starting iteration 3 
    ## Calucating evaluation values... .................................................................................................. done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 420 mutations applied
    ## Starting iteration 4 
    ## Calucating evaluation values... .................................................................................................. done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 388 mutations applied
    ## Starting iteration 5 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 415 mutations applied
    ## Starting iteration 6 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 453 mutations applied
    ## Starting iteration 7 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 411 mutations applied
    ## Starting iteration 8 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 428 mutations applied
    ## Starting iteration 9 
    ## Calucating evaluation values... ................................................................................................. done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 392 mutations applied
    ## Starting iteration 10 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 422 mutations applied
    ## Starting iteration 11 
    ## Calucating evaluation values... .................................................................................................. done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 412 mutations applied
    ## Starting iteration 12 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 423 mutations applied
    ## Starting iteration 13 
    ## Calucating evaluation values... ................................................................................................... done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 418 mutations applied
    ## Starting iteration 14 
    ## Calucating evaluation values... .................................................................................................. done.
    ## Creating next generation...
    ##   sorting results...
    ##   applying elitism...
    ##   applying crossover...
    ##   applying mutations... 408 mutations applied
    ## Starting iteration 15 
    ## Calucating evaluation values... .................................................................................................. done.

``` r
#Calculamos el mejor cromosoma
best.anti.desertores <- getBest(ga.tree)
#best.anti.desertores
head(best.anti.desertores,5)
```

    ## [1] 0 1 0 1 0

## Mejor cromosoma

imprmimos el mejor cromosoma para repartir el
    presupuesto

``` r
printStats(best.anti.desertores)
```

    ## [1] "--------------------------------------------------------------------------------"
    ## [1] "Vector: "
    ##   [1] 0 1 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0
    ##  [38] 0 0 0 1 1 0 1 0 0 0 1 0 0 0 1 0 0 1 0 1 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0
    ##  [75] 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1
    ## [112] 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0
    ## [149] 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1
    ## [186] 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0
    ## [223] 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
    ## [260] 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [297] 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0
    ## [334] 1 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 0
    ## [371] 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 1 0 0 0 0 0 0 1 0 1
    ## [408] 0 0 0 0 0 0 0 0 0 0 0
    ## [1] "Items USD sum:  9510"
    ## [1] "Puntos de estadía de este vector:  5205"
    ## [1] "--------------------------------------------------------------------------------"

``` r
for(i in 1:desercion.size){
  start <- 11*i-10
  end <- 11*i
  #print(i)
  print(best.anti.desertores[start:end])
}
```

    ##  [1] 0 1 0 1 0 0 0 0 0 1 0
    ##  [1] 0 1 0 0 0 0 0 0 0 0 0
    ##  [1] 1 0 0 0 1 0 0 0 0 1 0
    ##  [1] 0 0 0 0 0 0 0 1 1 0 1
    ##  [1] 0 0 0 1 0 0 0 1 0 0 1
    ##  [1] 0 1 0 0 0 0 0 0 0 1 1
    ##  [1] 0 0 0 1 0 0 0 0 1 0 0
    ##  [1] 0 0 1 0 0 0 0 0 1 0 0
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0
    ##  [1] 0 0 0 0 0 1 0 0 0 1 0
    ##  [1] 1 0 0 0 1 0 0 0 0 1 0
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0
    ##  [1] 0 0 0 1 0 0 0 0 0 1 0
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0
    ##  [1] 0 1 0 0 0 0 0 1 0 0 0
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0
    ##  [1] 0 0 0 1 1 0 0 0 1 0 0
    ##  [1] 0 1 0 0 1 0 0 0 0 0 0
    ##  [1] 0 0 0 0 0 0 0 1 0 0 0
    ##  [1] 0 0 0 0 0 0 0 1 1 1 0
    ##  [1] 0 0 0 0 1 0 0 0 1 0 0
    ##  [1] 0 0 0 0 0 0 0 0 1 0 0
    ##  [1] 1 0 0 0 0 1 0 0 0 0 0
    ##  [1] 0 0 1 0 0 0 0 0 0 0 0
    ##  [1] 0 0 0 1 0 0 0 0 0 0 0
    ##  [1] 0 0 0 1 1 0 0 0 0 0 0
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0
    ##  [1] 0 0 0 0 0 0 0 1 0 0 0
    ##  [1] 0 0 0 0 0 0 0 1 0 0 0
    ##  [1] 0 0 0 0 0 0 0 0 1 1 0
    ##  [1] 0 0 0 1 0 0 0 0 1 0 0
    ##  [1] 0 1 0 0 0 0 0 0 1 0 0
    ##  [1] 0 1 0 0 0 0 0 0 0 1 1
    ##  [1] 0 0 0 0 0 1 0 0 1 0 0
    ##  [1] 0 0 0 0 0 1 0 0 0 0 0
    ##  [1] 0 0 0 0 0 1 0 0 0 0 1
    ##  [1] 0 1 0 0 0 0 0 0 1 0 1
    ##  [1] 0 0 0 0 0 0 0 0 0 0 0

``` r
alumnos.desertores.df$beca
```

    ##  [1] 2 1 1 1 1 2 1 1 1 1 2 1 1 1 1 1 2 1 2 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 1

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
