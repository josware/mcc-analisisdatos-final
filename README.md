# Proyecto Final 
### El objetivo del proyecto es identificar alumnos en riesgo de deserción. 
### Metodología
 
- Integración de datos Los datos están limpios. El mismo identificador de alumno es utilizado en los diferentes archivos, es necesario integrar adecuadamente los datos en un solo dataframe. Separar 100 alumnos para correr la red, estos alumnos no entran al kmeans ni entrenan la red.  
- Clustering (usan 900 alumnos) Es necesario identificar a los alumnos que están en riesgo a través de encontrar el cluster problemático. plot el codo. Digamos que encuentran 5 clusters, y hay un cluster con los más bajos indicadores (cluster.centers) con 200 alumnos. Crean una nueva columna deserción y ponen el valor de 1 para estos 200 y para los otros 700 con 0. 
- Red neuronal Se debe crear un dataset de entrenamiento creando una nueva columna en el dataset llamada "deserción" que sea 1 si el alumno pertenece al cluster problemático. Este será el set de entrenamiento para la red neuronal. Se debe particionar la información. Entrenan con 700 y prueban con 200, diferentes topologías. 
- Corran la red para los 100 alumnos que separaron. Identifiquen a los desertores. Digamos que fueron 20.  
- Algoritmo genético Se deben establecer medidas de remediación para la deserción tales como becas, vales de transporte, asesorías, mentorías y establecer un costo y un beneficio. El resultado el algoritmo genético son las series de medidas a aplicar para mitigar la deserción. 


## Ideas de listas:
### Mecanismos
### presupuesto: 10k dólares
1. Dar beca estudiantil, 500 dólares. 
2. Vales de transporte, 100 dólares.
3. Mentoría, 200 dólares. 
4. Consultoría psicológica, 400 dólares. 
5. Boleto a evento integración, 50 dólares.
6. Asesor??a individual, 250 dólares.
7. Cursos remediales, 2500 dólares
8. Visita a empresa, 50 dólares
9. Ex??men extemporáneo, 100 dólares
10. plótica motivacional, 50 dólares 
11. viaje recreativo, 10 dólares

para este caso, tenemos 20 alumnos y 11 medidas. Esto representa un cromosoma binario de tamaño 20 x 11 = 220

10000000000 0...
00000000000 100...
11111111111 0...
