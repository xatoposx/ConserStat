# AYUDA 

## Instalación de R
Se asume que el usuario tiene instaldo R con las librerías: `ggplot2`, `knitr`, `plyr` y `xtable`.

Para más información sobre la instalación de R y librerías añadidas consultar la página:

http://cran.r-project.org/doc/manuals/r-release/R-admin.html

## Cargar scripts
Para iniciar la exploración hay que cargar el script `interfaz.R` con el siguiente comando:

`source("interfaz.R")`

## Uso básico

## Exploración

### Ejemplos básicos de generación de gráficos

| Comando                                                                     |  Gráfico                                                            |
|:----------------------------------------------------------------------------|:-------------------------------------------------------------------:|
|                                                                             |                                                                     |
| `MostrarAprobadosPorCursoEnAsignatura("Guitarra")`                          | ![](./graficos/samples/sample_aprobados_asignatura.png)             |
| `MostrarAprobadosPorCursoEnDepartamento("Cuerda_Pulsada")`                  | ![](./graficos/samples/sample_aprobados_dpto_desglosado.png)        |
| `MostrarAprobadosPorCursoEnDepartamento("Cuerda_Pulsada", desglosar=FALSE)` | ![](./graficos/samples/sample_aprobados_dpto.png)                   |
| `CompararNotasPorCursoEnAsignatura("Guitarra")`                             | ![](./graficos/samples/sample_comparar_notas_asignatura.png)        |
| `CompararNotasPorCursoEnDepartamento("Cuerda_Pulsada")`                     | ![](./graficos/samples/sample_comparar_notas_dpto_desglosado.png)   |
| `CompararNotasPorCursoEnDepartamento("Cuerda_Pulsada", desglosar=FALSE)`    | ![](./graficos/samples/sample_comparar_notas_dpto.png)              |
| `ContarNotasEnAsignatura("Guitarra")`                                       | ![](./graficos/samples/sample_contar_notas_asignatura.png)          |
| `ContarNotasEnDepartamento("Cuerda_Pulsada")`                               | ![](./graficos/samples/sample_contar_notas_dpto_desglosado.png)     |
| `ContarNotasEnDepartamento("Cuerda_Pulsada", desglosar=FALSE)`              | ![](./graficos/samples/sample_contar_notas_dpto.png)                |
| `CompararDistribucionesDeNotasPorCurso()`                                   | ![](./graficos/samples/sample_comparar_distros_notas.png)           |
| `CompararAprobadosPorCurso()`                                               | ![](./graficos/samples/sample_comparar_aprobados.png)               |

## Impresión de informe

`GenerarInforme()`
