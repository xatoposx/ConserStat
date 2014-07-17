## Análisis de Calificaciones

### Contenido del archivo

* `README.txt`: Este documento
* `TODO.txt`: Tareas pendientes
* `limpiar.R`: Script para limpiar datos brutos.
* `explorar.R`: Funciones para análisis exploratorio de datos.
* `reproducir.R`: Script para ejecutar los procesos de carga, limpieza y generación automática de tablas y gráficos.
* `Calificaciones 2013_2014.txt`: Datos brutos suministrados por Codex.
* `Asignaturas.txt`: Fichero editado para el análisis que asocia el código de asignatura (tal como aparece en los datos brutos) con su nombre legible.
* `Departamentos.txt`: Fichero editado para el análisis que asocia el referido código de asignatura con el nombre del departamento a que pertenece. 
* `notas_2013_2014.csv`: Datos limpios (ver *Descripción de Variables* más abajo).
* `informe.tex`: Plantilla LaTeX para generación de informe.pdf.
* `informe.pdf`: Informe final.
* `tablas/`: Directorio con ficheros `.tex` de tablas de Aprobados/Suspensos por asignatura.
* `graficos/`: Directorio con ficheros `.png` de gráficos de Aprobados/Suspensos por departamento.

### Variables en `notas_2013_2014.csv`
* **Departamento**: Nombre de departamento. Variable categórica con los siguientes niveles: "Agrupaciones", "Cuerda_Arco", "Cuerda_Pulsada", "Optativa", "Piano", "Teoría", "Viento_Madera", "Viento_Metal_Percusión".
* **Asignatura**: Nombre de asignatura. Variable categórica con los siguientes niveles: "Acompañamiento", "Alemán", "Análisis", "Armonía", "Asignatura", "Banda", "Canto", "Clarinete", "Conjunto_Guitarra", "Conjunto_Piano", "Contrabajo", "Coro", "Coro1_Optativa", "Coro2_Optativa", "Edición_de_Partituras", "Educación_Auditiva1", "Educación_Auditiva2", "Fagot", "Flauta", "Francés", "Fundamentos_de_Composición", "Guitarra", "Historia_de_la_Música", "Iniciación_a_la_Percusión", "Iniciación_al_Canto", "Italiano", "Jazz1", "Lenguaje_Musical", "Música_de_Cámara", "Oboe", "Orquesta", "PA1E", "Percusión", "Piano", "Piano_Complementario", "Saxofón", "Trombón", "Trompa", "Trompeta", "Tuba", "Viola", "Violín", "Violoncello".
* **Curso**: Nombre del Curso. Variable categórica con los siguientes niveles: "1E", "1P", "2E", "2P", "3E", "3P", "4E", "4P", "5P", "6P".
* **Nota**: Nota final de Evaluación ordinaria. Variable numérica entera, de 1 a 10.
* **Aprobado**: Variable booleana que codifica si la nota es aprobado o no, con niveles: "SÍ", "NO"

### Cómo reproducir el proceso 
Ejecutar desde un interprete de R la orden source("reproducir.R")
                
