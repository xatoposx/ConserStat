# LEEME

## Contenido del archivo

* `graficos`: Directorio de imágenes.
* `tablas`: Directorio de tablas (Ausente en la rama pública).
* `AYUDA.md`: Ayuda para el usuario.
* `Asignaturas.txt`: Asocia el código de asignatura (tal como aparece en los datos brutos) con su nombre legible.
* `Calificaciones 2013_2014.txt`: Datos brutos suministrados por Codex. (Ausente en la rama pública).
* `Departamentos.txt`: Asocia el código de asignatura con el nombre del departamento a que pertenece. 
* `LEEME.md`: The actual README (in Spanish).
* `README.txt`: Este documento.
* `explorar.R`: Funciones para análisis exploratorio de datos.
* `imprimir.R`: Funciones para impresión de informe en pdf.
* `informe.tex`: Fichero LaTeX generado para el informe. (Ausente en la rama publica).
* `informe.pdf`: Informe final. (Ausente en la rama pública).
* `interfaz.R`: Wrappers de interfaz de usuario.
* `limpiar.R`: Script para limpiar datos brutos.
* `notas_2013_2014.csv`: Datos limpios. Ver *Descripción de Variables* más abajo. (Ausente en la rama pública).

### Variables en datos limpios (*Codebook*)
 
* **Departamento**: Nombre de departamento. Variable categórica con los siguientes niveles: 
    * "Agrupaciones"
    * "Cuerda_Arco"
    * "Cuerda_Pulsada"
    * "Optativa"
    * "Piano"
    * "Teoría"
    * "Viento_Madera"
    * "Viento_Metal_Percusión"

* **Asignatura**: Nombre de asignatura. Variable categórica con los siguientes niveles: 
    * "Acompañamiento"
    * "Alemán"
    * "Armonía"
    * "Análisis"
    * "Banda"
    * "Canto"
    * "Clarinete"
    * "Conjunto_Guitarra"
    * "Conjunto_Piano"
    * "Contrabajo"
    * "Coro"
    * "Coro1_Optativa"
    * "Coro2_Optativa"
    * "Edición_de_Partituras"
    * "Educación_Auditiva1"
    * "Educación_Auditiva2"
    * "Fagot"
    * "Flauta"
    * "Francés"
    * "Fundamentos_de_Composición"
    * "Guitarra"
    * "Historia_de_la_Música"
    * "Iniciación_a_la_Percusión"
    * "Iniciación_al_Canto"
    * "Italiano"
    * "Jazz1"
    * "Lenguaje_Musical"
    * "Música_de_Cámara"
    * "Oboe", "Orquesta"
    * "PA1E"
    * "Percusión"
    * "Piano"
    * "Piano_Complementario"
    * "Saxofón"
    * "Trombón"
    * "Trompa"
    * "Trompeta"
    * "Tuba"
    * "Viola"
    * "Violín"
    * "Violoncello"

* **Curso**: Nombre del Curso. Variable categórica con los siguientes niveles: 
    * "1E"
    * "2E"
    * "3E"
    * "4E"
    * "1P"
    * "2P"
    * "3P"
    * "4P"
    * "5P"
    * "6P"

* **Nota**: Nota final de Evaluación ordinaria. Variable numérica entera, de 1 a 10.

* **Aprobado**: Variable booleana que codifica si la nota es aprobado o no, con niveles: "SÍ", "NO"

## Cómo reproducir el proceso 
Ejecutar desde un interprete de R la orden `source("interfaz.R")`
                
