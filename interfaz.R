## FUNCIONES DE INTERFAZ DE USUARIO (FACHADA)
# --- Funciones proporcionadas
# * [Accion]Asignatura: La acción se aplica sobre una(s) asignatura(s)
# * [Accion]Departamento: La acción se aplica sobre un(os) departamento(s)
# * Generar[*]ParaMemoria: Genera ficheros necesarios para el informe latex
#
# --- Parámetros de las siguientes funciones de usuario
# * tipo: tipo de gráfico (sólo en funciones gráficas). Uno de los siguientes:
# - "apilado": barras apiladas
# - "agrupado": barras agrupadas
# - "poligono": polígono de frecuencias
# - ...
#
# * estadistica: clase de estadistica. Una de las siguientes:
# - "aprobados": número de aprobados/suspensos por curso
# - "nota-media": nota media por curso

# !!! TODO: Más funciones
# !!! TODO: Completar implementación de casos
# - ...
source("explorar.R")

MostrarGraficoAsignatura <-
function(asignatura, estadistica="aprobados", tipo="apilado") {
	# Muestra gráfico de la estadística por curso en la(s) asignatura(s)
	g <- DibujarBarrasApiladas(GraficarAprobadosPorAsignatura(asignatura))
	print(g)
}

MostrarGraficoAsignaturasDelDepartamento <-
function(departamento, estadistica="aprobados", tipo="apilado") {
	# Muestra gráfico de la estadística por curso en las asignaturas
	# del departamento
	g <- DibujarBarrasApiladas(
               GraficarAprobadosPorAsignatura(AsignaturasEn(departamento)))
	print(g)
}

MostrarGraficoDepartamento <-
function(departamento, estadistica="aprobados", tipo="apilado") {
	# Muestra gráfico de la estadística por curso en lo(s) departamento(s)
	g <- DibujarBarrasApiladas(GraficarAprobadosPorDepartamento(departamento))
	print(g)
}

MostrarTablaAsignatura <-
function(asignatura, estadistica="aprobados") {
	# Muestra tabla de la estadística por curso en la(s) asignatura(s)
	t <- TabularAprobadosPorAsignatura(asignatura)
	print(t)
}

MostrarTablaAsignaturasDelDepartamento <-
function(departamento, estadistica="aprobados") {
	# Muestra tabla de la estadística por curso en las asignaturas
	# del departamento
	t <- TabularAprobadosPorAsignatura(AsignaturasEn(departamento))
	print(t)
}

MostrarTablaDepartamento <-
function(departamento, estadistica="aprobados") {
	# Muestra tabla de la estadística por curso en lo(s) departamento(s)
	t <- TabularAprobadosPorDepartamento(departamento)
	print(t)
}

GenerarImagenesParaMemoria <- 
function(estadistica="aprobados", tipo="apilado") {
	# Genera imágenes PNG para la memoria y los guarda en disco

	# Aprobados desglosados por asignatura
	lapply(departamentos,
	       function(d) GuardarGrafico(
			     DibujarBarrasApiladas( 
			       GraficarAprobadosPorAsignatura(
			         AsignaturasEn(d)))))

	# Aprobados agrupados por asignatura

}

# !!! TODO - Fix
GenerarTablasParaMemoria <- 
function(estadistica="aprobados", variable="departamento") {
	# Genera tablas LATEX para la memoria y las guarda en disco
	# según estadística y variable de interés

	# nombre apropiado de directorio y funcion que pasar, 
	# diferente según parámetros
	if (variable == "asignatura") {
		dir <- "graficos_desglosados"
		fun <- function(x) 
		GuardarTabla(LatexizarTabla(TabularAprobadosPorAsignatura(AsignaturasEn(x))),
			     directorio=dir)
	} else if (variable == "departamento") {
		dir <- "graficos_sin_desglosar"
		fun <- function(x)
		GuardarTabla(TabularAprobadosPorDepartamento(LatexizarTabla(x)),
			     directorio=dir)
	}
 
	lapply(departamentos, fun)
}

