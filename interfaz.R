## FUNCIONES DE INTERFAZ DE USUARIO
source("explorar.R")
source("imprimir.R")

## Funciones de exploración
MostrarAprobadosPorCursoEnAsignatura <- function(asignatura) {
	# Muestra diagrama de barras y tabla en consola de aprobados/suspensos
	# por curso en la asignatura dada
	print(Tabular("Curso", "Aprobado", "Asignatura", asignatura))
	Graficar("Curso", "Aprobado", "Asignatura", asignatura)
}

MostrarAprobadosPorCursoEnDepartamento <- function(departamento, desglosar=TRUE) {
	# Muestra diagrama de barras y tabla en consola de aprobados/suspensos
	# por curso en el departamento dado. Si desglosar=TRUE, muestra datos
	# desglosados por asignatura dentro del departamento.
	if (desglosar) {
		print(Tabular("Curso", "Aprobado", "Asignatura", 
			      AsignaturasEn(departamento)))
		Graficar("Curso", "Aprobado", "Asignatura", 
			      AsignaturasEn(departamento))
	} else {
		print(Tabular("Curso", "Aprobado", "Departamento", departamento))
		Graficar("Curso", "Aprobado", "Departamento", departamento)
	}
}

CompararNotasPorCursoEnAsignatura <- function(asignatura) {
	# Muestra diagramas de caja de notas por curso en la asignatura dada
	Graficar("Curso", "Nota", "Asignatura", asignatura)
}

CompararNotasPorCursoEnDepartamento <- function(departamento, desglosar=TRUE) {
	# Muestra diagramas de caja de notas por curso en el departamento dado.
	# Si desglosar=TRUE muestra datos desglosados por asignatura en el
        # departamento
	if (desglosar)
		Graficar("Curso", "Nota", "Asignatura", AsignaturasEn(departamento))
	else
		Graficar("Curso", "Nota", "Departamento", departamento)
}

ContarNotasEnAsignatura <- function(asignatura) {
	# Muestra un histograma de cuentas de notas en la asignatura dada
	Graficar("Nota", y=NULL, "Asignatura", asignatura)
}

ContarNotasEnDepartamento <- function(departamento, desglosar=TRUE) {
	# Muestra un histograma de cuentas de notas en el departamento dado.
	# Si desglosar=TRUE muestra datos desglosados por asignatura en el
	# departamento
	if (desglosar)
		Graficar("Nota", y=NULL, "Asignatura", AsignaturasEn(departamento))
	else
		Graficar("Nota", y=NULL, "Departamento", departamento)
}

CompararDistribucionDeNotasPorCurso <- function() {
	# Muestra gráficos de densidad de notas por curso
	Graficar("Nota", y=NULL, "Curso", asignaturas)
}

CompararAprobadosPorCurso <- function() {
	# Muestra gráfico mosica de aprobados/suspensos por curso.
	mosaicplot(xtabs(~ Curso + Aprobado, data=datos), 
		   col=c("Pink", "LightBlue"), main="")
}

## Funciones de generación de informe
GenerarInforme <- function(nombreInforme="informe.tex", desglosar=TRUE) {
	# Genera un informe en pdf

        # Tablas
	GenerarTablasParaMemoria(desglosar=desglosar)
	lapply(dir("./tablas", full.names=TRUE), TablasNoFlotantes)

        # Graficos
	GenerarGraficosParaMemoria(desglosar=desglosar)

        # Fichero LaTeX
	GenerarFicheroTex(nombreInforme)

        # Fichero pdf
	# knit ignora 'compiler' por el momento
	#knit2pdf(nombreInforme, compiler="pdflatex")
        system(command="pdflatex informe.tex")

}

