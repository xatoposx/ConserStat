library(ggplot2)
library(xtable)

source("limpiar.R")

## CONSTANTES
# @data.frame: califaciones. Léase README.md para descripción de sus variables 
datos <- notas2013

# @character: nombres de departamento
departamentos <- levels(departamentosDF$Departamento)

# @character: nombres de asignaturas
asignaturas <- levels(asignaturasDF$Asignatura)

# Número de columnas en graficos ggplot con facetas 
ncolFacets <- 2

## FUNCIONES DE CREACIÓN DE GRÁFICOS
GraficarAprobadosPorAsignatura <- function(asignatura) {
        # Produce gráfico de aprobados/suspensos por curso en la asignatura dada
	# @ character, character -> ggplot
	g <- ggplot(Seleccionar(asignatura, dpto=FALSE), 
		    aes(x=Curso, fill=Aprobado)) +
	facet_wrap(~ Asignatura, ncol=ncolFacets, scales="fixed") +
	labs(y="Número de Alumnos")
}

GraficarAprobadosPorDepartamento <- function(departamento) {
        # Produce gráfico de aprobados/suspensos por curso en el departamento dado
	# @ character, character -> ggplot
	g <- ggplot(Seleccionar(departamento, dpto=TRUE), 
		    aes(x=Curso, fill=Aprobado)) +
	facet_wrap(~ Departamento, ncol=ncolFacets, scales="fixed") +
	labs(y="Número de Alumnos")
}

DibujarBarrasApiladas <- function(grafico) {
	# Dibuja un gráfico de barras apiladas a partir de un ggplot sin capas
	# @ ggplot -> ggplot
	grafico + geom_bar(position="stack")
}

DibujarBarrasAgrupadas <- function(grafico) {
	# Dibuja un gráfico de barras agrupadas a partir de un ggplot sin capas
	# @ ggplot -> ggplot
	grafico + geom_bar(position="dodge")
}

DibujarPoligono <- function(grafico, grupo) {
	# Dibuja un polinomio de frecuencias para el grupo dado a partir 
	# de un ggplot sin capas
	# @ ggplot, character -> ggplot
	grafico + geom_freqpoly(aes_string(group=grupo, colour=grupo))
}

GuardarGrafico <- function(grafico, directorio=".") {
	# Guarda el gráfico como fichero png
	# @ ggplot -> IO
	atPng <- AtributosPng(grafico)
	nombreGrafico <- atPng$nombreGrafico
	ancho <- atPng$ancho
	alto <- atPng$alto

	nombreFichero <- file.path(directorio, 
				   paste(nombreGrafico, "png", sep="."))
	png(nombreFichero, ancho, alto)
	print(grafico)
	dev.off()
}

AtributosPng <- function(grafico) {
	# Computa atributos adecuadas a la imagen pgn a partir del grafico dado
	# [@IMPL: Se usa introspección. La implementación es más compleja de
	# de este modo, pero permite desacoplar las funciones]
	# ggplot -> list

	# Dimesiones !!! TODO - Eliminar magic numbers
	ancho <- 460 ; alto <- 460 # ancho, alto por defecto (pixels)

	asignaturasEnGrafico <- unique(grafico$data$Asignatura)
	numAsignaturas <- length(asignaturasEnGrafico)
	numColumnas <- grafico$facet$ncol
	numFilas <- ceiling(numAsignaturas / numColumnas)
	escala <- 0.5 # asume 2 cols
	altoCelda <- 138 + 138 * escala
	rellenoSuperior <- 16 + 16 * escala
	rellenoInferior <- 40 + 40 * escala
	ancho <- ancho + ancho * escala
	alto <- rellenoSuperior + rellenoInferior + altoCelda * numFilas 

	# Nombre imagen
        nombreDepartamento <- DeterminarDepartamento(asignaturasEnGrafico)
	if (is.null(nombreDepartamento)) {
		nombreDepartamento <- 
		Reduce(function(x, y) paste(x, y, sep="-"), asignaturasEnGrafico)
	}

	list(ancho=ancho, alto=alto, nombreGrafico=SinAcentos(nombreDepartamento))
}

## FUNCIONES DE CREACIÓN DE TABLAS
TabularAprobadosPorAsignatura <- function(asignatura) {
	# Produce tablas de aprobados/suspensos por curso en la asignatura dada
	# [&IMPL: Se devuelve como elemento de lista para emular vectorización]
	# @ character -> list(table)
	L <- lapply(lapply(asignatura, Seleccionar, dpto=FALSE), 
		    xtabs, formula= ~ Aprobado + Curso)
	names(L) <- asignatura
	L
}

TabularAprobadosPorDepartamento <- function(departamento) {
	# Produce tablas de aprobados/suspensos por curso en la asignatura dada
	# [&IMPL: Se devuelve como elemento de lista para emular vectorización]
	# @ character -> list(table)
	L <- lapply(lapply(departamento, Seleccionar, dpto=TRUE), 
		    xtabs, formula= ~ Aprobado + Curso)
	names(L) <- departamento
	L
}

LatexizarTabla <- function(tablas) {
	# Devuelve tabla con etiquetas de LaTeX 
	# @ list(table) -> list(xtable)
	tt <- lapply(tablas, xtable, digits=0, type="latex")

	# Da títulos a las tablas (LaTeX requiere ademas títulos sin guiones) 
	for (i in 1:length(tt))
		attr(tt[[i]], "caption") <- SinGuiones(names(tt)[[i]])
	return(tt)
}

GuardarTabla <- function(tablas, directorio="tablas") {
	# Guarda la tabla latex
	# list(xtable) -> IO
	atTabla <- AtributosTabla(tablas)
	nombreTabla <- atTabla$nombreTabla

	nombreFichero <- file.path(directorio, 
				   paste(nombreTabla, "tbl", "tex", sep="."))
	
	# imprime primera tabla a fichero y si hay más de una las añade
	# al mismo fichero [Permite manejar mejor grupos de asignaturas] 
	print(tablas[[1]], file=nombreFichero, table.placement="H")

	len <- length(tablas)
	if (len > 1) {
		restoTablas <- tail(tablas, len - 1)
		lapply(restoTablas, print, file=nombreFichero, append=TRUE,
		       table.placement="H")
	}
}

AtributosTabla <- function(tabla) {
	# Computa atributos adecuados de la tabla dada
	# list(xtable) -> list
	asignaturasEnTabla <- names(tabla)
	nombreDepartamento <- DeterminarDepartamento(asignaturasEnTabla)

	if (is.null(nombreDepartamento)) {
		nombreDepartamento <- 
		Reduce(function(x, y) paste(x, y, sep="-"), asignaturasEnTabla)
	}

	list(nombreTabla=SinAcentos(nombreDepartamento))
}

## FUNCIONES COMPARTIDAS DE AYUDA
InputValido <- function(input) {
	# Verifica que el input es valido
	# @ character -> boolean
	input %in% asignaturas || input %in% departamentos
}

Seleccionar <- function(especialidad, dpto=TRUE, data=datos) {
	# Selecciona las observaciones en los datos relativos a especialidad 
	# Si los nombres de asignatura y departamento coinciden se selecciona
	# por defecto el departamento, salvo que 'dpto' sea FALSE. 
	# (se omiten NAs)
	# @ character, data.frame -> data.frame | IO_Error
	if (!InputValido(especialidad))
		stop("Especialidad inexistente. Compruebe ortografía", 
		     call.=TRUE)

	variable <- DeterminarVariable(especialidad, dpto)
	na.omit(data[data[[variable]] %in% especialidad, ])
}

DeterminarVariable <- function(especialidad, dpto=TRUE) {
	# Determina si la especilidad dada es una asignatura o un departamento
	# Si es ambas cosas y dpto=TRUE (por defecto) se considera departamento;
	# asignatura en caso contrario.
	# ASUME: input valido = es una asignatura o departamento
	# @ character -> character
	if (!especialidad %in% asignaturas)
		res <- "Departamento"	
	else if (!especialidad %in% departamentos)
		res <- "Asignatura"
	else if (dpto == TRUE)
		res <- "Departamento"
	else
		res <- "Asignatura"

	res
}

AsignaturasEn <- function(departamento, data=datos) {
	# Devuelve las asignaturas de que consta el departamento dado
	# (Se omiten observaciones con NA en Nota para evitar introducir
	# asignaturas cuyos alumnos no hayan sido actualmente evaluados)
	# @ character -> factor
	if (!InputValido(departamento)) {
		stop("Departamento inexistente. Compruebe ortografía", 
		     call.=TRUE)
	}
		
	unique(with(data, 
		    data[Departamento == departamento & !is.na(Nota), 
			 "Asignatura"]))
}

DeterminarDepartamento <- function(asignaturas) {
	# Devuelve el nombre del departmento que integran las 
	# asignaturas dadas, si ellas son TODAS las actuales integrantes 
	# de un departamento; en caso contrario devuelve NULL
	# @ character -> character | NULL
	# !!! TODO: Como función local de parámetros PNG ??
	departamento <- NULL
	ass <- list() 

	for (dpto in departamentos) {
		ass[[dpto]] <- AsignaturasEn(dpto)
		if (!anyNA(pmatch(ass[[dpto]], asignaturas))) {
		    departamento <- dpto
		    break
		}
	}

	departamento
}

SinGuiones <- function(palabras) {
	# Substituye guiones por espacios
	gsub("(.)[_-](.)", "\\1 \\2", palabras)
}

SinAcentos <- function(palabras) {
	# Elimina signos diacríticos 
	iconv(palabras, "UTF-8", "ASCII//TRANSLIT")
}

