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

## FUNCIONES PARA GRÁFICOS
Graficar <- function(x, y, grupo, subconjunto, tipoGrafico="apilado") {	
	# Produce gráfico y versus x con facetas por grupo sobre una
	# selección de los datos por el subconjunto dado
	# @ string, string, string, string, string -> ggplot
	# Ejemplo: Graficar("Curso", "Aprobado", "Asignatura", "Piano", "apilado")

	# Manejo de errores 
	# TODO!!! separar a función genérica y refinar según casos de uso
	if (!all(c(x, y, grupo) %in% names(datos)))
		stop("Los datos no contienen alguna de las variables x, y, grupo.")
	if (!(subconjunto %in% asignaturas || subconjunto %in% departamentos))
		stop("La especialidad no existe en departamentos o asignaturas")
	if (grupo == "Departamento" && !subconjunto %in% departamentos)
		stop("Si grupo es \"Departamento\", subconjunto debe serlo también.")
	if (grupo == "Asignatura" && !subconjunto %in% asignaturas)
		stop("Si grupo es \"Asignatura\", subconjunto debe serlo también.")

	dpto    <- grupo == "Departamento"
	formula <- as.formula(paste("~", grupo))

	g <- ggplot(Seleccionar(subconjunto, dpto=dpto), aes_string(x=x, fill=y)) +
	facet_wrap(formula, ncol=ncolFacets, scales="fixed") +
	labs(y="Número de Alumnos")

	if (tipoGrafico == "apilado")
		g <- g + geom_bar(position="stack")
	else if (tipoGrafico == "agrupado")
		g <- g + geom_bar(position="dodge")
	else if (tipoGrafico == "polinomio")
		g <- g + geom_freqpoly(aes_string(group=grupo, colour=grupo))

	return(g)
}

GuardarGrafico <- function(grafico, fichero="grafico", directorio="graficos") {
	# Guarda el gráfico como fichero png
	# @ ggplot -> IO
	atPng <- AtributosPng(grafico)
	ancho <- atPng$ancho
	alto <- atPng$alto
	ruta <- file.path(directorio, paste(fichero, "png", sep="."))

	png(ruta, ancho, alto)
	print(grafico)
	dev.off()
}

AtributosPng <- function(grafico) {
	# Computa atributos adecuadas a la imagen pgn a partir del grafico dado
	# [@IMPL: Se usa introspección. La implementación es más compleja de
	# de este modo, pero permite desacoplar las funciones]
	# @ ggplot -> list

	# !!! TODO - Eliminar magic numbers
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

	list(ancho=ancho, alto=alto)
}

## FUNCIONES PARA TABLAS
Tabular <- function(x, y, grupo, subconjunto, tablaLatex=FALSE) {
	# Produce tabla y versus x por grupo sobre una selección de los 
	# datos por el subconjunto dado
	# @ string, string, string, string, boolean -> table | xtable
	# Ejemplo: Tabular("Curso", "Aprobado", "Asignatura", "Piano", TRUE)

	# Manejo de errores 
	# TODO!!! separar a función genérica y refinar según casos de uso
	if (!all(c(x, y, grupo) %in% names(datos)))
		stop("Los datos no contienen alguna de las variables x, y, grupo.")
	if (!(subconjunto %in% asignaturas || subconjunto %in% departamentos))
		stop("La especialidad no existe en departamentos o asignaturas")
	if (grupo == "Departamento" && !subconjunto %in% departamentos)
		stop("Si grupo es \"Departamento\", subconjunto debe serlo también.")
	if (grupo == "Asignatura" && !subconjunto %in% asignaturas)
		stop("Si grupo es \"Asignatura\", subconjunto debe serlo también.")

	dpto    <- grupo == "Departamento"
	formula <- as.formula(paste("~", y, "+", x))

	L <- lapply(lapply(subconjunto, Seleccionar, dpto=dpto), 
		    xtabs, formula=formula)
	names(L) <- subconjunto

        L <- if(tablaLatex) TablaLatex(L) else L	

	return(L)
}

TablaLatex <- function(tablas) {
	# Devuelve tabla con etiquetas de LaTeX 
	# @ list(table) -> list(xtable)
	tt <- lapply(tablas, xtable, digits=0, type="latex")

	# Da títulos a las tablas (LaTeX requiere títulos sin guiones) 
	for (i in 1:length(tt))
		attr(tt[[i]], "caption") <- SinGuiones(names(tt)[[i]])
	return(tt)
}

GuardarTabla <- function(tablas, fichero="tabla", directorio="tablas") {
	# Guarda la tabla latex
	# @ list(xtable) -> IO
	if (!"xtable" %in% class(tablas[[1]])) 
		tablas <- TablaLatex(tablas)

	ruta <- file.path(directorio, paste(fichero, "tbl", "tex", sep="."))
	
	# imprime primero tabla a fichero y si hay más de una las añade
	# al mismo fichero
	print(tablas[[1]], file=ruta, table.placement="H")

	len <- length(tablas)
	if (len > 1) {
		restoTablas <- tail(tablas, len - 1)
		lapply(restoTablas, print, file=ruta, append=TRUE,
		       table.placement="H")
	}
}

## FUNCIONES GENÉRICAS
Guardar <- function(objeto, nombreFichero) {
	# Guarda objeto (Wrapper)
	# @ list(table) | ggplot -> funcall
	if (is.table(objeto[[1]]))
		GuardarTabla(objeto, nombreFichero)
	if (is.ggplot(objeto))
		GuardarGrafico(objeto, nombreFichero)
}	

## FUNCIONES COMPARTIDAS DE AYUDA
Seleccionar <- function(especialidad, dpto, data=datos) {
	variable <- ifelse(dpto, "Departamento", "Asignatura")
	na.omit(data[data[[variable]] %in% especialidad, ])
}

AsignaturasEn <- function(departamento, data=datos) {
	# Devuelve las asignaturas de que consta el departamento dado
	# (Se omiten observaciones con NA en Nota para evitar introducir
	# asignaturas cuyos alumnos no hayan sido actualmente evaluados)
	# @ character -> factor
	
	# !!! TODO: Modificar cuando Generic Error Handling se implemente
	if (!departamento %in% departamentos) {
		stop("Departamento inexistente.")
	}
		
	unique(with(data, 
		    data[Departamento == departamento & !is.na(Nota), 
			 "Asignatura"]))
}

SinGuiones <- function(palabras) {
	# Substituye guiones por espacios
	gsub("(.)[_-](.)", "\\1 \\2", palabras)
}

# No usada en la actual implementacion. Se mantiene por conveniencia
SinAcentos <- function(palabras) {
	# Elimina signos diacríticos 
	iconv(palabras, "UTF-8", "ASCII//TRANSLIT")
}

