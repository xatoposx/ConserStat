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
}

GuardarGrafico <- function(grafico, fichero, directorio="graficos") {
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
	dpto    <- grupo == "Departamento"
	formula <- as.formula(paste("~", y, "+", x))

	L <- lapply(lapply(subconjunto, Seleccionar, dpto=dpto), 
		    xtabs, formula=formula)
	names(L) <- subconjunto

        L <- if(tablaLatex) TablaLatex(L) else L	

}

TablaLatex <- function(tablas) {
	# Devuelve tabla con etiquetas de LaTeX 
	# @ list(table) -> list(xtable)
	tt <- lapply(tablas, xtable, digits=0, type="latex")

	# Da títulos a las tablas (LaTeX requiere ademas títulos sin guiones) 
	for (i in 1:length(tt))
		attr(tt[[i]], "caption") <- SinGuiones(names(tt)[[i]])
	return(tt)
}

GuardarTabla <- function(tablas, fichero, directorio="tablas") {
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
Mostrar <- function(objeto) {
	# Muestra objeto (un envoltorio de conveniencia para usuario)
	print(objeto)
}

Guardar <- function(fc) {
	# Guarda en disco el resultado de aplicar una funcion de
	# graficación o tabulación
	# La implementación se basa en la siguiente estrategia:
	# 1. Obtiene de la llamada a función (fc) los datos precisos
	#    para producir un nombre de fichero apropiado.
	# 2. Evalúa la llamada a función y pasa el resultado a funciones
	#    especializadas para guardar gráficos o tablas.
	mc <- match.call()
	mf <- mc[[-1]] # La función llamada

	c <- as.character(mf) 
	fun  <- c[1] 
	args <- c[-1] 

	if (fun == "Tabular")
	    params <- names(formals(Tabular))
	if (fun == "Graficar")
	    params <- names(formals(Graficar))

	# Menos params cuando no todos los argumentos posibles fueron pasados
	if (length(args) != length(params))
	    params <- params[1:length(args)]

	# args limpios: sin signos de llamadas internas a funciones
	aL <- mapply(function(p, r, x) gsub(p, r, x), "[[:punct:]]", "", args)
	names(aL) <- params

	# Construye nombre de fichero
	nombreFichero <- paste(aL["y"], "Por", aL["x"], "_", aL["grupo"], "_", 
			       aL["subconjunto"], sep="")

	# Llama a la funcion especializada
	if (fun == "Tabular")
		GuardarTabla(eval(mf), nombreFichero)
	if (fun == "Graficar")
		GuardarGrafico(eval(mf), nombreFichero)
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

# !!! TODO: Es esto todavía necesario?
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

SinGuiones <- function(palabras) {
	# Substituye guiones por espacios
	gsub("(.)[_-](.)", "\\1 \\2", palabras)
}

# No usada en la actual implementacion. Se mantiene por conveniencia
SinAcentos <- function(palabras) {
	# Elimina signos diacríticos 
	iconv(palabras, "UTF-8", "ASCII//TRANSLIT")
}

