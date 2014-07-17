library(ggplot2)
library(xtable)

source("limpiar.R")

## CONSTANTES
# Importadas desde 'limpiar.R' 
datos <- notas2013
departamentos <- levels(departamentosDF$Departamento)
asignaturas <- levels(asignaturasDF$Asignatura)

# Parámetros para gráficos ggplot
ncolFacets <- 2

## FUNCIONES DE CREACIÓN DE GRÁFICOS
GraficarAprobadosPorAsignatura <- function(asignatura) {
        # Produce gráfico de aprobados/suspensos por curso en la asignatura dada
	# @ character, character -> ggplot
	g <- ggplot(Seleccionar(asignatura), aes(x=Curso, fill=Aprobado)) +
	facet_wrap(~ Asignatura, ncol=ncolFacets, scales="fixed") +
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

GuardarGrafico <- function(grafico, directorio="graficos") {
	# Guarda el gráfico como fichero png
	# @ ggplot -> IO
	atPng <- AtributosPng(grafico)
	nombreGrafico <- atPng$nombreGrafico
	ancho <- atPng$ancho
	alto <- atPng$alto

	nombreFichero <- file.path(directorio, paste(nombreGrafico, "png", sep="."))
	png(nombreFichero, ancho, alto)
	print(grafico)
	dev.off()
}

AtributosPng <- function(grafico) {
	# Computa atributos adecuadas de la imagen pgn a partir del grafico dado
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
TabularAprobadosPorAsignaturaOLD <- function(asignatura) {
	if (length(asignatura) == 1) {
		L <- list(xtabs(~ Aprobado + Curso, Seleccionar(asignatura)))
		names(L) <- asignatura
		L

	} else {
		L <- sapply(asignatura, TabularAprobadosPorAsignatura, 
			    USE.NAMES=FALSE)
	}

	if (tipoTabla == "latex")
		L <- lapply(L, xtable, digits=0, type="latex")

	return(L) # !!! TODO: FOR DEBUGGING PURPOSES
}

TabularAprobadosPorAsignatura <- function(asignatura) {
	# Produce tablas de aprobados/suspensos por curso en la asignatura dada
	# [&IMPL: Se devuelve como elemento de lista para emular vectorización]
	# @ character -> list(table)
	L <- lapply(lapply(asignatura, Seleccionar), 
		    xtabs, formula= ~ Aprobado + Curso)
	names(L) <- asignatura
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

# FUNCIONES COMPARTIDAS DE AYUDA
InputValido <- function(input) {
	# Verifica que el input es valido
	# @ character -> boolean
	input %in% asignaturas || input %in% departamentos
}

Seleccionar <- function(asignatura, data=datos) {
	# Selecciona las observaciones en los datos relativos a asignatura 
	# (se omiten NAs)
	# @ character, data.frame -> data.frame | IO_Error
	if (!InputValido(asignatura))
		stop("Asignatura inexistente. Compruebe ortografía", call.=TRUE)

	na.omit(data[data$Asignatura %in% asignatura, ])
}

AsignaturasEn <- function(departamento, data=datos) {
	# Devuelve las asignaturas de que consta el departamento dado
	# @ character -> factor
	if (!InputValido(departamento)) {
		stop("Departamento inexistente. Compruebe ortografía", call.=TRUE)
	}
		
	unique(data[data$Departamento == departamento, "Asignatura"])
}

DeterminarDepartamento <- function(asignaturas) {
	# Devuelve el nombre del departmento que integran las asignaturas
        # dadas, si ellas son TODAS las integrantes de un departamento; 
	# en caso contrario devuelve NULL
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

## WRAPPERS PARA INTERFAZ DE USUARIO: FACHADA
# !!! TODO: Revisar estas funciones
DibujarAprobadosPorAsignatura <- 
function(asignaturas, tipo="apilado", guardar="si") {
	# Dibuja gráfico de aprobados/suspensos por curso en las asignaturas dadas
	#
	# Args:
	#     asignaturas: vector de nombre(s) de asignatura(s)
	#     tipo:        tipo de gráfico: "apilado" | "agrupado" | "polinomio"
	#                  Se muestra un gráfico de barras apiladas por defecto
	#     guardar:     "si" | "no"
	#                  El gráfico se guarda por defecto en un fichero pgn con el
	#                  nombre de las asignaturas
	#
	# Returns:
	#     gráfico construido con ggplot. Crea fichero PNG por defecto
	if (tipo == "apilado")
		res <- 
		DibujarBarrasApiladas(GraficarAprobadosPorAsignatura(asignaturas))
	if (tipo == "agrupado")
		res <- 
		DibujarBarrasAgrupadas(GraficarAprobadosPorAsignatura(asignaturas))
	if (tipo == "poligono")
		res <- 
		DibujarPoligono(GraficarAprobadosPorAsignatura(asignatura))

	if (guardar == "si")
		GuardarGrafico(res)

	print(res)
}

MostrarTablaDeAprobadosPorAsignatura <- 
function(asignaturas, guardar="si") {
	# Muestra tabla de aprobados/suspensos por curso en las asignaturas dadas
	#
	# Args:
	#     asignaturas: vector de nombre(s) de asignatura(s)
	#     guardar:     "si" | "no"
	#                  La tabla se guarda por defecto en un fichero tex con el
	#                  nombre de las asignaturas seguido del sufijo '.tbl' 
	# Returns:
	#     tabla en consola. Crea fichero LaTeX por defecto
	tt <- TabularAprobadosPorAsignatura(asignaturas)

	if (guardar == "si")
		GuardarTabla(invisible(LatexizarTabla(t)))

	tt
}

CrearGraficosDeAprobadosPorAsignatura <- 
function(tipo="barras") {
	# Genera gráficos de aprobados/suspensos por curso para las asignaturas
	# de todos los departamentos. Los resultados se guardan en sendos ficheros
	# con nombre 'departamento'.png en el subdirectorio 'graficos'.
	#
	# Args:
	#
	#     tipo: tipo de gráfico: "apilado" | "agrupado" | "polinomio"
	#           Se genera un gráfico de barras apiladas por defecto
	# Returns:
	#     ficheros de imagenes png
	lapply(departamentos,
	       function(dpto) DibujarAprobadosPorAsignatura(AsignaturasEn(dpto),
							    tipo=tipo,
							    guardar="si"))
}
