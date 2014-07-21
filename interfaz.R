## FUNCIONES DE INTERFAZ DE USUARIO (GENERACIÓN DE FICHEROS PARA MEMORIA)
library(knitr)
source("explorar.R")

# !!! TODO - Generalize to avoid code repetition
GenerarGraficosParaMemoria <- 
function(estadistica="AprobadoPorCurso", tipoGrafico="apilado", desglosar=FALSE) {
	# Genera imágenes PNG para la memoria y los guarda en disco
	xy <- unlist(strsplit(estadistica, "Por"))	

	x <- xy[2]
	y <- xy[1]
	grupo <- ifelse(desglosar, "Asignatura", "Departamento")
	subconjunto <- if (desglosar) AsignaturasEn else identity

	# Construye nombre de fichero y tablas
	nf <- lapply(departamentos,
		     function(d) SinAcentos(paste(x, y, "_", grupo, "_", d, 
						  "-", tipoGrafico, sep="")))
	print(nf)
	gg <- lapply(departamentos,
		     function(d) Graficar(x, y, grupo, subconjunto(d),
					  tipoGrafico=tipoGrafico))

	mapply(Guardar, gg, nf)
}

GenerarTablasParaMemoria <- 
function(estadistica="AprobadoPorCurso", desglosar=FALSE) {
	# Genera tablas LATEX para la memoria y las guarda en disco
	xy <- unlist(strsplit(estadistica, "Por"))	

	x <- xy[2]
	y <- xy[1]
	grupo <- ifelse(desglosar, "Asignatura", "Departamento")
	subconjunto <- if (desglosar) AsignaturasEn else identity

	# Construye nombre de fichero y tablas
	nf <- lapply(departamentos,
		     function(d) SinAcentos(paste(x, y, "_", grupo, 
						  "_", d, sep="")))
	print(nf)
	tt <- lapply(departamentos,
		     function(d) Tabular(x, y, grupo, subconjunto(d)))

	mapply(Guardar, tt, nf)
}

GenerarInforme <- function(nombreInforme="informe.tex") {
	# Genera un informe en pdf

	GenerarTablasParaMemoria(desglosar=TRUE)
	GenerarGraficosParaMemoria(desglosar=TRUE)

	lapply(dir("./tablas"), TablasNoFlotantes)
	GenerarFicheroTex(nombreInforme)

#	knit2pdf(nombreInforme)
}

GenerarFicheroTex <- function(fichero) {
	# Preámbulo
	preambulo <- ("
		      \\documentclass[a4paper]{article}
		      \\usepackage[T1]{fontenc}
		      \\usepackage[utf8]{inputenc}
		      \\usepackage[spanish]{babel}
		      \\usepackage{graphicx}
		      \\usepackage{booktabs}\n
		      \\renewcommand{\\arraystretch}{1.3}
		      \\pagestyle{empty}\n
		      \\title{Gráficos y Tablas}
		      \\begin{document}
		      \\maketitle
		      ")
	preambulo <- gsub("[\t ]*", "", preambulo)

	# Sección tablas 
	tituloTablas <- ("\\section{Tablas}\n")
	tablas <- unlist(lapply(dir("./tablas"), GenerarSeccion, 
				tipoSeccion="subsection*", tipoFichero="tabla"))

	# Sección gráficos
	tituloGraficos <- ("\\section{Gráficos}\n")
	graficos <- unlist(lapply(dir("./graficos"), GenerarSeccion, 
				  tipoSeccion="subsection*", tipoFichero="imagen"))

	# Final del documento
	final <- "\\end{document}"

	cat(preambulo, "\n", tituloTablas, tablas, "\n", 
	    tituloGraficos, graficos, final, sep="", file=fichero)
}

GenerarSeccion <- function(fichero, tipoSeccion, tipoFichero) {
	# Produce titulo de sección y etiqueta '\input' por fichero que incluir
	# Asumo: formato nombre fichero =
	#        estadistica_Grupo_Departamento_<*>.extension
	nombreBase <- ifelse(grepl("-", fichero),
			     strsplit(fichero, split="-")[[1]][1],
			     strsplit(fichero, split="\\.")[[1]][1]) 
	nombreDpto <- sub("([[:alpha:]]+_){2}", "", nombreBase)

	tituloSeccion <- paste("\\", tipoSeccion, "{", SinGuiones(nombreDpto), "}", sep="")

	tag <- ifelse(tipoFichero == "imagen", 
		      "\\includegraphics[scale=0.5]", 
		      "\\input")
	if (tipoFichero == "imagen")
		dir <- "graficos"

	if (tipoFichero == "tabla")
		dir <- "tablas"

	inclusionFichero <- paste(tag, "{", file.path(dir, fichero), "}", sep="")
#	nuevaPagina <- "\\newpage"

#	paste(tituloSeccion, "\n", inclusionFichero, "\n", nuevaPagina, "\n", sep="")
	paste(tituloSeccion, "\n", inclusionFichero, "\n", sep="")
}

TablasNoFlotantes <- function(fichero) {
	# Elimina tablas flotantes del contenido del fichero pero mantiene título
	#
	# [Es más fácil modificar aquí las cosas con este hack. La razón
	# es que pdflatex produce "too many floats" si el documento contiene
	# muchas tablas flotantes. Sin embargo es necesario
	# mantener la 'caption' de las tablas generadas por xtable para
	# convertirla en 'caption' de los entornos 'tabular'.
	# Esto sólo es posible en la actual implementación dejando que
	# print.xtable (función 'Guardar') genere flotantes en primera
	# instancia y modificando el resultado para el caso de un documento
	# que contiene muchas tablas como es el requerido para el informe.]
	ruta <- file.path("tablas", fichero)

	zz <- file(ruta, "r")
	lineas <- readLines(zz)
	lineas <- gsub(".*\\{table\\}.*", "", lineas)
	lineas <- gsub("caption", "captionof{table}", lineas)
	close(zz)
	unlink(ruta)

	zz <- file(ruta, "w")
	writeLines(lineas, zz)
	close(zz)
	unlink(ruta)
}


