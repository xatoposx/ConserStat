## FUNCIONES DE INTERFAZ DE USUARIO (GENERACIÓN DE FICHEROS PARA MEMORIA)
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
		     function(d) paste(x, y, "_", grupo, "_", d, 
				       "_", tipoGrafico, sep=""))
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
		     function(d) paste(x, y, "_", grupo, "_", d, sep=""))
	print(nf)
	tt <- lapply(departamentos,
		     function(d) Tabular(x, y, grupo, subconjunto(d)))

	mapply(Guardar, tt, nf)
}
