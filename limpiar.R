# --------------------------------
# --- LIMPIEZA DE DATOS BRUTOS --- 
# --------------------------------
# * Se eliminan filas "sucias" y filas relativas a clases colectivas, 
#   que contienen notas no oficiales.
# * Se modifica la asignatura de Conjunto para indicar su adscripcion a los
#   correspondientes departamentos que la programan (Guitarra y Piano).
# * Se añaden tres nuevas columnas: 'Departamento', 'Aprobado' y 'Curso',
#   la última concatena las variables brutas 'CURSO' y 'GRADO'
# * Se convierten a factores las variables categóricas y se dan etiquetas 
# * Se proporcionan etiquetas legibles a las Asignaturas, en lugar de los 
#   códigos presentes en los datos brutos.
# * Las notas que en origen aparecen como decimales (no oficial) se truncan
#   a enteros.
# * Se guarda un subconjunto útil para análisis exploratorio en el fichero 
#   CSV "notas_2013_2014.csv"
library(plyr)

notas2013 <- read.table("Calificaciones 2013_2014.txt", sep="\t", 
			colClasses="character", na.strings=c("", "NA"), 
			header=TRUE)

# Limpia filas y columnas 
notas2013 <- notas2013[-1, ]
notas2013 <- notas2013[!grepl("CC.", notas2013$ASIGNATURA), ]
notas2013 <- notas2013[, c("GRADO", "CURSO", "ESPECIALID", "ASIGNATURA", "NOTA_EXAME")]

# Modifica asignatura de Conjunto
filasConjunto <- which(notas2013$ESPECIALID %in% c("Gi", "Pi") & 
		       notas2013$ASIGNATURA == "Cj")
asignaturaConjunto <- paste(notas2013$ESPECIALID[filasConjunto], 
			    notas2013$ASIGNATURA[filasConjunto], sep="")
notas2013$ASIGNATURA[filasConjunto] <- asignaturaConjunto

# Convierte variables brutas a variables estadísticamente apropiadas
# [En la conversión de las notas a números se introducirán por coerción NAs
# para valores no númericos: olvidar el Warning que produzca el proceso.]
notas2013$ASIGNATURA <- factor(notas2013$ASIGNATURA)
notas2013$NOTA_EXAME <- trunc(as.numeric(sub(",", ".", notas2013$NOTA_EXAME)))

# Añade columna Departamento
departamentosDF <- read.csv("Departamentos.txt")
notas2013 <- merge(notas2013, departamentosDF, by.x="ASIGNATURA", by.y="Asignatura")

# Añade columna Aprobado
notas2013$Aprobado <- factor(notas2013$NOTA_EXAME >= 5, levels=c(TRUE, FALSE), 
			     labels=c("SÍ", "NO"))

# Añade columna 'CursoGrado' (CURSO+GRADO)
cursoGrado <- Reduce(function(x, y) paste(x, y, sep=""), 
		     notas2013[, c("CURSO", "GRADO")])
notas2013$CursoGrado <- factor(cursoGrado, 
	levels=c("1E", "2E", "3E", "4E", "1P", "2P", "3P", "4P", "5P", "6P"))

# Etiquetas legibles para Asignatura
asignaturasDF <- read.csv("Asignaturas.txt")
notas2013$ASIGNATURA <- 
	mapvalues(notas2013$ASIGNATURA, 
		  as.character(asignaturasDF$CodigoAsignatura), 
		  as.character(asignaturasDF$Asignatura))

# Subconjunto final de datos limpios
notas2013 <- notas2013[, c("Departamento", "ASIGNATURA", "CursoGrado", 
			   "NOTA_EXAME", "Aprobado")]
names(notas2013) <- c("Departamento", "Asignatura", "Curso", "Nota", "Aprobado")
write.csv(notas2013, "notas_2013_2014.csv", row.names=FALSE)
