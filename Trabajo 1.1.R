# ============================================================
#             PROGRAMA ESTADISTICO EN R
# ============================================================
# Incluye:
# - Estadistica descriptiva e inferencial
# - Poblacion y muestra
# - Tipos de variables
# - Niveles de medicion
# - Media, mediana y moda
# - Datos agrupados y no agrupados
# - Percentiles y cuartiles
# - Varianza y desviacion estandar
# - Frecuencia absoluta, relativa y porcentual
# - Histograma
# - Poligono de frecuencias
# - Diagrama de pastel
# - Diagrama de caja
# - Resumen de cinco numeros
# - Tablas cruzadas o de contingencia
# ============================================================


# ============================================================
# FUNCION: PAUSA
# ============================================================

pausa <- function() {
  invisible(readline("\nPresione ENTER para continuar..."))
}


# ============================================================
# FUNCION: VALIDAR OPCION
# ============================================================

leer_opcion <- function(mensaje, opciones_validas) {
  opcion <- trimws(readline(mensaje))
  while (!(opcion %in% opciones_validas)) {
    cat("\nERROR: Opcion no valida.\n")
    opcion <- trimws(readline(mensaje))
  }
  return(opcion)
}


# ============================================================
# FUNCION: MODA
# ============================================================

calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  frecuencias <- table(x)
  max_frecuencia <- max(frecuencias)
  # Si todos aparecen la misma cantidad de veces
  if (max_frecuencia == 1) {
    return("No existe moda")
  }
  modas <- names(frecuencias[frecuencias == max_frecuencia])
  return(paste(modas, collapse = ", "))
}


# ============================================================
# FUNCION: ELEGIR VARIABLE NUMERICA
# ============================================================

seleccionar_variable_numerica <- function(datos) {
  columnas_numericas <- which(sapply(datos, is.numeric))
  if (length(columnas_numericas) == 0) {
    cat("\nNo existen variables numericas en los datos.\n")
    return(NULL)
  }
  cat("\nVARIABLES NUMERICAS DISPONIBLES\n")
  cat("--------------------------------------------\n")
  for (i in columnas_numericas) {
    cat(i, "-", names(datos)[i], "\n")
  }
  seleccion <- suppressWarnings(
    as.integer(readline("\nSeleccione el numero de la variable: "))
  )
  while (is.na(seleccion) ||
         !(seleccion %in% columnas_numericas)) {
    cat("\nERROR: Seleccione una variable numerica valida.\n")
    seleccion <- suppressWarnings(
      as.integer(readline("Seleccione el numero de la variable: "))
    )
  }
  return(seleccion)
}


# ============================================================
# FUNCION: CARGAR ARCHIVO
# ============================================================

cargar_archivo <- function() {
  cat("\n============================================\n")
  cat("          CARGAR ARCHIVO\n")
  cat("============================================\n")
  cat("Seleccione un archivo .CSV o .TXT\n\n")
  archivo <- tryCatch(
    file.choose(),
    error = function(e) NULL
  )
  if (is.null(archivo)) {
    cat("\nNo se selecciono ningun archivo.\n")
    return(NULL)
  }
  extension <- tolower(tools::file_ext(archivo))
  if (!(extension %in% c("csv", "txt"))) {
    cat("\nERROR: Solo se permiten archivos .csv o .txt\n")
    return(NULL)
  }
  datos <- tryCatch({
    if (extension == "csv") {
      read.csv(
        archivo,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      read.table(
        archivo,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }, error = function(e) {
    cat("\nERROR AL LEER EL ARCHIVO:\n")
    cat(e$message, "\n")
    return(NULL)
  })
  if (!is.null(datos)) {
    cat("\nArchivo cargado correctamente.\n")
    cat("\nNombre del archivo:\n")
    cat(basename(archivo), "\n")
    cat("\nNumero de filas:", nrow(datos), "\n")
    cat("Numero de columnas:", ncol(datos), "\n")
    cat("\nPRIMEROS REGISTROS:\n")
    print(head(datos))
  }
  return(datos)
}


# ============================================================
# FUNCION: INGRESO MANUAL
# ============================================================

ingreso_manual <- function() {
  cat("\n============================================\n")
  cat("       INGRESO MANUAL DE DATOS\n")
  cat("============================================\n")
  datos_correctos <- FALSE
  while (!datos_correctos) {
    entrada <- readline(
      paste0(
        "\nIngrese numeros separados por comas.\n",
        "Ejemplo: 10,20,30,40,50\n\n",
        "Datos: "
      )
    )
    partes <- strsplit(entrada, ",")[[1]]
    valores <- suppressWarnings(
      as.numeric(trimws(partes))
    )
    if (length(valores) > 0 &&
        !any(is.na(valores))) {
      datos_correctos <- TRUE
    } else {
      cat("\nERROR: Ingrese solamente numeros separados por comas.\n")
    }
  }
  datos <- data.frame(Valor = valores)
  cat("\nDatos ingresados correctamente:\n")
  print(datos)
  return(datos)
}


# ============================================================
# CONCEPTOS BASICOS
# ============================================================

mostrar_conceptos <- function() {
  opcion <- ""
  while (opcion != "0") {
    cat("\n============================================\n")
    cat("         CONCEPTOS DE ESTADISTICA\n")
    cat("============================================\n")
    cat("1. Estadistica descriptiva\n")
    cat("2. Estadistica inferencial\n")
    cat("3. Poblacion\n")
    cat("4. Muestra\n")
    cat("5. Tipos de variables\n")
    cat("6. Niveles de medicion\n")
    cat("0. Regresar\n")
    cat("============================================\n")
    opcion <- leer_opcion(
      "Seleccione una opcion: ",
      as.character(0:6)
    )
    if (opcion == "1") {
      cat("\nESTADISTICA DESCRIPTIVA\n")
      cat("--------------------------------------------\n")
      cat(
        "Es la rama de la estadistica que organiza,\n",
        "resume y presenta un conjunto de datos mediante\n",
        "tablas, graficas y medidas numericas.\n"
      )
      pausa()
    }
    if (opcion == "2") {
      cat("\nESTADISTICA INFERENCIAL\n")
      cat("--------------------------------------------\n")
      cat(
        "Permite obtener conclusiones sobre una poblacion\n",
        "a partir de la informacion obtenida de una muestra.\n"
      )
      pausa()
    }
    if (opcion == "3") {
      cat("\nPOBLACION\n")
      cat("--------------------------------------------\n")
      cat(
        "Es el conjunto total de individuos, objetos o\n",
        "elementos que se desean estudiar.\n"
      )
      pausa()
    }
    if (opcion == "4") {
      cat("\nMUESTRA\n")
      cat("--------------------------------------------\n")
      cat(
        "Es un subconjunto de la poblacion seleccionado\n",
        "para realizar un estudio estadistico.\n"
      )
      pausa()
    }
    if (opcion == "5") {
      cat("\nTIPOS DE VARIABLES\n")
      cat("--------------------------------------------\n")
      cat("1. Cualitativas\n")
      cat("   - Nominales\n")
      cat("   - Ordinales\n\n")
      cat("2. Cuantitativas\n")
      cat("   - Discretas\n")
      cat("   - Continuas\n")
      pausa()
    }
    if (opcion == "6") {
      cat("\nNIVELES DE MEDICION\n")
      cat("--------------------------------------------\n")
      cat("1. Nominal\n")
      cat("2. Ordinal\n")
      cat("3. Intervalo\n")
      cat("4. Razon\n")
      pausa()
    }
  }
}


# ============================================================
# MEDIDAS PARA DATOS NO AGRUPADOS
# ============================================================

datos_no_agrupados <- function(datos) {
  columna <- seleccionar_variable_numerica(datos)
  if (is.null(columna)) {
    pausa()
    return()
  }
  x <- datos[[columna]]
  x <- x[!is.na(x)]
  cat("\n============================================\n")
  cat("       DATOS NO AGRUPADOS\n")
  cat("============================================\n")
  cat("\nVariable:", names(datos)[columna], "\n")
  cat("\nNumero de datos:", length(x), "\n")
  cat("\nMedia:")
  print(mean(x))
  cat("\nMediana:")
  print(median(x))
  cat("\nModa:")
  print(calcular_moda(x))
  cat("\nVarianza muestral:")
  print(var(x))
  cat("\nDesviacion estandar muestral:")
  print(sd(x))
  cat("\nValor minimo:")
  print(min(x))
  cat("\nValor maximo:")
  print(max(x))
  cat("\nRango:")
  print(max(x) - min(x))
  pausa()
}


# ============================================================
# CREAR TABLA DE DATOS AGRUPADOS
# ============================================================

crear_datos_agrupados <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2 || min(x) == max(x)) {
    cat("\nSe necesitan al menos dos valores diferentes.\n")
    return(NULL)
  }
  # Regla de Sturges
  k <- ceiling(1 + 3.322 * log10(n))
  rango <- max(x) - min(x)
  amplitud <- rango / k
  limites <- seq(
    from = min(x),
    to = max(x) + amplitud,
    length.out = k + 1
  )
  clases <- cut(
    x,
    breaks = limites,
    include.lowest = TRUE,
    right = TRUE
  )
  fi <- as.numeric(table(clases))
  inferior <- head(limites, -1)
  superior <- tail(limites, -1)
  marca_clase <- (inferior + superior) / 2
  Fi <- cumsum(fi)
  fr <- fi / sum(fi)
  porcentaje <- fr * 100
  tabla <- data.frame(
    Clase = levels(clases),
    Limite_Inferior = round(inferior, 4),
    Limite_Superior = round(superior, 4),
    Marca_Clase = round(marca_clase, 4),
    Frecuencia = fi,
    Frecuencia_Acumulada = Fi,
    Frecuencia_Relativa = round(fr, 4),
    Frecuencia_Porcentual = round(porcentaje, 2)
  )
  return(tabla)
}


# ============================================================
# MEDIDAS PARA DATOS AGRUPADOS
# ============================================================

datos_agrupados <- function(datos) {
  columna <- seleccionar_variable_numerica(datos)
  if (is.null(columna)) {
    pausa()
    return()
  }
  x <- datos[[columna]]
  tabla <- crear_datos_agrupados(x)
  if (is.null(tabla)) {
    pausa()
    return()
  }
  cat("\n============================================\n")
  cat("          DATOS AGRUPADOS\n")
  cat("============================================\n")
  print(tabla, row.names = FALSE)
  fi <- tabla$Frecuencia
  xi <- tabla$Marca_Clase
  n <- sum(fi)
  # Media agrupada
  media_agrupada <- sum(fi * xi) / n
  # Varianza agrupada muestral
  if (n > 1) {
    varianza_agrupada <-
      sum(fi * (xi - media_agrupada)^2) / (n - 1)
  } else {
    varianza_agrupada <- NA
  }
  desviacion_agrupada <- sqrt(varianza_agrupada)
  # Mediana agrupada
  posicion_mediana <- n / 2
  clase_mediana <- which(
    tabla$Frecuencia_Acumulada >= posicion_mediana
  )[1]
  F_anterior <- ifelse(
    clase_mediana == 1,
    0,
    tabla$Frecuencia_Acumulada[clase_mediana - 1]
  )
  L <- tabla$Limite_Inferior[clase_mediana]
  f_mediana <- fi[clase_mediana]
  c_amplitud <-
    tabla$Limite_Superior[clase_mediana] -
    tabla$Limite_Inferior[clase_mediana]
  mediana_agrupada <-
    L +
    ((posicion_mediana - F_anterior) /
       f_mediana) *
    c_amplitud
  # Moda agrupada
  clase_modal <- which.max(fi)
  f1 <- fi[clase_modal]
  f0 <- ifelse(
    clase_modal == 1,
    0,
    fi[clase_modal - 1]
  )
  f2 <- ifelse(
    clase_modal == length(fi),
    0,
    fi[clase_modal + 1]
  )
  L_modal <- tabla$Limite_Inferior[clase_modal]
  amplitud_modal <-
    tabla$Limite_Superior[clase_modal] -
    tabla$Limite_Inferior[clase_modal]
  denominador <- (f1 - f0) + (f1 - f2)
  if (denominador == 0) {
    moda_agrupada <- tabla$Marca_Clase[clase_modal]
  } else {
    moda_agrupada <-
      L_modal +
      ((f1 - f0) / denominador) *
      amplitud_modal
  }
  cat("\n--------------------------------------------\n")
  cat("RESULTADOS PARA DATOS AGRUPADOS\n")
  cat("--------------------------------------------\n")
  cat("\nMedia aproximada:", round(media_agrupada, 4), "\n")
  cat(
    "Mediana aproximada:",
    round(mediana_agrupada, 4),
    "\n"
  )
  cat(
    "Moda aproximada:",
    round(moda_agrupada, 4),
    "\n"
  )
  cat(
    "Varianza agrupada:",
    round(varianza_agrupada, 4),
    "\n"
  )
  cat(
    "Desviacion estandar agrupada:",
    round(desviacion_agrupada, 4),
    "\n"
  )
  pausa()
}


# ============================================================
# PERCENTILES Y CUARTILES
# ============================================================

percentiles_cuartiles <- function(datos) {
  columna <- seleccionar_variable_numerica(datos)
  if (is.null(columna)) {
    pausa()
    return()
  }
  x <- datos[[columna]]
  x <- x[!is.na(x)]
  cat("\n============================================\n")
  cat("        PERCENTILES Y CUARTILES\n")
  cat("============================================\n")
  cat("\nCUARTILES\n")
  cuartiles <- quantile(
    x,
    probs = c(0, 0.25, 0.50, 0.75, 1)
  )
  print(cuartiles)
  cat("\nIngrese un percentil entre 1 y 99.\n")
  p <- suppressWarnings(
    as.numeric(readline("Percentil: "))
  )
  while (is.na(p) || p < 1 || p > 99) {
    cat("\nERROR: Debe ingresar un numero de 1 a 99.\n")
    p <- suppressWarnings(
      as.numeric(readline("Percentil: "))
    )
  }
  resultado <- quantile(x, probs = p / 100)
  cat(
    "\nPercentil",
    p,
    "=",
    as.numeric(resultado),
    "\n"
  )
  pausa()
}


# ============================================================
# DISTRIBUCIONES DE FRECUENCIA
# ============================================================

tabla_frecuencias <- function(datos) {
  cat("\n============================================\n")
  cat("       DISTRIBUCION DE FRECUENCIAS\n")
  cat("============================================\n")
  cat("\nVariables disponibles:\n")
  for (i in seq_along(datos)) {
    cat(i, "-", names(datos)[i], "\n")
  }
  columna <- suppressWarnings(
    as.integer(readline("\nSeleccione una variable: "))
  )
  while (is.na(columna) ||
         columna < 1 ||
         columna > ncol(datos)) {
    cat("\nERROR: Seleccion invalida.\n")
    columna <- suppressWarnings(
      as.integer(readline("Seleccione una variable: "))
    )
  }
  x <- datos[[columna]]
  x <- x[!is.na(x)]
  fi <- table(x)
  fr <- prop.table(fi)
  fp <- fr * 100
  tabla <- data.frame(
    Valor = names(fi),
    Frecuencia_Absoluta = as.numeric(fi),
    Frecuencia_Relativa = round(as.numeric(fr), 4),
    Frecuencia_Porcentual = round(as.numeric(fp), 2)
  )
  cat("\nTABLA DE FRECUENCIAS\n\n")
  print(tabla, row.names = FALSE)
  pausa()
}


# ============================================================
# GRAFICAS
# ============================================================

menu_graficas <- function(datos) {
  opcion <- ""
  while (opcion != "0") {
    cat("\n============================================\n")
    cat("                GRAFICAS\n")
    cat("============================================\n")
    cat("1. Histograma\n")
    cat("2. Poligono de frecuencias\n")
    cat("3. Diagrama de pastel\n")
    cat("4. Diagrama de caja\n")
    cat("0. Regresar\n")
    cat("============================================\n")
    opcion <- leer_opcion(
      "Seleccione una opcion: ",
      c("0", "1", "2", "3", "4")
    )
    # --------------------------------------------------------
    # HISTOGRAMA
    # --------------------------------------------------------
    if (opcion == "1") {
      columna <- seleccionar_variable_numerica(datos)
      if (!is.null(columna)) {
        x <- datos[[columna]]
        x <- x[!is.na(x)]
        hist(
          x,
          main = paste(
            "Histograma de",
            names(datos)[columna]
          ),
          xlab = names(datos)[columna],
          ylab = "Frecuencia"
        )
        cat("\nHistograma generado.\n")
        pausa()
      }
    }
    # --------------------------------------------------------
    # POLIGONO
    # --------------------------------------------------------
    if (opcion == "2") {
      columna <- seleccionar_variable_numerica(datos)
      if (!is.null(columna)) {
        x <- datos[[columna]]
        tabla <- crear_datos_agrupados(x)
        if (!is.null(tabla)) {
          plot(
            tabla$Marca_Clase,
            tabla$Frecuencia,
            type = "o",
            main = "Poligono de Frecuencias",
            xlab = "Marca de clase",
            ylab = "Frecuencia"
          )
          cat("\nPoligono de frecuencias generado.\n")
          pausa()
        }
      }
    }
    # --------------------------------------------------------
    # PASTEL
    # --------------------------------------------------------
    if (opcion == "3") {
      cat("\nVariables disponibles:\n")
      for (i in seq_along(datos)) {
        cat(i, "-", names(datos)[i], "\n")
      }
      columna <- suppressWarnings(
        as.integer(
          readline("\nSeleccione una variable: ")
        )
      )
      while (is.na(columna) ||
             columna < 1 ||
             columna > ncol(datos)) {
        cat("\nERROR: Variable no valida.\n")
        columna <- suppressWarnings(
          as.integer(
            readline("Seleccione una variable: ")
          )
        )
      }
      frecuencias <- table(datos[[columna]])
      pie(
        frecuencias,
        main = paste(
          "Diagrama de pastel de",
          names(datos)[columna]
        )
      )
      cat("\nDiagrama de pastel generado.\n")
      pausa()
    }
    # --------------------------------------------------------
    # CAJA
    # --------------------------------------------------------
    if (opcion == "4") {
      columna <- seleccionar_variable_numerica(datos)
      if (!is.null(columna)) {
        x <- datos[[columna]]
        boxplot(
          x,
          main = paste(
            "Diagrama de caja de",
            names(datos)[columna]
          ),
          ylab = names(datos)[columna]
        )
        cat("\nDiagrama de caja generado.\n")
        pausa()
      }
    }
  }
}


# ============================================================
# RESUMEN DE CINCO NUMEROS
# ============================================================

resumen_cinco_numeros <- function(datos) {
  columna <- seleccionar_variable_numerica(datos)
  if (is.null(columna)) {
    pausa()
    return()
  }
  x <- datos[[columna]]
  x <- x[!is.na(x)]
  resumen <- quantile(
    x,
    probs = c(0, 0.25, 0.50, 0.75, 1)
  )
  cat("\n============================================\n")
  cat("       RESUMEN DE CINCO NUMEROS\n")
  cat("============================================\n")
  cat("\nMinimo :", resumen[1], "\n")
  cat("Q1     :", resumen[2], "\n")
  cat("Mediana:", resumen[3], "\n")
  cat("Q3     :", resumen[4], "\n")
  cat("Maximo :", resumen[5], "\n")
  cat("\nRango intercuartilico (IQR):",
      IQR(x),
      "\n")
  pausa()
}


# ============================================================
# TABLAS CRUZADAS O DE CONTINGENCIA
# ============================================================

tabla_contingencia <- function(datos) {
  if (ncol(datos) < 2) {
    cat(
      "\nSe necesitan al menos dos variables para crear una tabla cruzada.\n"
    )
    pausa()
    return()
  }
  cat("\n============================================\n")
  cat("       TABLA DE CONTINGENCIA\n")
  cat("============================================\n")
  cat("\nVariables disponibles:\n")
  for (i in seq_along(datos)) {
    cat(i, "-", names(datos)[i], "\n")
  }
  v1 <- suppressWarnings(
    as.integer(readline("\nSeleccione la primera variable: "))
  )
  while (is.na(v1) || v1 < 1 || v1 > ncol(datos)) {
    cat("\nERROR: Variable no valida.\n")
    v1 <- suppressWarnings(
      as.integer(readline("Primera variable: "))
    )
  }
  v2 <- suppressWarnings(
    as.integer(readline("Seleccione la segunda variable: "))
  )
  while (
    is.na(v2) ||
    v2 < 1 ||
    v2 > ncol(datos) ||
    v2 == v1
  ) {
    cat(
      "\nERROR: Seleccione una variable diferente y valida.\n"
    )
    v2 <- suppressWarnings(
      as.integer(readline("Segunda variable: "))
    )
  }
  tabla <- table(
    datos[[v1]],
    datos[[v2]],
    useNA = "ifany"
  )
  cat("\nTABLA DE CONTINGENCIA\n\n")
  print(tabla)
  cat("\nFRECUENCIAS RELATIVAS\n\n")
  print(round(prop.table(tabla), 4))
  cat("\nPORCENTAJES\n\n")
  print(round(prop.table(tabla) * 100, 2))
  pausa()
}


# ============================================================
# ESTADISTICA INFERENCIAL
# ============================================================

estadistica_inferencial <- function(datos) {
  columna <- seleccionar_variable_numerica(datos)
  if (is.null(columna)) {
    pausa()
    return()
  }
  x <- datos[[columna]]
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    cat("\nSe necesitan al menos dos observaciones.\n")
    pausa()
    return()
  }
  cat("\n============================================\n")
  cat("        ESTADISTICA INFERENCIAL\n")
  cat("============================================\n")
  cat("\nVariable:", names(datos)[columna], "\n")
  cat("\nMedia muestral:", mean(x), "\n")
  cat(
    "Desviacion estandar muestral:",
    sd(x),
    "\n"
  )
  cat(
    "Tamano de la muestra:",
    length(x),
    "\n"
  )
  # Intervalo de confianza de la media al 95%
  prueba <- t.test(x, conf.level = 0.95)
  cat("\nINTERVALO DE CONFIANZA DEL 95% PARA LA MEDIA\n")
  cat(
    "Limite inferior:",
    prueba$conf.int[1],
    "\n"
  )
  cat(
    "Limite superior:",
    prueba$conf.int[2],
    "\n"
  )
  pausa()
}


# ============================================================
# MENU DE ANALISIS ESTADISTICO
# ============================================================

menu_analisis <- function(datos) {
  opcion <- ""
  while (opcion != "0") {
    cat("\n============================================\n")
    cat("         ANALISIS ESTADISTICO\n")
    cat("============================================\n")
    cat("1. Datos no agrupados\n")
    cat("2. Datos agrupados\n")
    cat("3. Percentiles y cuartiles\n")
    cat("4. Distribucion de frecuencias\n")
    cat("5. Graficas\n")
    cat("6. Resumen de cinco numeros\n")
    cat("7. Tabla cruzada o de contingencia\n")
    cat("8. Estadistica inferencial\n")
    cat("0. Regresar al menu principal\n")
    cat("============================================\n")
    opcion <- leer_opcion(
      "Seleccione una opcion: ",
      as.character(0:8)
    )
    if (opcion == "1") {
      datos_no_agrupados(datos)
    }
    if (opcion == "2") {
      datos_agrupados(datos)
    }
    if (opcion == "3") {
      percentiles_cuartiles(datos)
    }
    if (opcion == "4") {
      tabla_frecuencias(datos)
    }
    if (opcion == "5") {
      menu_graficas(datos)
    }
    if (opcion == "6") {
      resumen_cinco_numeros(datos)
    }
    if (opcion == "7") {
      tabla_contingencia(datos)
    }
    if (opcion == "8") {
      estadistica_inferencial(datos)
    }
  }
}


# ============================================================
#                  PROGRAMA PRINCIPAL
# ============================================================

datos <- NULL

salir <- FALSE


while (!salir) {
  cat("\n\n")
  cat("====================================================\n")
  cat("             PROGRAMA ESTADISTICO EN R\n")
  cat("====================================================\n")
  cat("1. Cargar datos desde archivo (.txt o .csv)\n")
  cat("2. Ingresar datos manualmente\n")
  cat("3. Conceptos basicos de estadistica\n")
  cat("4. Realizar analisis estadistico\n")
  cat("5. Mostrar datos cargados\n")
  cat("6. Eliminar datos actuales\n")
  cat("0. Salir\n")
  cat("====================================================\n")
  opcion_principal <- leer_opcion(
    "Seleccione una opcion: ",
    c("0", "1", "2", "3", "4", "5", "6")
  )
  # ----------------------------------------------------------
  # OPCION 1
  # ----------------------------------------------------------
  if (opcion_principal == "1") {
    nuevos_datos <- cargar_archivo()
    if (!is.null(nuevos_datos)) {
      datos <- nuevos_datos
    }
    pausa()
  }
  # ----------------------------------------------------------
  # OPCION 2
  # ----------------------------------------------------------
  if (opcion_principal == "2") {
    datos <- ingreso_manual()
    pausa()
  }
  # ----------------------------------------------------------
  # OPCION 3
  # ----------------------------------------------------------
  if (opcion_principal == "3") {
    mostrar_conceptos()
  }
  # ----------------------------------------------------------
  # OPCION 4
  # ----------------------------------------------------------
  if (opcion_principal == "4") {
    if (is.null(datos)) {
      cat("\nERROR: Primero debe cargar o ingresar datos.\n")
      pausa()
    } else {
      menu_analisis(datos)
    }
  }
  # ----------------------------------------------------------
  # OPCION 5
  # ----------------------------------------------------------
  if (opcion_principal == "5") {
    if (is.null(datos)) {
      cat("\nNo hay datos cargados actualmente.\n")
    } else {
      cat("\n============================================\n")
      cat("             DATOS ACTUALES\n")
      cat("============================================\n")
      print(datos)
      cat("\nEstructura de los datos:\n")
      str(datos)
    }
    pausa()
  }
  # ----------------------------------------------------------
  # OPCION 6
  # ----------------------------------------------------------
  if (opcion_principal == "6") {
    if (is.null(datos)) {
      cat("\nNo existen datos para eliminar.\n")
    } else {
      respuesta <- tolower(
        trimws(
          readline(
            "¿Desea eliminar los datos actuales? (s/n): "
          )
        )
      )
      while (!(respuesta %in% c("s", "n"))) {
        cat("\nERROR: Escriba s o n.\n")
        respuesta <- tolower(
          trimws(
            readline("Respuesta (s/n): ")
          )
        )
      }
      if (respuesta == "s") {
        datos <- NULL
        cat("\nDatos eliminados correctamente.\n")
      }
    }
    pausa()
  }
  # ----------------------------------------------------------
  # SALIR
  # ----------------------------------------------------------
  if (opcion_principal == "0") {
    salir <- TRUE
    cat("\n====================================================\n")
    cat("       PROGRAMA FINALIZADO CORRECTAMENTE\n")
    cat("====================================================\n")
  }
}

