#' Cargar y limpiar los datos crudos del sistema de ordeño (AMS)
#'
#' Lee un archivo CSV con los registros de visitas exportados por el sistema
#' de ordeño automático (AMS), normaliza los nombres de las columnas, convierte
#' los tipos de datos y ordena las filas cronológicamente. La función admite
#' los dos formatos de exportación del AMS: con columna de evento
#' (ENTRADA/SALIDA, 8 columnas) y sin ella (7 columnas).
#'
#' @param archivo Ruta al archivo CSV exportado por el AMS (por ejemplo,
#'   `"Campon102_new.csv"`).
#' @param sep Carácter separador de campos. Por defecto `";"`.
#' @param formato_fecha Formato de fecha y hora pasado a [as.POSIXct()].
#'   Por defecto `"%d/%m/%Y %H:%M"`, que corresponde a marcas del tipo
#'   `"08/03/2021 0:07"`.
#' @param fileEncoding Codificación del archivo en disco. Por defecto `"latin1"`,
#'   que recupera correctamente los acentos de la columna `Descripcion` en las
#'   exportaciones originales. Para un archivo en UTF-8, usar `"UTF-8"`.
#' @param solo_entradas Si es `TRUE` y el archivo incluye la columna de evento,
#'   conserva únicamente las filas `"ENTRADA"`, de modo que quede una sola fila
#'   por visita. En archivos sin columna de evento (que ya traen una fila por
#'   visita) no tiene efecto. Por defecto `FALSE`.
#' @param verbose Si es `TRUE`, informa por consola del número de registros
#'   leídos, del formato detectado y de los valores ausentes en las columnas
#'   clave. Por defecto `FALSE`.
#'
#' @return Un `data.frame` con las columnas estandarizadas `id_vaca`,
#'   `id_robot`, `FechaHora` (`POSIXct`), `Tiempo_cubiculo`, `Produccion`,
#'   `Tiempo_ordeño` y `Descripcion`, ordenado de forma ascendente por
#'   `FechaHora`. Si el archivo incluye la columna de evento y `solo_entradas`
#'   es `FALSE`, se añade además `Evento` con los valores `"ENTRADA"`/`"SALIDA"`
#'   sin espacios sobrantes.
#'
#' @examples
#' \dontrun{
#' # Base con evento, conservando ambas filas por visita
#' sala <- limpiar_sala("Campon102_new.csv", verbose = TRUE)
#'
#' # Base con evento, dejando una sola fila por visita
#' sala_unica <- limpiar_sala("Campon102_new.csv", solo_entradas = TRUE)
#' }
#'
#' @export
limpiar_sala <- function(archivo,
                         sep = ";",
                         formato_fecha = "%d/%m/%Y %H:%M",
                         fileEncoding = "latin1",
                         solo_entradas = FALSE,
                         verbose = FALSE) {

  if (!file.exists(archivo)) {
    stop("No se encuentra el archivo: ", archivo)
  }

  data_sala <- utils::read.csv(
    archivo,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = fileEncoding
  )

  # Detectar el formato segun el numero de columnas y renombrar
  n_col <- ncol(data_sala)
  if (n_col == 8) {
    names(data_sala) <- c("Evento", "id_vaca", "id_robot", "FechaHora",
                          "Tiempo_cubiculo", "Produccion", "Tiempo_ordeño",
                          "Descripcion")
    tiene_evento <- TRUE
  } else if (n_col == 7) {
    names(data_sala) <- c("id_vaca", "id_robot", "FechaHora",
                          "Tiempo_cubiculo", "Produccion", "Tiempo_ordeño",
                          "Descripcion")
    tiene_evento <- FALSE
  } else {
    stop("El archivo se ha leido con ", n_col, " columna(s); se esperaban 7 ",
         "(sin evento) u 8 (con evento ENTRADA/SALIDA). ",
         "Revisa el separador indicado en 'sep'.")
  }

  # Limpiar espacios sobrantes en la columna de evento, si existe
  if (tiene_evento) {
    data_sala$Evento <- trimws(data_sala$Evento)
  }

  # --- NUEVO: deduplicacion opcional quedandose con las ENTRADA ---
  n_antes <- nrow(data_sala)
  if (solo_entradas) {
    if (tiene_evento) {
      data_sala <- data_sala[data_sala$Evento == "ENTRADA", ]
      # La columna Evento queda constante; se elimina por redundante
      data_sala$Evento <- NULL
    } else if (verbose) {
      message("solo_entradas = TRUE ignorado: el archivo no tiene columna de ",
              "evento (ya hay una fila por visita).")
    }
  }

  # Conversion de tipos
  data_sala$id_vaca   <- as.integer(data_sala$id_vaca)
  data_sala$id_robot  <- as.integer(data_sala$id_robot)
  data_sala$Produccion <- as.numeric(data_sala$Produccion)
  data_sala$FechaHora <- as.POSIXct(data_sala$FechaHora,
                                    format = formato_fecha)

  # Aviso si la conversion de fecha ha fallado por completo
  if (all(is.na(data_sala$FechaHora))) {
    warning("Todas las fechas resultaron NA: revisa 'formato_fecha' (",
            formato_fecha, ").")
  }

  # Ordenar cronologicamente
  data_sala <- data_sala[order(data_sala$FechaHora), ]
  rownames(data_sala) <- NULL

  if (verbose) {
    message("Registros leidos: ", n_antes)
    message("Formato detectado: ", if (tiene_evento) "8 columnas (con evento)"
            else "7 columnas (sin evento)")
    if (solo_entradas && tiene_evento) {
      message("Tras conservar solo ENTRADA: ", nrow(data_sala), " registros")
    }
    message("NA en id_vaca: ", sum(is.na(data_sala$id_vaca)))
    message("NA en id_robot: ", sum(is.na(data_sala$id_robot)))
    message("NA en FechaHora: ", sum(is.na(data_sala$FechaHora)))
    message("NA en Produccion: ", sum(is.na(data_sala$Produccion)))
  }

  data_sala
}
