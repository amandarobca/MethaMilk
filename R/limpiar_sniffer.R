#' Cargar y limpiar los datos crudos del sniffer
#'
#' Lee un archivo de texto con los registros crudos del sensor sniffer,
#' normaliza los nombres de las columnas, convierte los tipos de datos y
#' calcula el cociente CH4/CO2 instantáneo. Las filas se devuelven ordenadas
#' cronológicamente por marca temporal.
#'
#' @param archivo Ruta al archivo de texto exportado por el sniffer (por
#'   ejemplo, `"CAMPON_102_Loggy_25sg_FD2.02.txt"`). Debe contener una
#'   cabecera y, al menos, tres columnas en el orden marca temporal, CH4 y CO2.
#' @param sep Carácter separador de campos. Por defecto `";"`.
#' @param formato_fecha Formato de fecha y hora pasado a [as.POSIXct()].
#'   Por defecto `"%Y-%m-%d %H:%M:%S"`.
#' @param verbose Si es `TRUE`, informa por consola del número de registros
#'   leídos y de los valores ausentes detectados en cada columna. Por defecto
#'   `FALSE`.
#'
#' @return Un `data.frame` con las columnas `FechaHora` (`POSIXct`),
#'   `CH4` (numérica), `CO2` (numérica) y `ratio_CH4_CO2`
#'   (numérica; `NA` cuando `CO2` no es positivo), ordenado de forma
#'   ascendente por `FechaHora`.
#'
#' @examples
#' \dontrun{
#' sniffer <- limpiar_sniffer("CAMPON_102_Loggy_25sg_FD2.02.txt")
#' head(sniffer)
#' }
#'
#' @export
limpiar_sniffer <- function(archivo,
                            sep = ";",
                            formato_fecha = "%Y-%m-%d %H:%M:%S",
                            verbose = FALSE) {

  if (!file.exists(archivo)) {
    stop("No se encuentra el archivo: ", archivo)
  }

  data_sniffer <- utils::read.table(
    archivo,
    header = TRUE,
    sep = sep,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (ncol(data_sniffer) < 3) {
    stop("El archivo se ha leido con ", ncol(data_sniffer),
         " columna(s); se esperaban al menos 3 (FechaHora, CH4, CO2). ",
         "Revisa el separador indicado en 'sep'.")
  }

  # Conservar y renombrar las tres primeras columnas
  data_sniffer <- data_sniffer[, 1:3]
  names(data_sniffer) <- c("FechaHora", "CH4", "CO2")

  # Conversion de tipos
  data_sniffer$FechaHora <- as.POSIXct(data_sniffer$FechaHora,
                                       format = formato_fecha)
  data_sniffer$CH4 <- as.numeric(data_sniffer$CH4)
  data_sniffer$CO2 <- as.numeric(data_sniffer$CO2)

  # Cociente CH4/CO2 instantaneo (NA si CO2 no es positivo)
  data_sniffer$ratio_CH4_CO2 <- ifelse(data_sniffer$CO2 > 0,
                                       data_sniffer$CH4 / data_sniffer$CO2,
                                       NA)

  # Ordenar cronologicamente
  data_sniffer <- data_sniffer[order(data_sniffer$FechaHora), ]
  rownames(data_sniffer) <- NULL

  if (verbose) {
    message("Registros leidos: ", nrow(data_sniffer))
    message("NA en FechaHora: ", sum(is.na(data_sniffer$FechaHora)))
    message("NA en CH4: ", sum(is.na(data_sniffer$CH4)))
    message("NA en CO2: ", sum(is.na(data_sniffer$CO2)))
    message("NA en ratio_CH4_CO2: ", sum(is.na(data_sniffer$ratio_CH4_CO2)))
  }

  data_sniffer
}
