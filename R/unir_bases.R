#' Unificar los datos del AMS y del sniffer en una base por visita
#'
#' Cruza los registros de visitas del sistema de ordeño (AMS) ya limpios con
#' los registros del sniffer ya limpios, define la ventana efectiva de análisis
#' de cada visita, descarta los segundos cubiertos por más de una visita y
#' resume, por visita, las concentraciones de gases, el área bajo la curva de
#' CH4 corregida por el nivel de fondo, la varianza de CH4 y el número de picos
#' de eructación. Todo el proceso se realiza en una sola función.
#'
#' @param data_sala Data frame del AMS ya limpio (salida de [limpiar_sala()]),
#'   con `FechaHora` en formato `POSIXct`. Si conserva la columna `Evento`, se
#'   filtran automáticamente las filas `"ENTRADA"`.
#' @param data_sniffer Data frame del sniffer ya limpio (salida de
#'   [limpiar_sniffer()]).
#' @param delay_seg Retardo de detección, en segundos, aplicado tanto a la
#'   detección de solapes como a la búsqueda de medidas del sniffer. Por
#'   defecto `25`.
#' @param dur_min,dur_max Límites inferior y superior, en segundos, para
#'   conservar una visita según su duración. Por defecto `120` y `600`.
#' @param ventana_seg Duración máxima, en segundos, de la ventana efectiva de
#'   análisis a partir del inicio de la visita. Por defecto `300`.
#' @param prob_background Cuantil empleado para estimar el nivel de fondo
#'   (background) de CH4. Por defecto `0.20`.
#' @param min_altura,min_distancia,suavizado Parámetros de la detección de
#'   picos: altura mínima del pico, distancia mínima entre picos (en medidas) y
#'   tamaño de la ventana de suavizado (en medidas). Por defecto `0.02`, `5` y
#'   `7`, los valores óptimos obtenidos en la validación.
#' @param truncar_neg_auc Si es `TRUE`, los valores negativos de CH4 corregido
#'   se truncan a cero antes de integrar el área bajo la curva. Por defecto
#'   `FALSE` (se conservan los valores negativos).
#' @param verbose Si es `TRUE`, informa por consola del número de visitas y
#'   medidas en cada paso. Por defecto `FALSE`.
#'
#' @return Un `data.frame` con una fila por visita y las columnas `id_vaca`,
#'   `inicio_visita`, `fin_analisis`, `tiempo_efectivo`, `produccion`,
#'   `media_CH4`, `media_CO2`, `ratio_media`, `auc_CH4`, `varianza_CH4`,
#'   `n_picos`, `picos_por_minuto` y `n_medidas`.
#'
#' @examples
#' \dontrun{
#' sala    <- limpiar_sala("Campon102_new.csv")
#' sniffer <- limpiar_sniffer("CAMPON_102_Loggy_25sg_FD2.02.txt")
#' base    <- unir_bases(sala, sniffer, delay_seg = 25, verbose = TRUE)
#' }
#'
#' @export
unir_bases <- function(data_sala, data_sniffer,
                       delay_seg = 25,
                       dur_min = 120, dur_max = 600,
                       ventana_seg = 300,
                       prob_background = 0.20,
                       min_altura = 0.02, min_distancia = 5, suavizado = 7,
                       truncar_neg_auc = FALSE,
                       verbose = FALSE) {

  
  tiempo_a_segundos <- function(x) {
    partes <- strsplit(as.character(x), ":")
    vapply(partes, function(p) {
      p <- as.numeric(p)
      if (length(p) == 2) p[1] * 60 + p[2]
      else if (length(p) == 3) p[1] * 3600 + p[2] * 60 + p[3]
      else NA_real_
    }, numeric(1))
  }

 
  contar_picos <- function(ch4_corr) {
    if (length(ch4_corr) < 5) return(0L)
    if (suavizado > 1) {
      suave <- as.numeric(stats::filter(ch4_corr, rep(1 / suavizado, suavizado),
                                        sides = 2))
      na_idx <- is.na(suave)
      suave[na_idx] <- ch4_corr[na_idx]
    } else {
      suave <- ch4_corr
    }
    picos <- pracma::findpeaks(suave,
                               minpeakheight = min_altura,
                               minpeakdistance = min_distancia)
    if (is.null(picos)) 0L else nrow(picos)
  }

  
  calcular_auc <- function(segundo, ch4_corr) {
    ok <- !is.na(segundo) & !is.na(ch4_corr)
    segundo <- segundo[ok]; ch4_corr <- ch4_corr[ok]
    if (length(segundo) < 2) return(NA_real_)
    o <- order(segundo); segundo <- segundo[o]; ch4_corr <- ch4_corr[o]
    if (truncar_neg_auc) ch4_corr <- pmax(ch4_corr, 0)
    sum(diff(segundo) *
          (utils::head(ch4_corr, -1) + utils::tail(ch4_corr, -1)) / 2)
  }

  
  metricas_una_visita <- function(inicio_visita, fin_analisis, sniffer) {
    inicio_busqueda <- inicio_visita + delay_seg
    fin_busqueda    <- fin_analisis + delay_seg

    salida <- data.frame(
      media_CH4      = NA_real_,
      media_CO2      = NA_real_,
      ratio_media    = NA_real_,
      background_CH4 = NA_real_,
      auc_CH4        = NA_real_,
      varianza_CH4   = NA_real_,
      n_picos        = 0L,
      n_medidas      = 0L
    )

    sv <- sniffer[sniffer$FechaHora >= inicio_busqueda &
                    sniffer$FechaHora <= fin_busqueda, ]
    if (nrow(sv) == 0) return(salida)
    sv <- sv[order(sv$FechaHora), ]

    
    ch4_validos <- sv$CH4[!is.na(sv$CH4)]
    background <- NA_real_
    if (length(ch4_validos) > 0) {
      corte <- stats::quantile(ch4_validos, probs = prob_background,
                               na.rm = TRUE)
      background <- stats::median(ch4_validos[ch4_validos <= corte],
                                  na.rm = TRUE)
    }

   
    ch4_corr <- sv$CH4 - background
    segundo  <- as.numeric(difftime(sv$FechaHora, min(sv$FechaHora),
                                    units = "secs"))

    salida$media_CH4      <- mean(sv$CH4, na.rm = TRUE)
    salida$media_CO2      <- mean(sv$CO2, na.rm = TRUE)
    salida$ratio_media    <- salida$media_CH4 / salida$media_CO2
    salida$background_CH4 <- background
    salida$varianza_CH4   <- stats::var(sv$CH4, na.rm = TRUE)
    salida$auc_CH4        <- calcular_auc(segundo, ch4_corr)
    salida$n_picos        <- contar_picos(ch4_corr)
    salida$n_medidas      <- nrow(sv)

    salida
  }

  
  if ("Evento" %in% names(data_sala)) {
    data_sala <- data_sala[trimws(data_sala$Evento) == "ENTRADA", ]
  }

  
  tiempo_seg <- tiempo_a_segundos(data_sala$Tiempo_cubiculo)

  
  data_visitas <- data.frame(
    id_vaca         = data_sala$id_vaca,
    id_robot        = data_sala$id_robot,
    inicio_visita   = data_sala$FechaHora,
    fin_visita      = data_sala$FechaHora + tiempo_seg,
    tiempo_cubiculo = data_sala$Tiempo_cubiculo,
    produccion      = data_sala$Produccion,
    stringsAsFactors = FALSE
  )

  
  data_visitas$duracion_visita <- as.numeric(
    difftime(data_visitas$fin_visita, data_visitas$inicio_visita,
             units = "secs"))
  n_total <- nrow(data_visitas)
  mask <- !is.na(data_visitas$duracion_visita) &
    data_visitas$duracion_visita >= dur_min &
    data_visitas$duracion_visita <= dur_max
  data_visitas <- data_visitas[mask, ]

  
  data_visitas$fin_analisis <- pmin(
    data_visitas$fin_visita,
    data_visitas$inicio_visita + ventana_seg)
  data_visitas$tiempo_efectivo <- as.numeric(
    difftime(data_visitas$fin_analisis, data_visitas$inicio_visita,
             units = "secs"))

  
  ini_num <- floor(as.numeric(data_visitas$inicio_visita) + delay_seg)
  fin_num <- floor(as.numeric(data_visitas$fin_analisis) + delay_seg)
  segundos_visita <- mapply(function(a, b) seq.int(a, b),
                            ini_num, fin_num, SIMPLIFY = FALSE)
  conteo <- table(unlist(segundos_visita))
  segundos_solapados <- as.numeric(names(conteo)[conteo > 1])

 
  sniffer_seg <- floor(as.numeric(data_sniffer$FechaHora))
  data_sniffer_sin_solape <- data_sniffer[
    !(sniffer_seg %in% segundos_solapados), ]

  
  resultados <- lapply(seq_len(nrow(data_visitas)), function(i) {
    metricas_una_visita(
      inicio_visita = data_visitas$inicio_visita[i],
      fin_analisis  = data_visitas$fin_analisis[i],
      sniffer       = data_sniffer_sin_solape)
  })
  resultados_df <- do.call(rbind, resultados)
  data_final <- cbind(data_visitas, resultados_df)

  
  data_final$picos_por_minuto <- data_final$n_picos /
    (data_final$tiempo_efectivo / 60)

  base_unificada <- data_final[, c(
    "id_vaca", "inicio_visita", "fin_analisis", "tiempo_efectivo",
    "produccion", "media_CH4", "media_CO2", "ratio_media",
    "auc_CH4", "varianza_CH4", "n_picos", "picos_por_minuto", "n_medidas")]

  if (verbose) {
    message("Visitas totales: ", n_total)
    message("Visitas tras filtrar por duracion [", dur_min, "-", dur_max,
            " s]: ", nrow(data_visitas),
            " (", round(100 * nrow(data_visitas) / n_total, 1), "%)")
    message("Segundos solapados eliminados: ", length(segundos_solapados))
    message("Medidas de sniffer conservadas: ",
            nrow(data_sniffer_sin_solape), " de ", nrow(data_sniffer))
    message("Picos detectados (total): ", sum(data_final$n_picos))
  }

  base_unificada
}
