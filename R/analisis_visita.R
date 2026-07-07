#' Análisis descriptivo y gráfico de una visita concreta
#'
#' Localiza una visita en la base unificada a partir del identificador de la
#' vaca y la fecha de inicio, reconstruye su serie temporal de CH4 a partir del
#' sniffer (descartando los segundos solapados, igual que [unir_bases()]) y
#' devuelve un resumen descriptivo de la visita. Opcionalmente dibuja la serie
#' de CH4, sin marcar los picos, con superposición del suavizado, la línea de
#' fondo y/o el área bajo la curva.
#'
#' @param base Base unificada (salida de [unir_bases()]).
#' @param data_sniffer Sniffer ya limpio (salida de [limpiar_sniffer()]).
#' @param id_vaca Identificador de la vaca.
#' @param fecha Fecha (o fecha y hora) de inicio de la visita, como texto. Puede
#'   ser parcial: `"2021-03-17"` selecciona todas las visitas de ese día y
#'   `"2021-03-17 03:29"` una concreta.
#' @param n_visita Si `fecha` corresponde a varias visitas, índice (1, 2, …) de
#'   la que se desea. Por defecto `NULL`.
#' @param grafico Si es `TRUE`, devuelve además el gráfico de la serie. Por
#'   defecto `FALSE`.
#' @param mostrar_suavizado,mostrar_background,mostrar_auc Interruptores de los
#'   overlays del gráfico: señal suavizada, línea de fondo y área bajo la curva.
#' @param delay_seg,prob_background,min_altura,min_distancia,suavizado,truncar_neg_auc
#'   Parámetros de cálculo, con los mismos valores por defecto que [unir_bases()].
#'
#' @return Si `grafico = FALSE`, devuelve de forma invisible una lista con el
#'   descriptivo. Si `grafico = TRUE`, devuelve el objeto `ggplot`. En ambos
#'   casos imprime el descriptivo por consola.
#'
#' @importFrom rlang .data
#' @export
analisis_visita <- function(base, data_sniffer, id_vaca, fecha,
                            n_visita = NULL,
                            grafico = FALSE,
                            mostrar_suavizado = FALSE,
                            mostrar_background = TRUE,
                            mostrar_auc = FALSE,
                            delay_seg = 25,
                            prob_background = 0.20,
                            min_altura = 0.02,
                            min_distancia = 5,
                            suavizado = 7,
                            truncar_neg_auc = FALSE) {

  # --- localizar la visita ---
  fecha <- as.character(fecha)
  ini_fmt <- format(base$inicio_visita, "%Y-%m-%d %H:%M:%S")
  cand <- which(base$id_vaca == id_vaca & startsWith(ini_fmt, fecha))

  if (length(cand) == 0) {
    stop("No hay ninguna visita de la vaca ", id_vaca,
         " que empiece por '", fecha, "'.")
  }
  if (length(cand) > 1) {
    if (is.null(n_visita)) {
      lista <- paste0("  ", seq_along(cand), ": ", ini_fmt[cand],
                      collapse = "\n")
      stop("Hay ", length(cand), " visitas que coinciden:\n", lista,
           "\nAfina 'fecha' o usa 'n_visita' (1-", length(cand), ").")
    }
    if (n_visita < 1 || n_visita > length(cand)) {
      stop("'n_visita' debe estar entre 1 y ", length(cand), ".")
    }
    fila <- cand[n_visita]
  } else {
    fila <- cand
  }

  inicio_visita <- base$inicio_visita[fila]
  fin_analisis  <- base$fin_analisis[fila]

  # --- descartar segundos solapados (coherente con unir_bases) ---
  ini_num <- floor(as.numeric(base$inicio_visita) + delay_seg)
  fin_num <- floor(as.numeric(base$fin_analisis) + delay_seg)
  seg_vis <- mapply(function(a, b) seq.int(a, b), ini_num, fin_num,
                    SIMPLIFY = FALSE)
  conteo <- table(unlist(seg_vis))
  solapados <- as.numeric(names(conteo)[conteo > 1])
  sniffer_seg <- floor(as.numeric(data_sniffer$FechaHora))
  sniffer <- data_sniffer[!(sniffer_seg %in% solapados), ]

  # --- recuperar la serie de la visita ---
  ini_busq <- inicio_visita + delay_seg
  fin_busq <- fin_analisis + delay_seg
  sv <- sniffer[sniffer$FechaHora >= ini_busq & sniffer$FechaHora <= fin_busq, ]
  sv <- sv[order(sv$FechaHora), ]
  if (nrow(sv) == 0) {
    stop("No hay medidas de sniffer en la ventana de esta visita.")
  }

  # --- background, senal corregida y eje temporal ---
  ch4_validos <- sv$CH4[!is.na(sv$CH4)]
  background <- NA_real_
  if (length(ch4_validos) > 0) {
    corte <- stats::quantile(ch4_validos, probs = prob_background, na.rm = TRUE)
    background <- stats::median(ch4_validos[ch4_validos <= corte], na.rm = TRUE)
  }
  sv$bg       <- background
  sv$ch4_corr <- sv$CH4 - background
  sv$segundo  <- as.numeric(difftime(sv$FechaHora, min(sv$FechaHora),
                                     units = "secs"))

  # --- suavizado de la senal cruda (para el grafico) ---
  suaviza <- function(x) {
    if (suavizado <= 1) return(x)
    s <- as.numeric(stats::filter(x, rep(1 / suavizado, suavizado), sides = 2))
    s[is.na(s)] <- x[is.na(s)]
    s
  }
  sv$ch4_suave <- suaviza(sv$CH4)

  # --- numero de picos (sobre la senal corregida y suavizada) ---
  suave_corr <- suaviza(sv$ch4_corr)
  picos <- if (nrow(sv) < 5) NULL else
    pracma::findpeaks(suave_corr, minpeakheight = min_altura,
                      minpeakdistance = min_distancia)
  n_picos <- if (is.null(picos)) 0L else nrow(picos)

  # --- AUC (regla del trapecio sobre CH4 corregido) ---
  s <- sv$segundo; cc <- sv$ch4_corr
  ok <- !is.na(s) & !is.na(cc); s <- s[ok]; cc <- cc[ok]
  auc <- if (length(s) < 2) NA_real_ else {
    o <- order(s); s <- s[o]; cc <- cc[o]
    if (truncar_neg_auc) cc <- pmax(cc, 0)
    sum(diff(s) * (utils::head(cc, -1) + utils::tail(cc, -1)) / 2)
  }

  tiempo_efectivo <- as.numeric(difftime(fin_analisis, inicio_visita,
                                         units = "secs"))
  q <- function(x, p) stats::quantile(x, probs = p, na.rm = TRUE)

  desc <- list(
    id_vaca = id_vaca,
    inicio_visita = format(inicio_visita, "%Y-%m-%d %H:%M:%S"),
    n_medidas = nrow(sv), tiempo_efectivo = tiempo_efectivo,
    CH4_media = mean(sv$CH4, na.rm = TRUE), CH4_sd = stats::sd(sv$CH4, na.rm = TRUE),
    CH4_min = min(sv$CH4, na.rm = TRUE), CH4_max = max(sv$CH4, na.rm = TRUE),
    CH4_p25 = q(sv$CH4, .25), CH4_p50 = q(sv$CH4, .50), CH4_p75 = q(sv$CH4, .75),
    CO2_media = mean(sv$CO2, na.rm = TRUE), CO2_sd = stats::sd(sv$CO2, na.rm = TRUE),
    CO2_min = min(sv$CO2, na.rm = TRUE), CO2_max = max(sv$CO2, na.rm = TRUE),
    CO2_p25 = q(sv$CO2, .25), CO2_p50 = q(sv$CO2, .50), CO2_p75 = q(sv$CO2, .75),
    ratio_media = mean(sv$CH4, na.rm = TRUE) / mean(sv$CO2, na.rm = TRUE),
    background_CH4 = background, varianza_CH4 = stats::var(sv$CH4, na.rm = TRUE),
    auc_CH4 = auc, n_picos = n_picos,
    picos_por_minuto = n_picos / (tiempo_efectivo / 60)
  )

  # --- impresion legible ---
  cat(sprintf("Vaca %s  |  inicio %s\n", id_vaca, desc$inicio_visita))
  cat(sprintf("Medidas: %d   Duracion efectiva: %.0f s\n",
              desc$n_medidas, desc$tiempo_efectivo))
  cat(sprintf("CH4:  media = %.4f   sd = %.4f   min = %.4f   max = %.4f\n",
              desc$CH4_media, desc$CH4_sd, desc$CH4_min, desc$CH4_max))
  cat(sprintf("      p25 = %.4f   mediana = %.4f   p75 = %.4f\n",
              desc$CH4_p25, desc$CH4_p50, desc$CH4_p75))
  cat(sprintf("CO2:  media = %.4f   sd = %.4f   min = %.4f   max = %.4f\n",
              desc$CO2_media, desc$CO2_sd, desc$CO2_min, desc$CO2_max))
  cat(sprintf("      p25 = %.4f   mediana = %.4f   p75 = %.4f\n",
              desc$CO2_p25, desc$CO2_p50, desc$CO2_p75))
  cat(sprintf("Ratio CH4/CO2 (cociente de medias): %.4f\n", desc$ratio_media))
  cat(sprintf("Background CH4: %.4f   Varianza CH4: %.5f\n",
              desc$background_CH4, desc$varianza_CH4))
  cat(sprintf("AUC (CH4 corregido): %.2f\n", desc$auc_CH4))
  cat(sprintf("Nº de picos: %d   Picos/min: %.2f\n",
              desc$n_picos, desc$picos_por_minuto))

  if (!isTRUE(grafico)) return(invisible(desc))

  # --- grafico (CH4 crudo, sin marcar picos) ---
  g <- ggplot2::ggplot(sv, ggplot2::aes(x = .data$segundo, y = .data$CH4))
  if (mostrar_auc) {
    g <- g + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$bg, ymax = .data$CH4),
      fill = "grey70", alpha = 0.4)
  }
  g <- g + ggplot2::geom_line(color = "grey30")
  if (mostrar_suavizado) {
    g <- g + ggplot2::geom_line(ggplot2::aes(y = .data$ch4_suave),
                                color = "steelblue", linewidth = 1)
  }
  if (mostrar_background) {
    g <- g + ggplot2::geom_hline(yintercept = background,
                                 linetype = "dashed", color = "firebrick")
  }
  g + ggplot2::labs(
    title = paste0("Vaca ", id_vaca, " \u2014 ", desc$inicio_visita),
    subtitle = sprintf("AUC = %.1f  |  picos = %d  |  CH4 medio = %.3f",
                       desc$auc_CH4, desc$n_picos, desc$CH4_media),
    x = "Segundos", y = "CH4") +
    ggplot2::theme_minimal()
}
