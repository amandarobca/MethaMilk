#' Análisis descriptivo y gráfico de una vaca a lo largo de sus visitas
#'
#' Resume las variables de emisión de todas las visitas de una vaca concreta
#' (una fila por visita en la base unificada) y, opcionalmente, dibuja la
#' evolución temporal de una variable, su distribución o su relación con otra.
#'
#' @param base Base unificada (salida de [unir_bases()]).
#' @param id_vaca Identificador de la vaca.
#' @param grafico Tipo de gráfico: `"ninguno"`, `"evolucion"` (la variable
#'   visita a visita en el tiempo), `"histograma"` o `"dispersion"`. Por
#'   defecto `"ninguno"`.
#' @param variable Variable principal a representar. Por defecto `"media_CH4"`.
#' @param variable_x Variable del eje X en el gráfico de dispersión. Por defecto
#'   `"produccion"`.
#' @param bins Número de barras del histograma. Por defecto `15`.
#'
#' @return Si `grafico = "ninguno"`, devuelve de forma invisible la tabla
#'   resumen. En otro caso, devuelve el objeto `ggplot`. Siempre imprime el
#'   resumen por consola.
#'
#' @importFrom rlang .data
#' @export
analisis_vaca <- function(base, id_vaca,
                          grafico = c("ninguno", "evolucion",
                                      "histograma", "dispersion"),
                          variable = "media_CH4",
                          variable_x = "produccion",
                          bins = 15) {

  grafico <- match.arg(grafico)

  sub <- base[base$id_vaca == id_vaca, ]
  if (nrow(sub) == 0) {
    stop("No hay ninguna visita de la vaca ", id_vaca, " en la base.")
  }
  sub <- sub[order(sub$inicio_visita), ]

  q  <- function(x, p) stats::quantile(x, probs = p, na.rm = TRUE)
  cv <- function(x) {
    m <- mean(x, na.rm = TRUE)
    if (is.na(m) || m == 0) NA_real_ else stats::sd(x, na.rm = TRUE) / m * 100
  }

  vars <- c("produccion", "media_CH4", "media_CO2", "ratio_media",
            "auc_CH4", "varianza_CH4", "n_picos", "picos_por_minuto")
  vars <- vars[vars %in% names(sub)]

  resumen <- do.call(rbind, lapply(vars, function(v) {
    x <- sub[[v]]
    data.frame(
      variable = v, n = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
      cv_pct = cv(x), min = min(x, na.rm = TRUE),
      mediana = q(x, .50), max = max(x, na.rm = TRUE),
      row.names = NULL)
  }))

  rango <- range(sub$inicio_visita, na.rm = TRUE)
  cat(sprintf("Vaca %s: %d visitas entre %s y %s\n\n",
              id_vaca, nrow(sub),
              format(rango[1], "%Y-%m-%d"), format(rango[2], "%Y-%m-%d")))
  print(format(resumen, digits = 4))

  
  if (grafico == "ninguno") return(invisible(resumen))

  if (grafico == "evolucion") {
    g <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$inicio_visita,
                                           y = .data[[variable]])) +
      ggplot2::geom_line(color = "grey60") +
      ggplot2::geom_point(color = "steelblue", size = 2) +
      ggplot2::labs(title = paste0("Vaca ", id_vaca, ": evolucion de ", variable),
                    x = "Fecha de la visita", y = variable)
  } else if (grafico == "histograma") {
    g <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[variable]])) +
      ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "white") +
      ggplot2::labs(title = paste0("Vaca ", id_vaca, ": distribucion de ", variable),
                    x = variable, y = "Nº de visitas")
  } else {
    g <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[variable_x]],
                                           y = .data[[variable]])) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", se = TRUE) +
      ggplot2::labs(title = paste0("Vaca ", id_vaca, ": ", variable,
                                   " frente a ", variable_x),
                    x = variable_x, y = variable)
  }

  g + ggplot2::theme_minimal()
}
