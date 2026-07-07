#' Análisis descriptivo y gráfico del rebaño completo
#'
#' Resume las variables de emisión de toda la base unificada (una fila por
#' visita) y, opcionalmente, dibuja la distribución de una variable, su relación
#' con otra o su distribución por vaca.
#'
#' @param base Base unificada (salida de [unir_bases()]).
#' @param grafico Tipo de gráfico: `"ninguno"`, `"histograma"`, `"dispersion"`
#'   o `"boxplot_vaca"`. Por defecto `"ninguno"`.
#' @param variable Variable principal a representar. Por defecto `"media_CH4"`.
#' @param variable_x Variable del eje X en el gráfico de dispersión. Por defecto
#'   `"produccion"`.
#' @param min_visitas En `"boxplot_vaca"`, número mínimo de visitas para incluir
#'   a una vaca. Por defecto `1`.
#' @param bins Número de barras del histograma. Por defecto `30`.
#'
#' @return Si `grafico = "ninguno"`, devuelve de forma invisible la tabla
#'   resumen. En otro caso, devuelve el objeto `ggplot`. Siempre imprime el
#'   resumen por consola.
#'
#' @importFrom rlang .data
#' @export
analisis_rebano <- function(base,
                            grafico = c("ninguno", "histograma",
                                        "dispersion", "boxplot_vaca"),
                            variable = "media_CH4",
                            variable_x = "produccion",
                            min_visitas = 1,
                            bins = 30) {

  grafico <- match.arg(grafico)
  q <- function(x, p) stats::quantile(x, probs = p, na.rm = TRUE)

  vars <- c("produccion", "media_CH4", "media_CO2", "ratio_media",
            "auc_CH4", "varianza_CH4", "n_picos", "picos_por_minuto")
  vars <- vars[vars %in% names(base)]

  resumen <- do.call(rbind, lapply(vars, function(v) {
    x <- base[[v]]
    data.frame(
      variable = v, n = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE), p25 = q(x, .25),
      mediana = q(x, .50), p75 = q(x, .75), max = max(x, na.rm = TRUE),
      row.names = NULL)
  }))

  cat(sprintf("Rebano: %d visitas de %d vacas\n\n",
              nrow(base), length(unique(base$id_vaca))))
  print(format(resumen, digits = 4))

  if (grafico == "ninguno") return(invisible(resumen))

  if (grafico == "histograma") {
    g <- ggplot2::ggplot(base, ggplot2::aes(x = .data[[variable]])) +
      ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "white") +
      ggplot2::labs(title = paste("Distribucion de", variable),
                    x = variable, y = "Nº de visitas")
  } else if (grafico == "dispersion") {
    g <- ggplot2::ggplot(base, ggplot2::aes(x = .data[[variable_x]],
                                            y = .data[[variable]])) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "lm", se = TRUE) +
      ggplot2::labs(title = paste(variable, "frente a", variable_x),
                    x = variable_x, y = variable)
  } else {
    cuenta <- table(base$id_vaca)
    vacas_ok <- as.integer(names(cuenta)[cuenta >= min_visitas])
    sub <- base[base$id_vaca %in% vacas_ok, ]
    g <- ggplot2::ggplot(sub, ggplot2::aes(x = factor(.data$id_vaca),
                                           y = .data[[variable]])) +
      ggplot2::geom_boxplot(outlier.size = 0.6) +
      ggplot2::labs(
        title = paste("Distribucion de", variable, "por vaca"),
        subtitle = if (min_visitas > 1)
          paste("Vacas con >=", min_visitas, "visitas") else NULL,
        x = "Vaca", y = variable) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5))
  }

  g + ggplot2::theme_minimal()
}
