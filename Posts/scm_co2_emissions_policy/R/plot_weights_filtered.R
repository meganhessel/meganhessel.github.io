plot_weights_filtered <- function(data, min_weight = 0.01) {
  grab_unit_weights(data, placebo = FALSE) %>%
    dplyr::mutate(type = "Control Unit Weights (W)") %>%
    dplyr::filter(weight >= min_weight) %>%
    dplyr::arrange(weight) %>%
    dplyr::mutate(unit = forcats::fct_reorder(unit, weight)) %>%
    ggplot2::ggplot(ggplot2::aes(unit, weight, fill = type, color = type)) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::facet_wrap(~type, ncol = 2, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = c("#b41e7c", "grey60")) +
    ggplot2::scale_color_manual(values = c("#b41e7c", "grey60")) +
    ggplot2::theme(text = ggplot2::element_text(size = 14))
}