# importing ggplot2 globally below as plan to use lots

#' residual vs time plot
#' @param df data
#' @param .time name of time column
#' @param .res name of residual column
#' @param .threshold absolute threshold for coloring outliers, defaults to 2.5
#' @param .ct show central tendency curve, default TRUE
#' @details
#' this function is primarily used for CWRES vs TAD
#' @examples \dontrun{
#'    df %>% gg_res_time(TAD, CWRES)
#' }
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom rlang !!
#' @importFrom magrittr %>%
#' @export
gg_res_time <- function(df, .time, .res, .threshold = 2.5, .ct = TRUE) {
  .time <- rlang::enexpr(.time)
  .res <- rlang::enexpr(.res)
  ggplot_output <- rlang::eval_tidy(
    rlang::quo(
      df %>%
        mutate(HIGHRES__ = factor(ifelse(abs(!!.res) > .threshold, 1, 0))) %>%
        ggplot(aes(x = !!.time, y = !!.res)) +
        geom_point(aes(color = HIGHRES__)) +
        scale_color_manual(values = c("black", "red"), name = "outliers",
                           labels = c("not outlier", "outlier")) +
        geom_smooth(aes(y = abs(!!.res)), se = F) +
        geom_smooth(aes(y = -abs(!!.res)), se = F) +
        theme_bw()
    ))
  if (!.ct) {
    return(ggplot_output)
  }

  return(ggplot_output +
           geom_smooth(se= F)
  )
}

