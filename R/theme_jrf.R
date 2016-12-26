#' Fivethirtyeight theme for ggplot2
#'
#' Creates a ggplot2 theme function
#'
#' @param base_size base font size, defaults to 8
#' @param users_v character vector of users
#'
#' @return None
#'
#' @examples
#' ggplot(mtcars, aes(weight, mpg)) + geom_point() + theme_jrf()
#'
#' @export
theme_jrf <- function(base_size = 8, users_v = c()) {
    ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#F0F0F0", colour = "#606063"),
        panel.background = ggplot2::element_rect(fill = "#F0F0F0", colour = NA),
        panel.border = ggplot2::element_blank(),
        panel.grid.major =   ggplot2::element_line(colour = "#D7D7D8"),
        panel.grid.minor =   ggplot2::element_line(colour = "#D7D7D8", size = 0.25),
        panel.spacing =       unit(0.25, "lines"),
        panel.spacing.x =     NULL,
        panel.spacing.y =     NULL,
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(colour = "#A0A0A3"),
        axis.text.x = ggplot2::element_text(vjust = 1, colour = '#3C3C3C',
                                   family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica")),
        axis.text.y = ggplot2::element_text(hjust = 1, colour = '#3C3C3C',
                                   family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica")),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = 'bold', colour = '#3C3C3C', hjust = 0),
        text = ggplot2::element_text(size = 9, family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica")),
        title = ggplot2::element_text(family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica")),

        complete = TRUE

    )
}
