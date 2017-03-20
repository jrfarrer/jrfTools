#' Regsubsets Plots
#'
#' Creates plots of Cp, BIC, and R^2 from a regsubsets object
#'
#' @param regsubsets_obj object created by regsubsets object
#' @param elbow xintercept (# of predictors) to show an elbow
#' @param users_v character vector of users
#'
#' @return
#' ggplot object
#'
#' @examples
#' fn_regsubsets_plots(regsubsets_obj, users_v = "Jordan")
#'
#' @export
fn_regsubsets_plots <- function(regsubsets_obj, elbow = NULL, users_v = c()) {
    regsubsets_summary <- summary(regsubsets_obj)

    g <-
        dplyr::data_frame(
            predictors = 1:length(regsubsets_summary$cp)
            , cp = regsubsets_summary$cp
            , bic = regsubsets_summary$bic
            , adjr2 = regsubsets_summary$adjr2
        ) %>%
        tidyr::gather(metric, value, -predictors) %>%
        dplyr::mutate(metric = factor(metric, levels = c("cp","bic","adjr2"))) %>%
        ggplot(aes(x = predictors, y = value, colour = metric)) +
        facet_grid(metric ~ ., scale = "free_y", switch = "y",
                   labeller = ggplot2::labeller(metric = c(cp = "Cp", bic = "BIC", adjr2 = "Adjusted R^2"))) +
        geom_line() +
        geom_point() +
        geom_label(data = dplyr::data_frame(
            predictors = c(which.min(regsubsets_summary$cp), which.min(regsubsets_summary$bic),
                           which.max(regsubsets_summary$adjr2))
            , metric = factor(c("cp","bic","adjr2"), levels = c("cp","bic","adjr2"))
            , value = c(min(regsubsets_summary$cp), min(regsubsets_summary$bic), max(regsubsets_summary$adjr2))
            , label = paste0("Optimal\nd=", c(which.min(regsubsets_summary$cp), which.min(regsubsets_summary$bic),
                                              which.max(regsubsets_summary$adjr2)))
            , vjust = c(-.5, -.5, 1.25)
        ), aes(x = predictors, y = value, label = label, vjust = vjust),
                    family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica")) +
        theme_jrf(users_v = users_v) +
        labs(title = paste0(stringr::str_to_title(regsubsets_summary$obj$method), " Search"),
             x = "# of Predictors", y = NULL) +
        scale_colour_manual(guide = FALSE, values = c(pal538[['red']], pal538[['green']], pal538[['blue']]))

    if (!is.null(elbow)) {
        g <- g + geom_vline(xintercept = elbow, alpha = 0.5) +
            geom_label(data = data_frame(x = elbow, y = 300, metric = factor(c("cp"), levels = c("cp","bic","adjr2")),
                                         label = "Elbow with\n3 predictors"), aes(x=x,y=y,label=label), colour = "black", hjust = -.1,
                       family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica"))
    }

    print(g)
}
