#' Plot of cv.glmnet objects
#'
#' Creates a styled plot of cv.glmnet objects
#'
#' @param cv_glmnet cv.glmnet object
#' @param main title of the plot
#' @param users_v character vector of users if fonts installed
#'
#' @return None
#'
#' @examples
#' fn_plot_cv_glmnet(cv_glmnet, main = "LASSO")
#'
#' @export
fn_plot_cv_glmnet <- function(cv_glmnet, main, users_v = c()) {

    data <-
        broom::tidy(cv_glmnet) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(log_lambda = log(lambda))

    data2 <-
        data %>%
        dplyr::filter(dplyr::row_number(nzero) %% 4 == 0)

    data3 <-
        dplyr::data_frame(
            log_lambda = c(log(cv_glmnet$lambda.min), log(cv_glmnet$lambda.1se))
            , name = c("Min", "1se")
        )

    ggplot() +
        geom_errorbar(data = data, aes(x = log_lambda, ymin = conf.low, ymax = conf.high),
                      colour = pal538['dkgray'][[1]], alpha = 0.6) +
        geom_point(data = data, aes(x = log_lambda, y = estimate), colour = pal538['red'][[1]]) +
        geom_vline(xintercept = log(cv_glmnet$lambda.min), colour = pal538['dkgray'][[1]], alpha = 0.6) +
        geom_vline(xintercept = log(cv_glmnet$lambda.1se), colour = pal538['dkgray'][[1]], alpha = 0.6) +
        theme_jrf(users_v = users_v) +
        labs(title = main, x = expression(log(lambda)), y = cv_glmnet$name) +
        geom_text(data = data2, aes(x = log_lambda, y = Inf, label = nzero), vjust = 1, colour = '#3C3C3C',
                  family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica"),
                  size = 2.25) +
        geom_label(data = data3, aes(x = log_lambda, y = Inf, label = name), vjust = 5, colour = '#3C3C3C',
                   family = ifelse(Sys.info()[['user']] %in% users_v,"DecimaMonoPro", "Helvetica"))
}
