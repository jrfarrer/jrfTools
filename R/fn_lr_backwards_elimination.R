#' Automated Backwards Elimination for Logistic Regression
#'
#' Performs backwards elimination on a logistic regression model
#'
#' @param lr_model logistic regression object created by glm
#'
#' @return
#' updated logistic regression object
#'
#' @examples
#'
#'
#' @export
fn_lr_backwards_elimination <- function(lr_model) {

    df <- car::Anova(lr_model) %>% broom::tidy()
    lr_formula1 <- lr_model$formula


    while (max(df$p.value) > 0.01) {

        to_remove <-
            df %>%
            dplyr::arrange(desc(p.value)) %>%
            head(1) %>%
            dplyr::select(term) %>%
            unlist()

        lr_formula1 <- update(lr_formula1,  as.formula(paste0("~ . -",to_remove)))
        lr_model <- glm(lr_formula1, data = lr_model$data, family = "binomial")

        df <- car::Anova(lr_model) %>% broom::tidy()

    }
    return(lr_model)
}
