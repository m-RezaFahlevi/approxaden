#' Z-val Approximate by Using Simulated Annealing (as supposed)
#'
#' Finding z value for a given cumulative probability of standard normal distribution,
#' such that probability of Z < z is equal to 100(1 - alpha) confidence coefficient using
#' Metaheuristic, simulated annealing (as supposed).
#'
#' @param given_area a given cumulative probability for a given confidence coefficient
#' @param max_iteration maximum iteration, the more iteration it have, the more precise it get
#' @param is_echo return a data frame if it's TRUE, by default is_echo is FALSE
#' @importFrom stats pnorm runif
#' @examples
#' z_approximate <- zsimulated_annealing(0.975, 20)
#' zdframe_approximate <- zsimulated_annealing(0.975, 20, is_echo= TRUE)
#' z_approximate
#' zdframe_approximate
#' @export
zsimulated_annealing <- function(given_area, max_iteration, is_echo = FALSE) {
    li <- -10 # sentinel value for lower bound
    ui <- 10 # sentinel value for upperbound
    zvect <- c()
    areavect <- c()
    for (iteration in seq(1, max_iteration)) {
        zval_estimate <- runif(n = 1, min = li, max = ui)
        calculated_area <- pnorm(zval_estimate)
        zvect <- append(zvect, zval_estimate)
        areavect <- append(areavect, calculated_area)
        ifelse(calculated_area > given_area,
               ui <- zval_estimate, li <- zval_estimate)
    }
    dframe <- data.frame("iteration" = seq(1, max_iteration), "z_value" = zvect, "cumulative_probability" = areavect)
    ifelse(is_echo == TRUE,
           return(dframe), return(zvect[max_iteration]))
}
