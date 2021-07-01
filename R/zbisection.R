#' Z-val Approximate by Using Bisection Method
#'
#' Finding z value for a given cumulative probability of standard normal distribution,
#' such that probability of Z < z is equal to (1 - alpha) confidence coefficient
#' using numerical analysis, bisection method.
#'
#' @param given_area a given cumulative probability for a given confidence coefficient
#' @param max_iteration maximum iteration, the more iteration it have, the more precise it is
#' @param li lower limit point estimate for z value (a.k.a. lower bound)
#' @param ui upper limit point estimate for z value (a.k.a. upper bound)
#' @param is_echo return a vector of estimate point for z value if it's TRUE, by default is_echo is FALSE
#' @details The probability of z can be calculated by using pnorm(z), when iteration is finish, for
#' z value, given_area ~ pnorm(z)
#' @examples
#' z_approximate <- znormbisection(0.975, 20, 0, 4)
#' zvect_approximate <- znormbisection(0.975, 20, -4, 4, is_echo= TRUE)
#' z_approximate
#' zvect_approximate
#' @export
znormbisection <- function(given_area, max_iteration, li, ui, is_echo = FALSE) {
    zvect <- c()
    for (iteration in seq(1, max_iteration)) {
        zval_estimate <- (li + ui) / 2
        calculated_area <- pnorm(zval_estimate)
        zvect <- append(zvect, zval_estimate)
        ifelse(calculated_area > given_area,
               ui <- zval_estimate, li <- zval_estimate)
    }
    ifelse(is_echo == TRUE,
           zvect, zvect <- zvect[max_iteration])
    return(zvect)
}
