
#' bayesian model of btts based on logit prior distirubtion (case = both teams to score (btts))
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param team_input team
#' @param var_input select whether team is play home ("hometeam") or away ("awayteam")
#' @param plot_content TRUE/FALSE to plot data
#' @param tester TRUE/FALSE whether data is used for testing (exclude last 30 days results)
#'
#' @return a tibble of team and median of samples distributions ("best guess")
#' @export
#'
sco_bayesian_btts <- function(year_input,league_input,var_input,team_input) {

  # ---------------------------------------------------------- #
  # input from acquire function
  sco_acquire(year_input, league_input) -> input

  # ---------------------------------------------------------- #
  # compute case data (case = btts)
  input %>%
    dplyr::filter(.[[grep(var_input,colnames(input))]] == team_input) %>%
    dplyr::mutate(case = dplyr::if_else(fthg > 0 & ftag > 0,1,0)) %>%
    dplyr::select(.[[grep(var_input,colnames(input))]],case) -> model_data

  # ---------------------------------------------------------- #
  # model inputs
  nrow(model_data) -> n_matches
  sum(model_data$case) -> n_cases # n cases of both teams score [data]
  seq(0,1,0.05) -> proportion_case # how large proportion ends with case [possible outcomes of prior]

  # ---------------------------------------------------------- #
  # create grid of parameters
  expand.grid(proportion_case = proportion_case, n_cases = n_cases) -> parm #
  dlogis(parm$proportion_case, location = .5, scale = .14) -> parm$prior # [prior]
  dbinom(parm$n_cases,size = n_matches, prob = parm$proportion_case) -> parm$likelihood # likelihood for each parameter combination

  # ---------------------------------------------------------- #
  # calculate and scale probability of parameters
  parm$probability <- parm$likelihood * parm$prior
  parm$probability <- parm$probability / sum(parm$probability)
  parm$prior_rescaled <- parm$prior / sum(parm$prior)

  # ---------------------------------------------------------- #
  # sample rows by row probability (bayes theorem sampling)
  samples <- sample(nrow(parm), size = 10000, replace = TRUE, prob = parm$probability)
  parm_samples <- parm[samples, c("proportion_case")]

  # ---------------------------------------------------------- #
  # plot data
  plot(parm$proportion_case, parm$probability, type = "h", xlab = "Proportion of cases", ylab = "p", main = paste0(team_input, " (",var_input,") \n cases: ",  n_cases,"/",n_matches), col = "darkgrey", xaxt = 'n')
    axis(side = 1, at = seq(0,1,0.1))
    lines(parm$proportion_case, parm$prior_rescaled, pch = 18, col = "red", type = "l", lty = 2,lwd = 2)
    abline(v = n_cases/n_matches, lwd = 2, col = "darkgreen")
    abline(v = median(parm_samples), lwd = 2, col = "blue")
    legend("topright", legend = c("est", "est median","prior","data"), col=c("darkgrey","blue", "red","darkgreen"), lty = c(1,1,2,1), cex = 0.8, lwd = c(1,2,2,2))

  # ---------------------------------------------------------- #
  # gather data in tibble as return
  return(dplyr::tibble(team = team_input,
         as = var_input,
         median_prop = median(parm_samples),
         sd_prop = sd(parm_samples)))
}