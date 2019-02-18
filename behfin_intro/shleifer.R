# Model of investor sentiment
# Shleifer, 2000

lambda_1 <- 0.1
lambda_2 <- 0.3
pi_l <- 1/3
pi_h <- 3/4
N_0 <- 1000
y <- 50

roll_step <- function(trans_matrix, current_state) {
  new_state <- sample(1:2, 1, prob=trans_matrix[current_state, ])
  new_state
}

simulate_earnings <- function(n) {
  y_t <- matrix(0, nrow = n, ncol = 2)
  y_t[1, 1] <- model_active <- 1
  y_t[1, 2] <- ifelse(runif(1) < 0.5, y, -y)
  model1_matrix <- matrix(c(pi_l, 1 - pi_l, 1 - pi_l, pi_l), nrow = 2)
  model2_matrix <- matrix(c(pi_h, 1 - pi_h, 1 - pi_h, pi_h), nrow = 2)
  model_switch <- matrix(c(1 - lambda_1, lambda_1,
                           lambda_2, 1 - lambda_2), nrow = 2, byrow = T)
  for (i in 2:n) {
    current_state <- ifelse(y_t[i - 1, 2] < 0, 1, 2)
    if (model_active == 1) {
      roll <- roll_step(model1_matrix, current_state)
    } else {
      roll <- roll_step(model2_matrix, current_state)
    }
    y_t[i, 2] <- ifelse(roll == 1, y, -y)
    y_t[i, 1] <- model_active <- roll_step(model_switch, model_active)
  }
  y_t <- as.data.frame(y_t)
  colnames(y_t) <- c('Model', 'Earnings')
  y_t
}

plot_series <- function(vec) {
  df <- data.frame(x = 1:length(vec), y = vec)
  ggplot(df, aes(x, y)) +
    geom_line(size = 1.2) +
    labs(x = '', y = '') +
    theme_minimal()
}

set.seed(102)
# earn_hist <- simulate_earnings(100)
# N_t <- N_0 + cumsum(earn_hist$Earnings)
# plot_series(N_t)

n_stocks <- 2000
n_years <- 6
stock_universe <- t(replicate(
  n_stocks,
  c(N_0, N_0 + cumsum(simulate_earnings(n_years)$Earnings))))

r_diff <- numeric(5)
for (i in 1:5) {
  su <- stock_universe[, 1:(i + 1)]
  p_plus <- apply(su, 1, function(x) all(diff(x) > 0))
  p_minus <- apply(su, 1, function(x) all(diff(x) < 0))
  ret <- stock_universe[, i + 2] - stock_universe[, i + 1]
  r_diff[i] <- sum(ret[p_plus]) - sum(ret[p_minus])
}
plot_series(r_diff) + ylim(c(0, NA))
