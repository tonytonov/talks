# Nonlinear "bubble" model
# Sornette, Andersen, 2002

library(reshape2)
library(ggplot2)
library(ggthemes)

m <- 3
y_0 <- 1
sigma_0 <- sqrt(0.0003)
mu_0 <- 0.5
dt <- 3E-3
#t_c <- 1
B_0 <- 1

alpha <- 1 / (m - 1)
t_c <- y_0 / (m - 1) / mu_0

generate_bm <- function(n, mu = 0, sigma = 1) {
  dB <- mu * dt + sigma * sqrt(dt) * rnorm(n)
  cumsum(dB)  
}

generate_bubble <- function(n, bm = NULL, simplified = T) {
  if (is.null(bm)) bm <- generate_bm(n, mu_0, sigma_0)
  t <- (1:n) * dt
  while(any(t >= t_c)) t[t >= t_c] <- t[t >= t_c] - 1
  if (simplified) {
    B <- alpha^alpha / (1 - bm)^alpha
  } else {
    B <- alpha^alpha / (mu_0 * (t_c - t) - sigma_0 / B_0^m * bm)^alpha    
  }
  B
}

set.seed(102)
N <- 2500
df <- data.frame(index = 1:N, 
                 bm = generate_bm(N) / 3,
                 bubble = generate_bubble(N, bm))
ggplot(melt(df, id.vars = 'index'), 
       aes(index, value, color = variable)) + 
  geom_line(size = 1.05) + 
  labs(x = '', y = '', color = NULL) +
  scale_color_manual(values = c('black', 'red'), guide = F) +
  theme_minimal()