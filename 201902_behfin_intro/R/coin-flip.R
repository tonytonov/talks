# Experiment results for Bayesian coin flipping experiment
# bit.ly/pgi-math

library(ggplot2)
library(stringr)
library(Hmisc)
library(reshape2)
library(extrafont)
loadfonts(device = "win")
options(stringsAsFactors = F)

outcomes <- c('', 'H', 'HH', 'HHH', 'HHHH', 'HHHHH', 'HHHHHT')
ff <- function(outcomes, p_heads = 0.7) {
    h <- str_count(outcomes, 'H')
    t <- str_count(outcomes, 'T')
    p_tails <- 1 - p_heads
    p_heads^h * p_tails^t / (p_heads^h * p_tails^t + p_heads^t * p_tails^h)
}

ggplot(data = data.frame(x = 0), mapping = aes(x = x, y = y)) +
    geom_point(data = data.frame(x = 0:6, y = ff(outcomes)), size = 3) +
    scale_x_continuous(breaks = 0:6) +
    labs(x = '', y = '') +
    theme_minimal()

df_coin <- read.csv('coin-flip-results.csv')[, -(1:2)]
colnames(df_coin) <- 1:6
nrow(df_coin[apply(df_coin, 1, function(x) any(x < 50)), ])
nrow(df_coin[apply(df_coin, 1, function(x) all(x == 50)), ])
nrow(df_coin[apply(df_coin, 1, function(x) any(x == 100)), ])

meltify <- function(df) {
    melt(t(df),
         measure.vars = 1:ncol(df),
         varnames = c('x', 'group'),
         value.name = 'y')
}

df_coin_melt <- meltify(df_coin)
gg <- ggplot(df_coin_melt, aes(x, y, group = group, color = '')) +
    geom_point(data = NULL) +
    geom_line(alpha = 0.2, size = 0.5, color = 'black') +
    geom_point(data = data.frame(x = 0:6, y = 100 * ff(outcomes), group = 0),
               size = 3, color = 'black') +
    labs(x = 'Coin flip',
         y = 'Probability estimate',
         title = 'A coin flipping experiment: survey results',
         subtitle = 'Error bars are 99% bootstrapped confidence intervals') +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(50, 100)) +
    scale_x_continuous(breaks = 0:6) +
    stat_summary(data = df_coin_melt, aes(group = NULL),
                 fun.data = 'mean_cl_boot',
                 fun.args = list(conf.int = 0.99),
                 geom = 'errorbar', color = '#0C5DA5', width = 0.15, size = 0.8) +
    stat_summary(data = meltify(df_coin[apply(df_coin, 1, function(x) all(x > 50) & any(x != 70)), ]), aes(group = NULL),
                 fun.data = 'mean_cl_boot',
                 fun.args = list(conf.int = 0.99),
                 geom = 'errorbar', color = '#FF9500', width = 0.15, size = 0.8) +
    scale_color_manual(name = NULL,
                       values = c('#0C5DA5', '#FF9500', 'black'),
                       limits = c('All responses', '\"Bayes-oriented\" responses', 'Fully rational Bayesian'),
                       guide = guide_legend(override.aes = list(
                           shape = c(124, 124, 19),
                           size = c(5, 5, 3)
                       ))) +
    theme_minimal() +
    theme(legend.position = 'bottom',
          text = element_text(size = 20, family = 'Palatino Linotype'))

ggsave(gg, filename = '../img/bayes-results.png', width = 9, height = 6)
