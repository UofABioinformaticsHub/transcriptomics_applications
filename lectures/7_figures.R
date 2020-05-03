library(tidyverse)
set.seed(1)
n <- 20
x <- runif(n, 0, 10)
y <- 0 + x + rnorm(n)

here::here("lectures/figures/regPoints.pdf") %>%
  pdf(width = 8, height = 6)
tibble(x, y) %>%
  ggplot(aes(x, y)) +
  geom_segment(
    x = min(x),
    xend = max(x),
    y = min(x),
    yend = max(x),
    colour = "blue"
  ) +
  geom_point() +
  geom_text(
    x = 2, y = 1, label = expression(y == beta[0] + beta[1] * x),
    size = 6,
    colour = "blue"
  ) +
  xlim(c(0, 10)) + 
  ylim(c(0, 10))
dev.off()

# With residuals
here::here("lectures/figures/regPoints_resid.pdf") %>%
  pdf(width = 8, height = 6)
tibble(x, y) %>%
  ggplot(aes(x, y)) +
  geom_segment(
    x = min(x),
    xend = max(x),
    y = min(x),
    yend = max(x),
    colour = "blue"
  ) +
  geom_segment(
    aes(x = x, xend = x, y = x, yend = y),
    colour = "grey",
    linetype = 2
  ) +
  geom_point() +
  geom_text(
    x = 2, y = 1, label = expression(y == beta[0] + beta[1] * x),
    size = 6,
    colour = "blue"
  ) +
  xlim(c(0, 10)) + 
  ylim(c(0, 10))
dev.off()

# Residuals
here::here("lectures/figures/resid.pdf") %>%
  pdf(width = 8, height = 6)
tibble(x, y) %>%
  ggplot(aes(x, y - x)) +
  geom_segment(aes(x = x, y = y - x, xend = x, yend = 0), linetype = 2, colour = "grey50") +
  geom_point() +
  geom_hline(yintercept = 0, colour = "blue") +
  ylab(expression(paste(r[i] == y[i] - hat(y)[i])))
dev.off()

# Using data from the practical 7.2
here::here("lectures/figures/AvSD.pdf") %>%
  pdf(width = 8, height = 6)
tibble(A = fit_int$Amean, SD = fit_int$sigma) %>% 
  ggplot(aes(A, SD)) +
  geom_point()+ 
  geom_smooth(se = FALSE, method = "loess") +
  labs(x = "AveExpr", y = expression(paste("SD(", log[2], hat(S), ")")))
dev.off()
