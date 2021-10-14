library(tidyverse)

# Data set ----------------------------------------------------------------
set.seed(1)
var_1 <- rnorm(50, 50, sd = 3)
var_2 <- .5*var_1 + rnorm(50, sd = sqrt(3))

data_set <- tibble(var_1, var_2)

# Scatter plot with Diameter and Volume -----------------------------------

sca <- data_set %>%
  ggplot(aes(var_1, var_2)) +
  geom_point(color = "blue", size = 2) +
  theme_classic()

sca

# Data subset -------------------------------------------------------------

data_set2 <- data_set %>% 
  mutate(var_1 = var_1 - mean(var_1), var_2 = var_2 - mean(var_2))

sca_2 <- data_set2 %>% 
  ggplot(aes(var_1, var_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  theme_classic()

sca_2

# Covariance matrix --------------------------------------------------------

cov_m <- cov(data_set2)

t(data_set2) %*% as.matrix(data_set2) / (nrow(data_set2) - 1) 

# Eigenvalues and eigenvectors from covariance matrix ----------------------

es <- eigen(cov_m)

ev <- es$vectors

ev_1 <- ev[, 1]
ev1_m <- ev_1[2]/ev_1[1]

ev_2 <- ev[, 2]
ev2_m <- ev_2[2]/ev_2[1]

sca_3 <- sca_2 +
  geom_abline(slope = ev1_m, color = "red") +
  geom_abline(slope = ev2_m, color = "blue")

sca_3

# Transform the data -------------------------------------------------------

scores <- as.matrix(data_set2) %*% ev
scores <- data.frame(scores)

sca_3 +
  geom_point(data = scores, aes(X1, X1), color = "green", size = 2)


