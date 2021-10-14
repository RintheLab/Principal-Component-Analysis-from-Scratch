### PCA 2D example ###

library(ggplot2)
library(dplyr)
library(ggpubr)

# Simulate data -----------------------------------------------------------

set.seed(1)

# Variable 1
var_1 <- rnorm(50, 50, sd = 3)

# Variable 2
var_2 <- .5*var_1 + rnorm(50, sd = sqrt(3))

# Both variables in a data frame
data_set_1 <- data.frame(var_1, var_2)

head(data_set_1)

# A scatter plot with the two simulated variables
ggplot(data_set_1, aes(x = var_1, y = var_2)) +
  geom_point(color = "blue", size = 2) +
  xlab("Variable 1") +
  ylab("Variable 2") +
  theme_classic()

# Center variables --------------------------------------------------------

# Substract the mean from each variable
data_set_1 <- data_set_1 %>% 
  mutate(varc_1 = var_1 - mean(var_1), varc_2 = var_2 - mean(var_2))

head(data_set_1)

# Scatter plot for the centered data 
ggplot(data_set_1, aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  theme_classic()


# Covariance matrix -------------------------------------------------------

# Select just the centered variables
data_set_2 <- data_set_1 %>% 
  select(varc_1, varc_2) %>% 
  as.matrix()

# Calculate the covariance matrix
cov_m <- (t(data_set_2) %*% data_set_2) / (nrow(data_set_2) - 1) 

cov_m

# Covariance matrix using cov()
cov(data_set_2)

# Covariance matrix using crossprod()
crossprod(data_set_2) / (nrow(data_set_2) - 1)

# Eigenvectors and eigenvalues --------------------------------------------

# Use eigen() to obtain eigenvectors and eigenvalues
cov_e <- eigen(cov_m)

# Eigenvectors
e_vec <- cov_e$vectors

# Eigenvalues
e_val <- cov_e$values

# First eigenvector 
ev_1 <- e_vec[,1]

# Slope of the first eigenvector
ev1_m <- ev_1[2] / ev_1[1]

# Second eigenvector 
ev_2 <- e_vec[,2]

# Slope of the second eigenvector
ev2_m <- ev_2[2] / ev_2[1]

# Scatter plot showing the span of both eigenvectors 
ggplot(data.frame(data_set_2), aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  geom_abline(slope = ev1_m, color = "blue", size = 0.7) +
  geom_abline(slope = ev2_m, color = "red", size = 0.7) +
  theme_classic()

# Multiply both eigenvectors 
ev_1 %*% ev_2

# Scree plot --------------------------------------------------------------

# Calculate the estimated variance for each eigenvalue
e_var <- e_val / (nrow(data_set_2) - 1)

# Data frame with variance percentages
var_per <- data.frame(
  PC  = c("PC1", "PC2"),
  PER = c(e_var) * 100 / sum(e_var) # Calculate the percentage
)

# Scree plot 
ggplot(var_per, aes(x = PC, y = PER)) +
  geom_col(width = 0.5, color = "black") +
  xlab("Principal component") +
  ylab("Percentage of variation (%)") +
  theme_classic()

# Loadings ----------------------------------------------------------------

# Norm of the first eigenvector
norm(as.matrix(ev_1), "F")

# Norm of the second eigenvector
norm(as.matrix(ev_2), "F")

# Data frame with both eigenvectors
loads <- data.frame(
  VAR   = c("var_1", "var_2"),
  PC1 = ev_1, # First eigenvecor
  PC2 = ev_2  # Second eigenvectors
)

loads

# Data in lower dimensions ------------------------------------------------

# Change the basis of the original data 
data_set_3 <- data_set_2 %*% solve(e_vec) # Inverse of eigenvectors matrix

# Scatter showing the rotation 
ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  theme_classic()

# Scatter plot with the centered data 
plot_data <- ggplot(data.frame(data_set_2), aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  ylim(c(-8, 8.5)) +
  ggtitle("Original Data") +
  theme_classic()

# Scatter plot with the rotated data
plot_rotation <- ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  ylim(c(-8, 8.5)) +
  ggtitle("Change of Basis to Eigenvectors") +
  theme_classic()

# Both graphs side by side
ggarrange(plot_data, plot_rotation)

# Data points just from PC 1
data_pc1 <- data.frame(v1 = data_set_3[,1], v2 = rep(0, nrow(data_set_3)))

# Scatter plot showing the projected points from PC1 (red points)
ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_point(data = data_pc1, aes(v1, v2), color = "red", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  ylim(c(-8, 8.5)) +
  theme_classic()
