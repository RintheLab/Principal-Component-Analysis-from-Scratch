### PCA Wine Data ###

library(purrr)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Data --------------------------------------------------------------------

# Variable names
var_names <- read.csv("data/Label_Pred_values_IR.csv")
var_names <- names(var_names)

# Wine labels
wine_label <- read.csv("data/Label_Wine_samples.csv", header = FALSE)
wine_label <- unname(unlist(wine_label))

# Wine data set
data_set_wine <- read.csv(
  "data/Pred_values.csv", header = FALSE,
  row.names = wine_label, col.names = var_names
)

head(data_set_wine)

# Center and divude by standard deviation ---------------------------------

# Means for each variable
var_means <- unlist(map(data_set_wine, mean))

# Standard deviation for each variable
var_sd <- unlist(map(data_set_wine, sd))

# Center each variable
data_set_wine_2 <- map2(
  data_set_wine, var_means, .f = function(x, mean) x -mean
)

# Devide by the standard deviation of each variable
data_set_wine_2 <- map2(
  data_set_wine_2, var_sd, .f = function(x, sd) x / sd
)

# Make a matrix from the previous list
data_set_wine_2 <- as.matrix(data.frame(data_set_wine_2))

head(data_set_wine_2)

# Covariance matrix -------------------------------------------------------

# Calculate the covariance matrix
cov_wine <- (t(data_set_wine_2) %*% data_set_wine_2) / 
  (nrow(data_set_wine_2) - 1)

cov_wine[1:5, 1:5]

# Eigenvectors and eigenvalues --------------------------------------------

# eigen() to obtain eigenvalues and eigenvectors
eg_wine <- eigen(cov_wine)

# Eigenvalues
eg_vals <- eg_wine$values

# Eigenvectors
eg_vecs <- eg_wine$vectors

# Number of eigenvalues
length(eg_vals)

# Number of eigenvectors
ncol(eg_vecs)

# Scree plot --------------------------------------------------------------

# Calculate variances from each eigenvalue
eg_vars <- eg_vals / (nrow(data_set_wine_2) - 1)

# Data frame with variance percenatges
vars_perc <- data.frame(
  PC  = unlist(map(1:14, function(x) paste0("PC", x))),
  PER = round((eg_vars * 100) / sum(eg_vars), 4)
)

# Scree plot
ggplot(
  vars_perc, 
  aes(x = reorder(PC, order(PER, decreasing = TRUE)), y = PER)
) +
  geom_col(width = 0.5, color = "black") +
  xlab("Principal component") +
  ylab("Percentage of variation (%)") +
  theme_classic()

# Data in lower dimensions ------------------------------------------------

# Change the basis of the data
data_set_wine_eb <- data_set_wine_2 %*% solve(eg_vecs)

# Transfrom to a data frame
data_set_wine_eb <- data.frame(data_set_wine_eb)
colnames(data_set_wine_eb) <- vars_perc$PC

# Add a column with the origin of each wine sample
data_set_wine_eb <- data_set_wine_eb %>% 
  mutate(
    WineSample = unlist(map(wine_label, function(x) substr(x, 1, 3)))
  ) %>% 
  relocate(WineSample)

head(data_set_wine_eb)

# Scatter plot for PC1 and PC2
pc12 <- ggplot(
  data_set_wine_eb, 
  aes(PC1, PC2, color = WineSample, shape = WineSample)
) +
  geom_point(size = 3) +
  ggtitle("PC1 and PC2") +
  xlab("PC1 (24.4%)") +
  ylab("PC2 (21.3%)") +
  theme_classic() +
  theme(legend.position = "none") 

# Scatter plot for PC3 and PC4
pc34 <- ggplot(
  data_set_wine_eb, 
  aes(PC3, PC4, color = WineSample, shape = WineSample)
) +
  geom_point(size = 3) +
  scale_color_discrete(
    name = "Wine origin", 
    labels = c("Argentina", "Australia", "Chile", "South Africa")
  ) +
  scale_shape_discrete(
    name = "Wine origin", 
    labels = c("Argentina", "Australia", "Chile", "South Africa")
  ) +
  ggtitle("PC3 and PC4") +
  xlab("PC3 (17.5%)") +
  ylab("PC4 (10.0%)") +
  theme_classic()

# Both graphs side by side
ggarrange(pc12, pc34)

# Loading scores ----------------------------------------------------------

# Data frame with loading scores
loads_wine <- data.frame(eg_vecs)
colnames(loads_wine) <- vars_perc$PC
rownames(loads_wine) <- var_names

head(loads_wine)

# Scatter plot with loadings of PC1 and PC2
ld_pc12 <- ggplot(loads_wine, aes(PC1, PC2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = rownames(loads_wine)), hjust = -.2) +
  ggtitle("Loadings for PC1 and PC2") +
  xlim(c(-.7, .7)) +
  ylim(c(-.7, .7)) +
  xlab("PC1 (24.4%)") +
  ylab("PC2 (21.3%)") +
  theme_classic()

# Scatter plot with loadings of PC3 and PC4
ld_pc34 <- ggplot(loads_wine, aes(PC3, PC4)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = rownames(loads_wine)), hjust = -.2) +
  ggtitle("Loadings for PC3 and PC4") +
  xlim(c(-.7, .7)) +
  ylim(c(-.7, .7)) +
  xlab("PC3 (17.5%)") +
  ylab("PC4 (10.0%)") +
  theme_classic()

# Both graphs side by side
ggarrange(ld_pc12, ld_pc34)  