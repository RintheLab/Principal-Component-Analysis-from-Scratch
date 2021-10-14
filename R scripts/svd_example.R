A <- cbind(c(2, 1, 0, 0), c(4, 3, 0, 0))

W1   <- A %*% t(A)
W1_e <- eigen(W1)
sqrt(W1_e$values)

W2   <- t(A) %*% A
W2_e <- eigen(W2)
sqrt(W2_e$values)

s <- svd(A)
D <- diag(s$d)
U <- s$u
V <- s$v


W2_e$vectors %*% diag(sqrt(W2_e$values)) %*% W1_e$vectors
U %*% D %*% V
