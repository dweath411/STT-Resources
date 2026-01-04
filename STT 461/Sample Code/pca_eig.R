penguin_x <- penguin[2:4]
eig <- eigen(cov(penguin_x))
eig
pca_p_x <- data.matrix(penguin_x) %*% eig$vectors
cov(pca_p_x)
pca_reg_data <- data.frame(penguin$body_mass_g, pca_p_x)
summary(lm(data = pca_reg_data, pca_reg_data$penguin.body_mass_g~X1+X3))