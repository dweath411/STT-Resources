# Principal Components Analysis
cov_matrix <- cov(USJudgeRatings[1:11])
eigen_cov <- eigen(cov_matrix)
trans_x <- as.matrix(USJudgeRatings[1:11]) %*% t(solve(eigen_cov$vectors))
trans_reg <- cbind(USJudgeRatings[12],trans_x)
colnames(trans_reg) <- c("Y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11")
pca_reg <- lm(data =trans_reg, Y ~ x1 + x2 +x3 +x4 + x5 + x6 + x7+ x8 + x9 + x10 + x11)
orig_coeff <- eigen_cov$vectors %*% as.matrix(pca_reg$coefficients[2:12])

norm_coeff <- orig_coeff * apply(USJudgeRatings,2,sd)[1:11]
sens_vals <- norm_coeff/sum(abs(norm_coeff))
sens_vals
norm_coeff_pca <- pca_reg$coefficients[2:12] * apply(trans_x,2,sd)
sens_vals_pca <- norm_coeff_pca / sum(abs(norm_coeff_pca))
sens_vals_pca