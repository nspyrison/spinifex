data <- flea[, 1:6]
p <- ncol(data)
b_rand <- basis_random(p = p)
b_iden <- basis_identity(p = p)
is_orthornormal(b_rand)
is_orthornormal(b_iden)

dp <- data_proj(data, b_iden, manip_var="head", manip="radial", to=pi/4) ###!!! GET NAN'S WHEN USING B_IDEN
pb <- proj_basis(data, dp)
is_orthornormal(pb)
is_orthornormal(b_rand)

    mat <- pb
    mat_t <- t(mat)
    all.equal(mat_t %*% mat, diag(ncol(basis)))
    mat_t %*% mat #expect diag(2)