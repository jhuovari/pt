toy_system <- function() {
  inds <- c("A", "B", "C")
  Z <- matrix(c(10, 20, 5,
                30, 5, 10,
                0, 15, 20), 3, byrow = TRUE, dimnames = list(inds, inds))
  x <- c(A = 100, B = 120, C = 80)
  Zm <- matrix(c(5, 10, 2,
                 1, 0, 8), 2, byrow = TRUE, dimnames = list(c("m1", "m2"), inds))
  va <- x - colSums(Z) - colSums(Zm)
  A <- ptfin::pt_coefficients(Z, x)
  structure(list(
    year = 2000L, industries = inds, labels = c(A = "a", B = "b", C = "c"),
    Z = Z, x = x, A = A, L = ptfin::pt_leontief(A), Zm = Zm,
    Am = ptfin::pt_coefficients(Zm, x), imports = colSums(Zm), taxes = 0 * x,
    va = va, comp = 0.6 * va, gos = 0.3 * va, cfc = 0.1 * va, othertax = 0 * x,
    emp = c(A = 1, B = 0.5, C = 2),
    fd = cbind(P3KS14 = c(A = 30, B = 40, C = 20)),
    fd_imports = c(P3KS14 = 10), fd_all = rbind(PUR_S2 = c(P3KS14 = 0))
  ), class = "pt_system")
}
