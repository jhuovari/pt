test_that("Leontief ja kertoimet ovat johdonmukaisia", {
  s <- toy_system()
  expect_equal(unname(s$L %*% (diag(3) - s$A)), diag(3))
  f <- s$x - rowSums(s$Z)
  expect_equal(as.vector(s$L %*% f), unname(s$x))
  m <- pt_multipliers(s)
  expect_equal(m$output, unname(colSums(s$L)))
  imp <- pt_impact(s, f)
  expect_equal(unname(imp$summary["output"]), sum(s$x))
  expect_equal(unname(imp$summary["value_added"]), sum(s$va))
})

test_that("tyypin II vaikutukset ovat suuremmat kuin tyypin I", {
  s <- toy_system()
  i1 <- pt_impact(s, c(A = 10))
  i2 <- pt_impact(s, c(A = 10), type2 = TRUE)
  expect_gt(i2$summary[["output"]], i1$summary[["output"]])
})

test_that("hintamalli: tuontihinnan shokki = am' L", {
  s <- toy_system()
  p <- pt_price_model(s, imported = c(m1 = 1))
  expect_equal(p$total, as.vector(s$Am["m1", ] %*% s$L))
  expect_equal(p$direct, unname(s$Am["m1", ]))
})

test_that("hintamalli: eksogeeninen toimiala", {
  s <- toy_system()
  p <- pt_price_model(s, domestic = c(A = 0.1))
  N <- c("B", "C")
  expect_equal(p$total[1], 0.1)
  expected <- as.vector(0.1 * s$A["A", N] %*% solve(diag(2) - s$A[N, N]))
  expect_equal(p$total[2:3], expected)
  # Lineaarisuus
  p2 <- pt_price_model(s, domestic = c(A = 0.2))
  expect_equal(p2$total[2:3], 2 * p$total[2:3])
})

test_that("kokonaiskustannusosuus >= suora osuus", {
  s <- toy_system()
  t <- pt_total_input_share(s, "A", "m1")
  expect_true(all(t$total[!t$exogenous] >= t$direct[!t$exogenous]))
  expect_true(is.na(t$total[t$industry == "A"]))
  expect_equal(t$direct[1], unname(s$A["A", "A"] + s$Am["m1", "A"]))
})

test_that("kytkentäindeksien keskiarvo on 1", {
  l <- pt_linkages(toy_system())
  expect_equal(mean(l$backward), 1)
  expect_equal(mean(l$forward), 1)
})

test_that("välittymiskanavat summautuvat kokonaisvaikutukseen", {
  s <- toy_system()
  p <- pt_price_model(s, domestic = c(A = 1), imported = c(m2 = 1))
  ch <- pt_price_channels(s, "C", domestic = c(A = 1), imported = c(m2 = 1))
  expect_equal(sum(ch$contribution), p$total[p$industry == "C"])
})
