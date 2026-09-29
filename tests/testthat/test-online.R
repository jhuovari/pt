test_that("StatFin-aineisto täsmää (verkkotesti)", {
  skip_on_cran()
  skip_if_offline("pxdata.stat.fi")
  s <- pt_system(2023)
  expect_length(s$industries, 64)
  expect_equal(sum(s$Zm), sum(s$imports), tolerance = 1e-6)
  # Tuotos = välituotteet (kotimaiset + tuonti) + tuoteverot + arvonlisäys
  expect_lt(max(abs(colSums(s$Z) + s$imports + s$taxes + s$va - s$x)), 0.1)
  sh <- pt_input_shares(c(sahko = "35"), years = 2023)
  expect_true(all(sh$share_purchaser >= 0 & sh$share_purchaser < 1))
})
