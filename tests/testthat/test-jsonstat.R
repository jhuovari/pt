test_that("json-stat2 puretaan oikeaan järjestykseen", {
  js <- jsonlite::fromJSON('{
    "id": ["a", "b"], "size": [2, 3],
    "dimension": {
      "a": {"category": {"index": {"x": 0, "y": 1}, "label": {"x": "X", "y": "Y"}}},
      "b": {"category": {"index": {"p": 0, "q": 1, "r": 2}}}
    },
    "value": [1, 2, 3, 4, null, 6]}', simplifyVector = FALSE)
  d <- jsonstat_to_df(js)
  expect_equal(d$a, c("x", "x", "x", "y", "y", "y"))
  expect_equal(d$b, c("p", "q", "r", "p", "q", "r"))
  expect_equal(d$a_label[4], "Y")
  expect_equal(d$value, c(1, 2, 3, 4, NA, 6))
})

test_that("harva value-muoto toimii", {
  js <- jsonlite::fromJSON('{
    "id": ["a"], "size": [3],
    "dimension": {"a": {"category": {"index": ["u", "v", "w"]}}},
    "value": {"0": 5, "2": 7}}', simplifyVector = FALSE)
  expect_equal(jsonstat_to_df(js)$value, c(5, NA, 7))
})
