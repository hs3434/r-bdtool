test_that("parse_call_chain formats call chain correctly", {
  trace <- list(
    as.call(list(quote(f1))),
    as.call(list(quote(f2), 1))
  )
  result <- parse_call_chain(trace)
  expect_match(result, "Call 1:.*f1")
  expect_match(result, "Call 2:.*f2")
})

test_that("parse_call_chain uses sys.calls() by default", {
  result <- parse_call_chain()
  expect_match(result, "Call [0-9]+:")
})