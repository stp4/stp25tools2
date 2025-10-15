test_that("set_opt works", {

  expect_true( is.list(get_opt()))


  set_opt(percent = list(digits = 10))
  expect_equal(get_opt("percent", "digits" ),10)



  set_opt(percent = list(digits = 0))
  expect_equal(get_opt("percent", "digits" ), 0)


  expect_equal(
  clnsng("Hallo Welt!"),
 "Hallo_Welt"
  )

})
