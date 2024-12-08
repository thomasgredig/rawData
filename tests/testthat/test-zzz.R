test_that("zzz: delete data folder", {
  p_data <- file.path(here::here(),'data')
  if (dir.exists(p_data)) {
    f_rawBase = file.path(p_data, 'rawBase.rda')
    if(file.exists(f_rawBase)) {
      unlink(p_data, recursive = TRUE)
    }
  }

  expect_true(!dir.exists(p_data))
})
