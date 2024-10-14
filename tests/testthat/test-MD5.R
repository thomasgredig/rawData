test_that("MD5 check sum", {
  filename = raw.getSampleFiles()
  md5_value = raw.getMD5(filename)
  expect_equal(as.character(md5_value), "8b9e85")
})
