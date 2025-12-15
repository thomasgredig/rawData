test_that("find raw files", {
  tmpDir = get_test_RAW_folder(2, "spinPc")

  rawBase=create_rawBase("spinPc", paths=tmpDir, sqlPaths=tmpDir)

  s <- raw.find(rawBase, quiet=TRUE)
  expect_true(length(s) >= 2)

})

