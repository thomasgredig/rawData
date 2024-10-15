test_that("rbind dataRAW", {
  # create 2 files with different CRC and
  # bind together, expect IDs to be unique
  f1 = tempfile()
  writeLines("file1", f1)
  d1 = create_dataRAW(7,raw_paths = dirname(f1), f1 )

  f2 = tempfile()
  writeLines("file2", f2)
  d2 = create_dataRAW(7,raw_paths = dirname(f2),f2 )

  f3 = tempfile()
  writeLines("file 3", f3)
  d3 = create_dataRAW(7,raw_paths = dirname(f3),f3 )

  d4 = rbind(d1,d2)
  d4 = rbind(d4,d2)
  d4 = rbind(d4,d4)
  d4 = rbind(d4,d3)

  expect_true(is(d4, "dataRAW"))

  df4 = as.data.frame(d4)
  expect_equal(df4$ID, 7:9)
})
