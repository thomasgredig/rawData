test_that("SQL database location", {
  num = floor(runif(1,0,100))
  projectName = paste0("sql",num)
  tmpDir = get_test_RAW_folder(4, projectName)
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      verbose=FALSE)
  sqlfile= dir(tmpDir, pattern="lite$", full.names = TRUE)
  expect_true(file.exists(sqlfile))

  # move SQL to new folder
  newSQLdir= projectName
  dir.create(file.path(tmpDir, newSQLdir))
  MOVED = file.rename(from = sqlfile,
              to = file.path(dirname(sqlfile),newSQLdir,basename(sqlfile)))
  expect_true(MOVED)

  # expect DB not to be found anymore
  expect_true(!file.exists(raw.getDatabase(rawBase)))
  rawBase <- raw.update(rawBase, sqlPath = file.path(dirname(sqlfile),newSQLdir))
  # DB is found again
  expect_true(file.exists(raw.getDatabase(rawBase)))
})
