test_that("SQL database location", {
  num = floor(runif(1,0,100))
  projectName = paste0("sql",num)
  tmpDir = get_test_RAW_folder(4, projectName)
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      verbose=FALSE)
  sqlfile = dir(tmpDir, pattern="lite$", full.names = TRUE)
  expect_true(file.exists(sqlfile))

  # move SQL to new folder
  newSQLdir= projectName
  dir.create(file.path(tmpDir, newSQLdir))
  MOVED = file.rename(from = sqlfile,
              to = file.path(dirname(sqlfile),newSQLdir,basename(sqlfile)))
  expect_true(MOVED)

  # expect DB not to be found anymore
  expect_true(!file.exists(raw.getDatabase(rawBase)))
  rawBase <- raw.update(rawBase, sqlPath = file.path(dirname(sqlfile), newSQLdir))
  # DB is found again
  expect_true(file.exists(raw.getDatabase(rawBase)))

  # delete database
  file.remove(raw.getDatabase(rawBase))
})


test_that("update version", {
  num = floor(runif(1,101,200))
  projectName = paste0("sql",num)
  tmpDir = get_test_RAW_folder(2, projectName)
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      verbose=FALSE)
  sqlfile = dir(tmpDir, pattern="lite$", full.names = TRUE)
  # change SQL to old version
  old_sqlfile = gsub("(.*)-\\d+\\.\\d+\\.\\d+\\.sqlite$","\\1-0.0.0.sqlite",sqlfile)
  expect_true(file.rename(from = sqlfile, to =old_sqlfile))
  expect_true(file.exists(old_sqlfile))
  # initialize again; should recognize old SQL file and update version back.
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      verbose=FALSE)

  expect_true(file.exists(sqlfile))

  # delete database
  file.remove(raw.getDatabase(rawBase))
})

test_that("version", {
  file_paths = c("dataCalibration1-1.2.1.sqlite","dataCalibration1-0.0.0.9000.sqlite",
                 "dataCalibration1-1.1.22.sqlite","dataCalibration1-1.sqlite",
                 "dataCalibration1-1.2.sqlite")
  latest_file = get_highest_version_file(file_paths)
  expect_equal(latest_file, file_paths[1])
})
