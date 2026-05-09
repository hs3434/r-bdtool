test_that("creat_dir creates directory", {
  tmpdir <- tempdir()
  test_path <- file.path(tmpdir, paste0("test_creat_dir_", round(runif(1) * 1e6)))

  expect_false(dir.exists(test_path))
  creat_dir(test_path)
  expect_true(dir.exists(test_path))

  unlink(test_path, recursive = TRUE)
})

test_that("creat_dir does not warn if directory exists", {
  tmpdir <- tempdir()
  expect_no_warning(creat_dir(tmpdir))
})

test_that("creat_dir creates nested directories", {
  tmpdir <- tempdir()
  test_path <- file.path(tmpdir, paste0("test_nested_", round(runif(1) * 1e6)), "a", "b", "c")

  creat_dir(test_path)
  expect_true(dir.exists(test_path))

  unlink(file.path(tmpdir, paste0("test_nested_", round(runif(1) * 1e6))), recursive = TRUE)
})

test_that("create_file_dir creates directory for file path", {
  tmpdir <- tempdir()
  test_path <- file.path(tmpdir, paste0("test_cfd_", round(runif(1) * 1e6)), "subdir", "file.txt")

  expect_false(dir.exists(file.path(tmpdir, paste0("test_cfd_", round(runif(1) * 1e6), "subdir"))))
  create_file_dir(test_path)
  expect_true(dir.exists(dirname(test_path)))

  unlink(file.path(tmpdir, paste0("test_cfd_", round(runif(1) * 1e6))), recursive = TRUE)
})