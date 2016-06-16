context("analysis_skeleton")
options(warn = 2)
expected_files <- c(".git", "data", "out", "tmp", 
                    ".gitignore", "01_load.R", "02_clean.R", "03_analysis.R", 
                    "04_output.R", "CONTRIBUTING.md", "CODE_OF_CONDUCT.md", 
                    "internal.R", "LICENSE", "README.md", "run_all.R")

name_incrementer <- function(name) {
  i <- 0
  function() {
    i <<- i + 1
    paste0(name, i)
  }
}

increment_foo <- name_incrementer("foo")

test_that("analysis_skeleton works with dot path and git_init", {
  base_dir <- increment_foo()
  dir <- tempdir()
  dir.create(file.path(dir, base_dir))
  setwd(file.path(dir, base_dir))
  ret <- analysis_skeleton()
  skip_on_travis()
  expect_equal(normalizePath(ret, winslash = "/"), 
               normalizePath(".", winslash = "/"))
  files <- list.files(all.files = TRUE, full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files, winslash = "/")), 
               sort(normalizePath(
                 file.path(".", c(expected_files, paste0(base_dir, ".Rproj"))), 
                 winslash = "/")))
})

test_that("analysis_skeleton works with relative path and git init", {
  base_dir <- increment_foo()
  dir <- tempdir()
  setwd(dir)
  ret <- analysis_skeleton(base_dir, git_init = TRUE)
  skip_on_travis()
  expect_equal(normalizePath(ret, winslash = "/"), 
               normalizePath(file.path(dir, base_dir), winslash = "/"))
  files <- list.files(file.path(dir, base_dir), all.files = TRUE, 
                      full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files, winslash = "/")), 
               sort(normalizePath(
                 file.path(dir, base_dir, c(expected_files, paste0(base_dir, ".Rproj"))), 
                 winslash = "/")))
})

test_that("analysis_skeleton works with absolute path and git init", {
  base_dir <- increment_foo()
  dir <- file.path(tempdir(), base_dir)
  ret <- analysis_skeleton(dir, git_init = TRUE)
  skip_on_travis()
  expect_equal(normalizePath(ret), normalizePath(dir))
  files <- list.files(dir, all.files = TRUE, full.names = TRUE, 
                      include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files)), 
  			   sort(normalizePath(file.path(dir, 
  			                                c(expected_files, paste0(base_dir, ".Rproj"))))))
})

bare_repo_path <- file.path(tempdir(), "bare_test_repo.git")
dir.create(bare_repo_path, recursive = TRUE)
bare_repo <- init(bare_repo_path, bare = TRUE)

test_that("analysis_skeleton works with dot path and git clone", {
  base_dir <- increment_foo()
  dir <- tempdir()
  dir.create(file.path(dir, base_dir))
  setwd(file.path(dir, base_dir))
  ret <- analysis_skeleton(git_clone = bare_repo)
  skip_on_travis()
  expect_equal(normalizePath(ret), normalizePath("."))
  files <- list.files(all.files = TRUE, full.names = TRUE, 
                      include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files)), 
  			   sort(normalizePath(file.path(".", 
  			                                c(expected_files, paste0(base_dir, ".Rproj"))))))
})

test_that("analysis_skeleton works with relative path and git clone", {
  base_dir <- increment_foo()
  dir <- tempdir()
  setwd(dir)
  ret <- analysis_skeleton(base_dir, git_clone = bare_repo)
  skip_on_travis()
  expect_equal(normalizePath(ret, winslash = "/"), 
  			   normalizePath(file.path(dir, base_dir), winslash = "/"))
  files <- list.files(file.path(dir, base_dir), all.files = TRUE, 
                      full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files, winslash = "/")), 
  			   sort(normalizePath(
  			     file.path(dir, base_dir, c(expected_files, paste0(base_dir, ".Rproj"))), 
  			               winslash = "/")))
})

test_that("analysis_skeleton works with absolute path and git clone", {
  base_dir <- increment_foo()
  dir <- file.path(tempdir(), base_dir)
  ret <- analysis_skeleton(dir, git_clone = bare_repo)
  skip_on_travis()
  expect_equal(normalizePath(ret, winslash = "/"), 
  			   normalizePath(dir, winslash = "/"))
  files <- list.files(dir, all.files = TRUE, full.names = TRUE, 
                      include.dirs = TRUE, no.. = TRUE)
  skip_on_travis()
  expect_equal(sort(normalizePath(files, winslash = "/")), 
  			   sort(normalizePath(
  			     file.path(dir, c(expected_files, paste0(base_dir, ".Rproj"))), 
  			     winslash = "/")))
})

unlink(bare_repo_path, recursive = TRUE, force = TRUE)