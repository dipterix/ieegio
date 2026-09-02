# Sys.setenv("IEEGIO_NO_PYTHON" = "TRUE")
enable_debugging(TRUE)

on_cran <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(env))
  }
}

if (!on_cran() && isTRUE(Sys.info()["login"] == "dipterix")) {
  cache_root <- tools::R_user_dir("ieegio", "cache")

  if (!dir.exists(cache_root)) {
    dir.create(cache_root, showWarnings = FALSE, recursive = TRUE)
  }

  # ieegio/tests/testthat"
  # print(getwd())

  file.copy(
    "../../inst/sample_data/",
    cache_root,
    overwrite = TRUE,
    recursive = TRUE
  )
}
