# Clean up sample data downloaded during testing

is_dev <- tryCatch(
  {
    isTRUE(Sys.info()['login'] == "dipterix")
  },
  error = function(e) {
    FALSE
  },
  warning = function(e) {
    FALSE
  }
)

if(!is_dev) {
  cachr_root <- tools::R_user_dir("ieegio", "cache")
  if (file.exists(cachr_root)) {
    unlink(cachr_root, recursive = TRUE, force = TRUE)
  }
}

