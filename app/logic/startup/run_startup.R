# To be sourced in main.R to call main_setup.py at app startup
box::use(reticulate[source_python, py_run_file])

#' @export
run_python_startup <- function(
    db_path     = "data/neuromics_registry.duckdb",
    script_path = "app/logic/startup/main_setup.py",
    python      = Sys.which("python3"),
    wait        = FALSE,    # starts Python script in background by default
    build_only  = FALSE
) {
  if (!file.exists(script_path)) {
    warning("Startup script not found: ", script_path); return(invisible(FALSE))
  }
  if (nchar(python) == 0) {
    warning("python3 not found on PATH"); return(invisible(FALSE))
  }
  
  if (isTRUE(build_only)) {
    Sys.setenv(NEX_BUILD_ONLY = "1")
    wait <- TRUE
  }
  on.exit({
    if (isTRUE(build_only)) Sys.unsetenv("NEX_BUILD_ONLY")
  }, add = TRUE)

  message("Running Python startup: ", script_path)
  
  # wrap single quotes = shell-safe
  # PYTHON_PATH    <- normalizePath(python,    mustWork = FALSE)
  # STARTUP_SCRIPT <- normalizePath(script_path, mustWork = FALSE)
  
  if (!isTRUE(wait)) {
    pid <- system2(
      command = (python),
      args    = c(shQuote(script_path)),
      stdout  = "",
      stderr  = "",
      wait    = FALSE
    )
    message("Python startup running in background (pid ", pid, ").")
    return(invisible(TRUE))
  }

  exit_code <- system2(
    command = (python),
    args    = c(shQuote(script_path)),
    stdout  = "",
    stderr  = "",
    wait    = TRUE
  )

  if (exit_code != 0) {
    warning("Startup script exited with code ", exit_code,
            " — check output above for details")
    return(invisible(FALSE))
  }

  message("Startup complete.")
  invisible(TRUE)

  # tryCatch({
  #   message("Running Python startup...")
  #   reticulate::py_run_file("app/logic/startup/main_setup.py")
  #   message("Startup complete.")
  # }, error = function(e) {
  #   warning("Startup script failed: ", e$message)
  # })
}
