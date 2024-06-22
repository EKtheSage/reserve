source("renv/activate.R")

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(), viewer = "Beside")
    })
  }
}

# does this make default conda environment?
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(
    file.path(
      Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
      ".vscode-R", "init.R"))
}