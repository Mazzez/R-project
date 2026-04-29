pkgs <- c("dplyr", "ggplot2", "lubridate", "Kendall", "trend",
          "forecast", "strucchange", "tidyr", "scales")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install,
                   repos = "https://cloud.r-project.org",
                   dependencies = TRUE)
}

invisible(lapply(pkgs, library, character.only = TRUE))
cat("Packages OK :", paste(pkgs, collapse = ", "), "\n")
