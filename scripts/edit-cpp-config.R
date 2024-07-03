dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX17FLAGS=-O3 -march=native -mtune=native -fPIC",
    "CXX17=g++", # or clang++ but you may need a version postfix
    file = M, sep = "\n", append = TRUE)