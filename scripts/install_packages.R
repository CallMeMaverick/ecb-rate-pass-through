packages <- readLines("requirements.txt")

installed <- rownames(installed.packages())
for (pkg in packages) {
    if (!(pkg %in% installed)) {
        install.packages(pkg, dependencies = TRUE)
    }
}

invisible(lapply(packages, library, character.only = TRUE))
