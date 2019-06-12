# Project Notes for creating a R package

## How to create a R package?
1. Put all the function in one .R file
2. Run it and use package.skeleton() to create a R package
3. Edit the help file skeletons in 'man', possibly combining help files for multiple functions.
4. Edit the exports in 'NAMESPACE', and add necessary imports.
5. In Rstudio, create a project in the existing directory (created by package.skeleton())
6. In Rstudio, from "Build", check the package. 
7. Build source package from "Build" in Rstudio. After that, one .tar.gz file will be built.

## Notes
- Write all the function in one .R file before using package.skeletion(). It will create separated files (.R & .Rd) for each function.
- Before checking package, must add title for each .Rd file at least, otherwise, there will be an error.

## Some library we might use

library(roxygen2)
library(devtools)