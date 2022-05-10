# learnitdown

<!-- badges: start -->

[![R-CMD-check](https://github.com/Sciviews/learnitdown/workflows/R-CMD-check/badge.svg)](https://github.com/Sciviews/learnitdown/actions) [![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/learnitdown?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/learnitdown) [![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/learnitdown/master.svg)](https://codecov.io/github/SciViews/learnitdown?branch=master) [![CRAN Status](https://www.r-pkg.org/badges/version/learnitdown)](https://cran.r-project.org/package=learnitdown) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Lifecycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

<!-- badges: end -->

Use R Markdown and {bookdown} to build e-learning material with h5p, differed loading of {Shiny} and {learnr} applications, course-specific content, and much more...

## Installation

(Not yet! The latest stable version of {learnitdown} can simply be installed from [CRAN](http://cran.r-project.org):)

``` r
install.packages("learnitdown")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {learnitdown} package from GitHub (source from **master** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/learnitdown")
```

R should install all required dependencies automatically, and then it should compile and install {learnitdown}.

Latest development version of {learnitdown} (source + Windows binaries for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/learnitdown/build/artifacts).

## Further explore {learnitdown}

You can get further help about this package this way. Make the {learnitdown} package available in your R session:

``` r
library("learnitdown")
```

Get help about this package:

``` r
library(help = "learnitdown")
help("learnitdown-package")
vignette("learnitdown") # None is installed with install_github()
```

For further instructions, please, refer to these help pages at <https://www.sciviews.org/learnitdown/>.

## Code of Conduct

Please note that the {learnitdown} project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
