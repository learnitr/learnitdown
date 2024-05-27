# learnitdown- R markdown, bookdown and learnr additions for learning material

<!-- badges: start -->

[![R-CMD-check](https://github.com/learnitr/learnitdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/learnitr/learnitdown/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/learnitr/learnitdown/master.svg)](https://codecov.io/github/learnitr/learnitdown?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/learnitdown)](https://cran.r-project.org/package=learnitdown)
[![r-universe status](https://learnitr.r-universe.dev/badges/learnitdown)](https://learnitr.r-universe.dev/learnitdown)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

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
