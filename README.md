
# learndown

<!-- badges: start -->
[![Linux Build Status](https://travis-ci.com/SciViews/learndown.svg )](https://travis-ci.com/SciViews/learndown)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/learndown?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/learndown)
[![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/learndown/master.svg)
](https://codecov.io/github/SciViews/learndown?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/learndown)](https://cran.r-project.org/package=learndown)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Life
cycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

> Use R markdown and bookdown for e-learning with h5p, differed loading of Shiny and learnr applications, course-specific content, and much more...


## Installation

The latest stable version of **learndown** can simply be installed from [CRAN](http://cran.r-project.org):

```r
install.packages("learndown")
```

You can also install the latest developement version. Make sure you have the **remotes** R package installed (note: **not yet!**):

```r
install.packages("remotes")
```

Use `install_github()` to install the **learndown** package from Github (source from **master** branch will be recompiled on your machine):

```r
dremotes::install_github("SciViews/learndown")
```

R should install all required dependencies automatically, and then it should compile and install **learndown**.

Latest development version of **learndown** (source + Windows binaires for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/learndown/build/artifacts).

## Further explore learndown

You can get further help about this package this way: Make the **learndown** package available in your R session:

```r
library("learndown")
```

Get help about this package:

```r
library(help = "learndown")
help("learndown-package")
vignette("learndown") # None is installed with install_github()
```

For further instructions, please, refer to these help pages at https://www.sciviews.org/learndown/.

## Code of Conduct

Please note that the **learndown** project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
