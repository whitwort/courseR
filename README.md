# courseR

courseR is a tool chain designed to build websites from [R Markdown](https://github.com/rstudio/rmarkdown) sources.  This third (and hopefully final) rewrite now leverages the power of R Markdown v2, which in turn uses [Knitr](http://yihui.name/knitr/) and [Pandoc](http://johnmacfarlane.net/pandoc/) to do the heavy lifting during page rendering.  While originally designed to produce websites for courses that are taught primarily in R, the package is likely to be generally useful when building any sort of website from a set of RMarkdown sources.  The chief features are:

1.  Source files can be RMarkdown (.rmd), plain markdown (.md), or R scripts.  The courseR headers are now fully compatible with RMarkdown v2.0 headers, making it easy to do *ad hoc* page renders with RMarkdown (which will come packaged in future versions of RStudio).

2.  Single source files can specify multiple output destinations; for example, in the default project, single .RMD sources are used to produce handout-style HTML documents, PDFs, and HTML slides.

3.  Separation in the build workflow between an intermediate page rendering step and the final website building step to allow templates to make use of site-wide information (for example, to build global tables of contents).

4.  Smart(ish) caches between builds.  The default Knitr caching mechanism caches the results of chunk blocks, re-runing blocks if the code in them changes.  Unfortunately, there's no way to automatically invalidate these caches when external files they depend upon change.  Instead of using R object chunk-caches, courseR caches the output that is rendered by RMarkdown.  If source files or data they depend upon change, all associated output files are re-rendered; if templates or configuration settings for a particular output target change, only these files are re-rendered.

## Getting started

### Installing

courseR and RMarkdown v2.0 are both on github, so the easiest way to install them is to use the devtools package.  If you don't have it already:

```r
install.packages("devtools")
```

To install courseR and the new version of RMarkdown from github:

```r
devtools::install_github("rstudio/rmarkdown")
devtools::install_github("whitwort/courseR")
```

### Starting a project

To initialize a new template project directory either start a blank project in RStudio or manually set your working directory into an empty path, then call:

```r
library(courseR)
courseR.init()
```

See the new README.md file in the root of the project folder for a full description of the structure of the subfolders that were created and the courseR.yaml file for example configuration options.  When you're ready to build the project website, just call:

```r
courseR.build()
```

## Version

* 0.3  Rewritten to leaverage new features in RMarkdown v2.0
* 0.2  Rewritten to use whisker for templating and make heavier use of new Knitr features
* 0.1  Initital experiment in developing a workflow to produce both handout-style webpages and HTML slides from a common set of markdown sources.

## License

Copyright © 2013-2014 Gregg Whitworth and licensed under [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html).