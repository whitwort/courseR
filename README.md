# courseR

This package implements my workflow for (1) creating websites for courses I teach in R and (2) deploying a custom R package that students can use to manage assignments in the class.  

For goal (1), as [R Markdown](https://github.com/rstudio/rmarkdown) functionality has improved over the years, this project has evolved into a boilerplate for using `rmarkdown::render_site` to create a course website where content renders as both handout style documents and `reveal.js` slides.  

Goal (2) is useful if your students are using R in a managed environment (eg. RStudio Server).

## Quick start

Install from github:

```r
devtools::install_github("whitwort/courseR")
```

Start a new project in the current working directory (example content will be created):

```r
courseR::init()
```

Build the website and package with:

```r
courseR::build()
```

See `courseR.yml` for configuration options and files in `templates/` to customize the generated website or R package.

## Version

* 0.4  Yup, it happened again.  The new `rmarkdown::render_site` supercedes most of what this package used to do.  Also added v2.0 of the course package implementation.
* 0.3  Rewritten to leaverage new features in RMarkdown v2.0
* 0.2  Rewritten to use whisker for templating and make heavier use of new Knitr features
* 0.1  Initital experiment in developing a workflow to produce both handout-style webpages and HTML slides from a common set of markdown sources.

## Credits

* [rmarkdown](http://rmarkdown.rstudio.com/) and in-turn [pandoc](http://pandoc.org/) for most of the heavy-lifting behind website creation
* Twitter [bootstrap](http://getbootstrap.com/) for layout and styles
* [Shiny](https://shiny.rstudio.com/) for interactive assignments viewer

## License

Copyright © 2013-2017 Gregg Whitworth and licensed under [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html).
