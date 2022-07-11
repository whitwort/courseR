# courseR

This package features a toolset for creating and updating website content for courses taught in R, especially in a managed environment running RStudio and/or Shiny servers.

## Quick start

Install from github:

```r
devtools::install_github("whitwort/courseR")
```

Start a new project in the current working directory (by default, some example content will be created):

```r
courseR::init()
```

Build a website and package with:

```r
courseR::build()
```

See `courseR.yml` for configuration options and files in `templates/` to customize the generated website and R package.

You can preview the website by opening files in the `dist/` subdirectory.  Then, after setting things up in `courseR.yml` publish your website and course application with:

```r
courseR::publish()
```

## Components

There are currently a few components, all of which are configurable from the `courseR.yml` file created after calling `courseR::init`:

* **Website generation**.  Source R Markdown files are built into "hand-out" style web pages and [reveal.js](https://github.com/hakimel/reveal.js/) slides.  Use `courseR::newContent()` to get a fresh course content R Markdown file.
* **Course package**.   By default `courseR` creates a course-specific R package; this is useful when students are interacting with R on a managed server, especially if you're also running RStudio Server.  The course package exports some convenience functions like `courseR::website`, but also helps implement a course assignment workflow.  The course package is built when you call `courseR::build` and published to a global R path defined in `courseR.yml` with `courseR::publish`.
* **Assignments**. Assignment documents, templated with `courseR::newAssignment` are built to reference solutions by instructors in the course and scaffolded into assignment R Markdowns files for students.  The assignments component depends on the course package and provides automatic code checking facilities (via [checkr](https://github.com/dtkaplan/checkr)) and a [Shiny](https://shiny.rstudio.com/) grading app.
* **Projects**. Provides some infrastructure for helping students in the class to deploy Shiny apps to a Shiny Server instance. This component is disable by default; see `courseR.yml` for instructions about how to enable it.

## Version

* 0.5  Re-implementation and overhaul of grading application; now integrates with [checkr](https://github.com/dtkaplan/checkr) while maintaining original pedagogical goal of showing students an answer key as their target.
* 0.4  Yup, it happened again.  The new `rmarkdown::render_site` supercedes most of what this package used to do.  Also added v2.0 of the course package implementation.
* 0.3  Rewritten to leaverage new features in RMarkdown v2.0
* 0.2  Rewritten to use whisker for templating and make heavier use of new Knitr features
* 0.1  Initital experiment in developing a workflow to produce both handout-style webpages and HTML slides from a common set of markdown sources.

## Credits

* [rmarkdown](http://rmarkdown.rstudio.com/) and in-turn [pandoc](http://pandoc.org/) for most of the heavy-lifting behind website creation
* Twitter [bootstrap](http://getbootstrap.com/) for layout and styles
* [Shiny](https://shiny.rstudio.com/) for interactive assignment apps

## License

Copyright © 2013-2022 Gregg Whitworth and licensed under [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html).
