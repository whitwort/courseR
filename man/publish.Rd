% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/courseR.r
\name{publish}
\alias{publish}
\title{Publish the latest build of the project}
\usage{
publish(path = getwd(), build = TRUE, www = buildSite(config),
  pkg = buildPackage(config))
}
\arguments{
\item{path}{path to the project to publish}

\item{build}{should the project be rebuilt before publishing}

\item{www}{should the website be published}

\item{pkg}{should the package be published}
}
\description{
Copies the lastest build of (optionally) the website and package to paths 
specified in in the `publish` section of the `courseR.yml` configuration
file.  The current user obviously needs write permissions for the specified
path.
}
