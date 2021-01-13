
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CPTrackRApp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

The goal of CPTrackRApp is to [fix track
identifiers](https://forum.image.sc/t/tracking-label/16409) from
[CellProfiler](https://cellprofiler.org/)’s sqlite output. CPTrackRApp
is a Shiny app that analyses the sqlite file, and determines the
parameters for [CPTrackR](https://github.com/burgerga/CPTrackR) to fix
the track. Additionally it shows the CellProfiler pipeline metadata, and
previews the fixed tracks.

## Installation

You can install the development version of CPTrackRApp from with:

``` r
# install.packages("remotes")
remotes::install_github("burgerga/CPTrackRApp")
```

## Usage

To start the app use

``` r
CPTrackRApp::run_app()
```

**Screenshot:**

![CPTrackRApp screenshot](man/figures/screenshot.png)
