
[![Travis-CI Build Status](https://travis-ci.org/gchung05/mds.svg?branch=master)](https://travis-ci.org/gchung05/mds) <!-- README.md is generated from README.Rmd. Please edit that file -->

Why Use `mds`?
--------------

Medical device event data are messy.

Common challenges include:

-   Performing ongoing surveillance on messy data
-   Quickly answering simple questions such as:
    -   Are events trending up?
    -   How did my trends look 1 year ago? 2 years ago?
-   Incompatibility of various sources of device-events
-   Difficulty integrating exposures, a.k.a. denominator data
-   Understanding all the possible combinations to analyze
-   Application of disproportionality analysis (DPA)
-   Documentation of analyses in a auditable, reproducible way

How Do I Use `mds`?
-------------------

The `mds` package provides a standardized framework to address these challenges:

-   Standardize events involving medical devices
-   Standardize exposures of the device (also known as opportunities for an event to occur, or event denominator)
-   Enumerate possible analyses in a flexible way
-   Generate times series of analyses for trending over time
-   Set up analyses for easy application of disproportionality analysis (DPA)
-   Save all files in lightweight `R` files for auditability, documentation, and reproducibility

**Note on Statistical Algorithms**

`mds` data and analysis standards allow for seamless application of various statistical trending algorithms via the `mdsstat` package (under development).

Raw Data to Trending in 4 Steps
-------------------------------

The general workflow to go from data to trending over time is as follows:

1.  Use `deviceevent()` to standardize device-event data.
2.  Use `exposure()` to standardize exposure data (optional).
3.  Use `define_analyses()` to enumerate possible analysis combinations.
4.  Use `time_series()` to generate counts (and/or rates) by time based on your defined analyses.
