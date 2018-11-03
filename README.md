
`forecast463` Overview
======================

This package is created to allow students to submit their daily forecasts for the PSU STAT 463 projects. The main function consists in the `send_prediction` which takes the following inputs:

-   `group`: the number of your group which should be an integer between 1 and 17.
-   `prediction`: the list object described in the "Project Description" document posted in the Piazza course page (under "Resources").
-   `from`: your group email address in the correct format (e.g. `"psu.forecasting.group.2@gmail.com"`).
-   `key`: the character string representing the unique key assigned to your group which you received by email.

Here is a stylized example of the usage of the function `send_prediction`:

``` r
group = 2
from = "psu.forecasting.group.2@gmail.com"
key = "aaaaaaaaaaaaaaa" 
send_prediction(group = group, prediction = prediction, from = from, key = key)
```

Install Instructions
--------------------

To install the `forecast463` package, please run the following code:

``` r
devtools::install_github("SMAC-Group/forecast463")
```