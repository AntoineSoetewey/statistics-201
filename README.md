# statistics-201

A Shiny app for performing statistical inference on mean(s), proportion(s), and variance(s) — step by step, by hand.

Live app: https://antoinesoetewey.shinyapps.io/statistics-201/

More details and a walkthrough: https://statsandr.com/blog/a-shiny-app-for-inferential-statistics-by-hand/

## Purpose and overview

This app is designed to help students learn and practice inferential statistics. It guides users through hypothesis tests and confidence intervals for the most common one- and two-sample scenarios, showing all intermediate calculations so that the reasoning behind each procedure is transparent.

Users select the type of inference, enter their data (either raw observations or summary statistics), specify the null hypothesis and significance level, and the app displays the full step-by-step solution along with a plot of the rejection region.

## Key features

- **Seven inference procedures** available from a single dropdown:
  - One mean
  - Two means — independent samples
  - Two means — paired samples
  - One proportion
  - Two proportions
  - One variance
  - Two variances

- **Flexible data entry**: enter raw comma-separated data or provide summary statistics directly (sample size, sample mean/proportion/variance).

- **Known or unknown population variance**: for mean-based tests the user can specify whether the population variance is known (Z-test) or unknown (t-test).

- **Equal or unequal variances**: for two-sample mean tests with unknown variances, choose between pooled (equal variances) and Welch (unequal variances) procedures.

- **Configurable hypothesis**: set the null hypothesis value and choose between two-sided, greater, or less alternative hypotheses.

- **Significance level**: adjustable via a slider from 0.01 to 0.20 (default 0.05).

- **Step-by-step output**: all computations (test statistic, critical value(s), confidence interval, and conclusion) are displayed with full mathematical notation rendered via MathJax.

- **Rejection region plot**: a ggplot2 visualisation of the test statistic relative to the critical region.

## Dependencies

The app requires R and the following packages:

- [`shiny`](https://cran.r-project.org/package=shiny)
- [`shinythemes`](https://cran.r-project.org/package=shinythemes)
- [`ggplot2`](https://cran.r-project.org/package=ggplot2)
- [`EnvStats`](https://cran.r-project.org/package=EnvStats)

Install them with:

```r
install.packages(c("shiny", "shinythemes", "ggplot2", "EnvStats"))
```

## Running the app locally

1. Clone or download this repository.
2. Open `statistics-201.Rproj` in RStudio, or set the repository root as your working directory in R.
3. Install the dependencies listed above if needed.
4. Launch the app:

```r
shiny::runApp()
```

The app will open in your default browser.

## Related apps

This app is part of a set of three complementary Shiny apps developed for students while I was a teaching assistant at UCLouvain. All three apps are still actively used when teaching introductory statistics and probability courses.

- **statistics-101** — compute probabilities for the main probability distributions: https://github.com/AntoineSoetewey/statistics-101
- **statistics-201** — perform statistical inference on mean(s), proportion(s), and variance(s): https://github.com/AntoineSoetewey/statistics-201
- **statistics-202** — simple linear regression by hand: https://github.com/AntoineSoetewey/statistics-202

## License

This project is licensed under the terms of the Creative Commons Attribution 4.0 International License (CC BY 4.0). See [LICENSE](LICENSE) for more details.
