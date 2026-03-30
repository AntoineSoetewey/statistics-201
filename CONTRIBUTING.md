# Contributing to statistics-201

Thank you for your interest in improving this project.

This app was built to help students learn inferential statistics step by step, and I am very grateful for any contribution that can make it clearer, more accurate, or easier to use.

## Before you start

- Please read the [Code of Conduct](CODE_OF_CONDUCT.md).
- If possible, open an issue first for larger changes so we can align on scope before you spend time implementing.
- Keep pull requests focused. Small, targeted PRs are much easier to review.

## Ways to contribute

You are welcome to contribute in many ways, for example:

- Correcting statistical formulas, explanations, or edge-case handling
- Improving UI labels, wording, or educational clarity
- Fixing bugs in the Shiny app behavior
- Improving documentation in [README.md](README.md)

## Local setup

1. Clone this repository.
2. Open `statistics-201.Rproj` in RStudio (or set the repo root as your working directory in R).
3. Install dependencies:

```r
install.packages(c("shiny", "shinythemes", "ggplot2", "EnvStats"))
```

4. Run the app:

```r
shiny::runApp()
```

## Development notes

- Main app logic and UI are in `app.R`.
- Static assets are under `www/`.
- Please preserve the educational, step-by-step spirit of the app when proposing changes.
- Try to keep naming and formatting consistent with the existing code.

## Pull request checklist

Before submitting a PR, please:

- Verify the app starts locally without errors.
- Test the inference modes affected by your change.
- Update documentation if behavior or UI text changed.
- Keep commit messages clear and descriptive.

## Review process

I may not always be able to review quickly, but I will do my best to respond as soon as possible.

Thank you again for your time, care, and generosity in contributing to this educational project.
