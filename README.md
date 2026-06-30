# Analytical Dashboard on R Shiny

An interactive web dashboard for data analysis built with the [Shiny](https://shiny.posit.co/) framework and [shinydashboard](https://rstudio.github.io/shinydashboard/).

## Features

- Dataset selection: `iris`, `mtcars`, `faithful`
- 4 chart types: scatter, bar, histogram, boxplot
- Value boxes with key metrics (rows, columns, unique values)
- Interactive data table (DT)
- Adjustable sample size via slider

## Tech Stack

- **Language:** R
- **UI:** shinydashboard
- **Charts:** ggplot2
- **Data processing:** dplyr
- **Tables:** DT

## Getting Started

```bash
Rscript run.R
```

The app will open in your browser at `http://127.0.0.1:3838`.

### Manual dependency installation

```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "DT"))
```

## Project Structure

```
.
├── app.R       # Main application file (UI + Server)
├── run.R       # Launch script with dependency installation
└── README.md
```

## Screenshot

![Dashboard](screenshot.png)
