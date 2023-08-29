# Methodology Write-up

All the Rmd and HTML files describing the methodology of the stock assessment prioritization for West Coast groundfish. Information in these files is presented in the "Methodology" tab in the Shiny app. 

To update the Rmd files, follow these steps as needed:

1. **Add Headers**: Use hash symbols (`#`) to create headers. A single hash symbol creates the main header, and additional hash symbols create subheaders. For example:

```r
# Main Header
## Subheader
```

# Main Header
## Subheader

2. **Add Links**: Insert links using square brackets for text and parentheses for URLs. For example:

```r
[Google](https://www.google.com/)
```
[Google](https://www.google.com)

3. **Math Equations**: Use LaTeX syntax to include mathematical equations. For inline equations, use single dollar signs, while for equations on a new line, use double dollar signs. For example:

```r
This is an inline equation: $y = mx + b$, and this is an equation on a new line: $$y = mx + b$$
```

This is an inline equation: $y = mx + b$, and this is an equation on a new line: $$y = mx + b$$

4. **gt Tables**: Use an R code block to include tables. Ensure that the `gt` library is installed in the code block before rendering and saving the file. For more information about `gt` and its functions, click [here](https://gt.rstudio.com/).

Before running the Shiny app, knit the edited Rmd file to HTML. Changes made should then be visible on the app.

To edit the appearance of the HTML files within the app, edit the `height` parameter where each file is rendered in `server.R`. All outputs are under the comment:

```r
# render HTML files for methodology page
```

and each output name matches to its corresponding `htmlOutput` label in `ui.R`.
