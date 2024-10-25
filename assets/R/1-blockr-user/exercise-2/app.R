webr::install("blockr", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))
webr::install("blockr.data", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))

library(blockr)
library(ggplot2)

new_ggplot_block <- function(col_x = character(), ...) {

  data_cols <- function(data) colnames(data)

  new_block(
    fields = list(
      x = new_select_field(col_x, data_cols, type = "name")
    ),
    expr = quote(
      ggplot(mapping = aes(x = .(x)))
    ),
    class = c("ggplot_block", "plot_block"),
    ...
  )
}

new_geomhistogram_block <- function(binwidth = 5, fill = "blue", alpha = 0.7, ...) {
  new_block(
    fields = list(
      binwidth = new_numeric_field(binwidth, min = 5, max = 20),
      fill = new_string_field(fill),
      alpha = new_numeric_field(alpha, min = 0, max = 1)
    ),
    expr = quote(
      ggplot2::geom_histogram(binwidth = .(binwidth), fill = .(fill), alpha = .(alpha))
    ),
    class = c("geomhistogram_block", "plot_layer_block", "plot_block"),
    ...
  )
}

new_theme_block <- function(...) {
  new_block(
    fields = list(
      theme = new_select_field("theme_minimal", 
        (function(x) {
            x[!x %in% c("theme_set", "theme_get", "theme_update", 
                "theme_test")]
        })(grep("^theme_.*$", ls("package:ggplot2"), perl = TRUE, 
            value = TRUE)), type = "name")
    ),
    expr = quote(do.call(.(theme), list())),
    class = c("theme_block", "plot_layer_block", "plot_block"),
      ...
  )
}

new_labs_block <- function(x_lab = character(), y_lab = x_lab, title = x_lab, ...) {
  new_block(
    fields = list(
      x = new_string_field(x_lab), 
      y = new_string_field(y_lab),
      title = new_string_field(title)
    ), 
    expr = quote(ggplot2::labs(x = .(x), y = .(y), title = .(title))),
    class = c("labs_block", "plot_layer_block", "plot_block"),
    ...
  )
}

new_scalefillbrewer_block <- function(palette = character(), ...) {

  pal_choices <- c(
    "Blues",
    "Greens",
    "Greys",
    "Oranges",
    "Purples",
    "Reds"
  )

  if (!length(palette) || !(palette %in% pal_choices)) palette <- pal_choices[1]

  new_block(
    fields = list(
      palette = new_select_field(palette, pal_choices)
    ), 
    expr = quote(ggplot2::scale_fill_brewer(palette = .(palette))),
    class = c("scalefillbrewer_block", "plot_layer_block", "plot_block"),
    ...
  )
}

new_customdata_block <- function(selected = character(), ...) {
  new_dataset_block(selected, package = "blockr.data", ...)
}

register_blocks(
  constructor = c(new_customdata_block, new_geomhistogram_block, new_theme_block, new_labs_block, new_scalefillbrewer_block, new_ggplot_block),
  name = c("customdata block", "geom histogram block", "theme block", "labs block", "scalefillbrewer block", "ggplot block"),
  description = c(
    "Data from CDISC package",
    "ggplot histogram layer",
    "ggplot theme layer",
    "ggplot labels",
    "Scale fill brewer",
    "Builds a ggplot object"
  ),
  classes = list(
    c("customdata_block", "dataset_block", "data_block"),
    c("geomhistogram_block", "plot_layer_block", "plot_block"),
    c("theme_block", "plot_layer_block", "plot_block"),
    c("labs_block", "plot_layer_block", "plot_block"),
    c("scalefillbrewer_block", "plot_layer_block", "plot_block"),
    c("ggplot_block", "plot_block")
  ),
  input = c(NA_character_, "ggplot", "ggplot", "ggplot", "ggplot", "data.frame"),
  output = c("data.frame", "ggplot", "ggplot", "ggplot", "ggplot", "ggplot"),
  package = "blockr.demo",
  category = c("custom data", "visualization", "visualization", "visualization", "visualization", "visualization")
)
stack <- new_stack()
serve_stack(stack)