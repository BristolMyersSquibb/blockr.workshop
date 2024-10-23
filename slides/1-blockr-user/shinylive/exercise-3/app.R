webr::install("blockr", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))

library(blockr)
library(palmerpenguins)
library(ggplot2)

new_ggplot_block <- function(col_x = character(), col_y = character(), ...) {

  data_cols <- function(data) colnames(data)

  new_block(
    fields = list(
      x = new_select_field(col_x, data_cols, type = "name"),
      y = new_select_field(col_y, data_cols, type = "name")
    ),
    expr = quote(
      ggplot(mapping = aes(x = .(x), y = .(y)))
    ),
    class = c("ggplot_block", "plot_block"),
    ...
  )
}

new_geompoint_block <- function(color = character(), shape = character(), ...) {

  data_cols <- function(data) colnames(data$data)

  new_block(
    fields = list(
      color = new_select_field(color, data_cols, type = "name"),
      shape = new_select_field(shape, data_cols, type = "name")
    ),
    expr = quote(
      geom_point(aes(color = .(color), shape = .(shape)), size = 2)
    ),
    class = c("geompoint_block", "plot_layer_block", "plot_block"),
    ...
  )
}

new_penguins_block <- function(selected = character(), ...) {
  new_dataset_block(selected, package = "palmerpenguins", ...)
}

register_blocks(
  constructor = c(new_penguins_block, new_ggplot_block, new_geompoint_block),
  name = c("penguins block", "ggplot block", "geompoint block"),
  description = c(
    "Data from palmerpenguins package",
    "Builds a ggplot object",
    "Add points geom to ggplot object"
  ),
  classes = list(
    c("palmer_penguins", "dataset_block", "data_block"),
    c("ggplot_block", "plot_block"),
    c("geompoint_block", "plot_layer_block", "plot_block")
  ),
  input = c(NA_character_, "data.frame", "ggplot"),
  output = c("data.frame", "ggplot", "ggplot"),
  package = "blockr.demo",
  category = c("custom data", "visualisation", "visualisation")
)

set_workspace(
  data = new_stack(
    data_block = new_dataset_block("penguins", "palmerpenguins"),
    title = "Data"
  )
)
serve_workspace(clear = FALSE)