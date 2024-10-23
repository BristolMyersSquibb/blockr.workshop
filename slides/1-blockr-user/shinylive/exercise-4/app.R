webr::install("blockr", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))
webr::install("blockr.data", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))

library(blockr)
library(ggplot2)
library(dplyr)

new_customdata_block <- function(selected = character(), ...) {
  new_dataset_block(selected, package = "blockr.data", ...)
}

new_ungroup_block <- function(...) {
  new_block(
    fields = list(),
    expr = quote(dplyr::ungroup()),
    ...,
    class = c("ungroup_block", "transform_block")
  )
}

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

new_geompoint_block <- function(func = c("shape", "color"), default_columns = character(), size = integer(), ...) {
  if (length(default_columns) > 0) {
    stopifnot(length(func) == length(default_columns))
  }

  sub_fields <- function(data, funcs) {
    all_cols <- colnames(data$data)
    tmp_selects <- lapply(seq_along(funcs), function(i) {
        default <- if (length(default_columns) > 0) {
            default_columns[[i]]
        }
        else {
            all_cols[[1]]
        }
        new_select_field(value = default, choices = all_cols)
    })
    names(tmp_selects) <- funcs
    tmp_selects
  }
  ggplot_expr <- function(data, funcs, columns) {
      if (length(funcs) == 0) {
          return(quote(TRUE))
      }
      if (length(columns) == 0) {
          return(quote(TRUE))
      }
      tmp_exprs <- lapply(funcs, function(fun) {
          col <- columns[[fun]]
          if (is.null(col)) {
              return(quote(TRUE))
          }
          if (!any(col %in% colnames(data$data))) {
              return(quote(TRUE))
          }
          col <- as.name(col)
          expr <- bquote(.(column), list(column = col))
          bquote(.(expr), list(expr = expr, column = col))
      })
      names(tmp_exprs) <- funcs
      bquote(ggplot2::geom_point(ggplot2::aes(..(exprs))), list(exprs = tmp_exprs), 
          splice = TRUE)
  }
  func_choices <- c("shape", "color")
  fields <- list(
    funcs = new_select_field(func, func_choices, multiple = TRUE),
    columns = new_list_field(sub_fields = sub_fields), 
    expression = new_hidden_field(ggplot_expr)
  )
  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ..., 
    class = c("geompoint_block", "plot_layer_block", "plot_block")
  )
}

new_geomerrorbar_block <- function(func = c("ymin", "ymax", "color"), default_columns = character(), width = integer(), ...) {
  if (length(default_columns) > 0) {
    stopifnot(length(func) == length(default_columns))
  }
  sub_fields <- function(data, funcs) {
      all_cols <- colnames(data$data)
      tmp_selects <- lapply(seq_along(funcs), function(i) {
          default <- if (length(default_columns) > 0) {
              default_columns[[i]]
          }
          else {
              all_cols[[1]]
          }
          new_select_field(value = default, choices = all_cols)
      })
      names(tmp_selects) <- funcs
      tmp_selects
  }
  ggplot_expr <- function(data, funcs, columns) {
      if (length(funcs) == 0) {
          return(quote(TRUE))
      }
      if (length(columns) == 0) {
          return(quote(TRUE))
      }
      tmp_exprs <- lapply(funcs, function(fun) {
          col <- columns[[fun]]
          if (is.null(col)) {
              return(quote(TRUE))
          }
          if (!any(col %in% colnames(data$data))) {
              return(quote(TRUE))
          }
          col <- as.name(col)
          expr <- bquote(.(column), list(column = col))
          bquote(.(expr), list(expr = expr, column = col))
      })
    names(tmp_exprs) <- funcs
      bquote(ggplot2::geom_errorbar(ggplot2::aes(..(exprs))), list(exprs = tmp_exprs), 
          splice = FALSE)
  }
  func_choices <- c("ymin", "ymax", "color")
  fields <- list(
    funcs = new_select_field(func, func_choices, multiple = TRUE),
    columns = new_list_field(sub_fields = sub_fields), 
    expression = new_hidden_field(ggplot_expr)
  )
  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ..., 
    class = c("geomerrorbar_block", "plot_layer_block", "plot_block")
  )
}

new_geomline_block <- function(func = c("group", "color"), default_columns = character(), ...) {
  if (length(default_columns) > 0) {
    stopifnot(length(func) == length(default_columns))
  }
  sub_fields <- function(data, funcs) {
      all_cols <- colnames(data$data)
      tmp_selects <- lapply(seq_along(funcs), function(i) {
          default <- if (length(default_columns) > 0) {
              default_columns[[i]]
          }
          else {
              all_cols[[1]]
          }
          new_select_field(value = default, choices = all_cols)
      })
      names(tmp_selects) <- funcs
      tmp_selects
  }
  ggplot_expr <- function(data, funcs, columns) {
      if (length(funcs) == 0) {
          return(quote(TRUE))
      }
      if (length(columns) == 0) {
          return(quote(TRUE))
      }
      tmp_exprs <- lapply(funcs, function(fun) {
          col <- columns[[fun]]
          if (is.null(col)) {
              return(quote(TRUE))
          }
          if (!any(col %in% colnames(data$data))) {
              return(quote(TRUE))
          }
          col <- as.name(col)
          expr <- bquote(.(column), list(column = col))
          bquote(.(expr), list(expr = expr, column = col))
      })
      names(tmp_exprs) <- funcs
      bquote(ggplot2::geom_line(ggplot2::aes(..(exprs))), list(exprs = tmp_exprs), 
          splice = TRUE)
  }
  func_choices <- c("group", "color")
  fields <- list(
    funcs = new_select_field(func, func_choices, multiple = TRUE),
    columns = new_list_field(sub_fields = sub_fields), 
    expression = new_hidden_field(ggplot_expr)
  )
  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ..., 
    class = c("geomline_block", "plot_layer_block", "plot_block")
  )
}

set_workspace()
serve_workspace(clear = FALSE)

#set_workspace(
#  lab_data = new_stack(
#    data = new_dataset_block("lab", "blockr.data"),
#    title = "Lab data"
#  ),
#  merged_data = new_stack(
#    data = new_dataset_block("demo", "blockr.data"),
#    join = new_join_block(y = "lab_data", type = "inner", by = c("STUDYID", "USUBJID"), submit = TRUE),
#    title = "Merged data"
#  ),
#  hb_data = new_stack(
#    data = new_result_block("merged_data"),
#    filter = new_filter_block(columns = "LBTEST", values = "Hemoglobin", submit = TRUE),
#    arrange = new_arrange_block("VISITNUM"),
#    mutate = new_mutate_block(),
#    title = "Hemoglobin data"
#  ),
#  summary_data = new_stack(
#    data = new_result_block("hb_data"),
#    group_by = new_group_by_block("VISIT", "ACTARM"),
#    summarize = new_summarize_block(func = c("mean", "se"), default_columns = c("LBSTRESN", "LBSTRESN"), submit = TRUE),
#    ungroup = new_ungroup_block(),
#    title = "Summary data"
#  ),
#  plot = new_stack(
#    new_ggplot_block(x = "VISIT", y = "MEAN"),
#    new_geompoint_block(func = c("color", "shape"), default_columns = c("ACTARM", "ACTARM"))
#  )
#)
#
#serve_workspace(clear = FALSE)