webr::install("blockr", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))
webr::install("blockr.data", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))

library(blockr)
library(ggplot2)
library(dplyr)
library(shinyAce)

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
          splice = TRUE)
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
    expr = quote({
      do.call(.(theme), list()) +
      ggplot2::theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
        legend.title = element_text(face = "bold"),  # Make legend title bold
        legend.position = "bottom"  # Position legend at the bottom
      )
    }),
    class = c("theme_block", "plot_layer_block", "plot_block"),
      ...
  )
}

# Custom mutate block that does need submit
new_mutate_block <- function(value = NULL, ...) {
  fields <- list(value = new_keyvalue_field(value = value, submit = FALSE), 
        expression = new_hidden_field(blockr:::mutate_expr))
    new_block(fields = fields, expr = quote(.(expression)), ..., 
        class = c("mutate_block", "transform_block"))
}

new_hbdata_block <- function(...) {
  new_block(
    fields = list(),
    expr = quote({
      merged_data <- blockr.data::lab %>% 
        dplyr::filter(LBTEST == "Hemoglobin") %>%
          inner_join(blockr.data::demo, by = c("STUDYID", "USUBJID"))

        # Step 2: Filter for Hemoglobin test results and preprocess
        hemoglobin_data <- merged_data %>%
          filter(LBTEST == "Hemoglobin") %>% # Keep only the rows where LBTEST is "Hemoglobin"
          filter(!startsWith(VISIT, "UNSCHEDULED")) %>%  # Remove rows with unscheduled visits
          arrange(VISITNUM) %>% # Sort data by VISITNUM (visit number)
          mutate(VISIT = factor(VISIT, levels = unique(VISIT), ordered = TRUE))  # Convert VISIT to ordered factor
        
        
        # Step 3: Compute summary statistics (Mean and SE) grouped by VISIT and ACTARM
        hemoglobin_data %>%
          group_by(VISIT, ACTARM) %>%  # Group data by VISIT and ACTARM
          summarise(
            Mean = mean(LBSTRESN, na.rm = TRUE),  # Calculate the mean of LBSTRESN, ignoring NA values
            SE = sd(LBSTRESN, na.rm = TRUE) / sqrt(n()),  # Calculate the standard error
            ymin = Mean - SE,
            ymax = Mean + SE,
            .groups = "drop"  # Drop the grouping
          )
    }),
    ...,
    class = c("hbdata_block", "data_block")
  )
}

register_blocks(
  constructor = c(
    new_ungroup_block,
    new_ggplot_block,
    new_geompoint_block,
    new_geomerrorbar_block,
    new_geomline_block,
    new_labs_block,
    new_theme_block,
    new_mutate_block
  ),
  name = c(
   "Ungroup block",
   "ggplot block",
   "Geom point block",
   "Geom error bar block",
   "Geom line block",
   "Labs block",
   "Theme block",
   "Mutate block"
  ),
  description = c(
    "Creates an ungroup block",
    "Creates a ggplot block",
    "Creates a geom point block",
    "Creates a geom errorbar block",
    "Creates a geom line block",
    "Creates a label block",
    "Creates a theme block",
    "Creates a mutate block"
  ),
  classes = list(
    c("ungroup_block", "transform_block"),
    c("ggplot_block", "plot_block"),
    c("geompoint_block", "plot_layer_block", "plot_block"),
    c("geomerrorbar_block", "plot_layer_block", "plot_block"),
    c("geomline_block", "plot_layer_block", "plot_block"),
    c("labs_block", "plot_layer_block", "plot_block"),
    c("theme_block", "plot_layer_block", "plot_block"),
    c("mutate_block", "transform_block")
  ),
  input = c(
    "data.frame",
    "data.frame",
    rep("ggplot", 5),
    "data.frame"
  ),
  output = c(
    "data.frame",
    rep("ggplot", 6),
    "data.frame"
  ),
  package = "blockr.demo",
  category = c(
    "transform",
    rep("visualization", 6),
    "transform"
  )
)

ws <- set_workspace(
  hb_data = new_stack(
    data = new_hbdata_block,
    title = "Hemoglobin data",
    name = "hb_data"
  )
)

ui <- bslib::page_fluid(
  div(
    style = "display: none",
    shinyAce::aceEditor(
      outputId = "ace",
      # to access content of `selectionId` in server.R use `ace_selection`
      # i.e., the outputId is prepended to the selectionId for use
      # with Shiny modules
      selectionId = "selection",
      value = "plop",
      placeholder = "Show a placeholder when the editor is empty ..."
    )
  ),
  generate_ui(ws, id = "myworkspace")
)
server <- function(input, output, session) {
  generate_server(ws, id = "myworkspace")
}
shinyApp(ui, server)

#set_workspace()
#serve_workspace(clear = FALSE)

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
#    mutate = new_mutate_block(),
#    title = "Summary data"
#  ),
#  plot = new_stack(
#    new_result_block("summary_data")#,
#    #new_ggplot_block(x = "VISIT", y = "MEAN"),
#    #new_geompoint_block(func = c("color", "shape"), default_columns = c("ACTARM", "ACTARM")),
#    #new_errorbar_block(func = c("ymin", "ymax", "color"), default_columns = c("ymin", "ymax", "ACTARM")),
#    #new_labs_block(x_lab = "Visit Label", y_lab = "Hemoglobin (g/dL)", title = "Mean and SD of Hemoglobin by Visit"),
#    #new_theme_block()
#  )
#)

#serve_workspace(clear = FALSE)