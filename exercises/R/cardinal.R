new_cardinal09_block <- function(
  id_var = "USUBJID",
  arm_var = "ARM",
  saffl_var = "SAFFL",
  pref_var = "AEDECOD",
  show_colcounts = TRUE,
  ...) {
  
  all_cols <- function(data) colnames(data)
  
  fields <- list(
    # TO FILL
  )
  
  new_block(
    fields = "<TO_REPLACE>", #1
    expr = "<TO_REPLACE>", #2
    ...,
    class = "<TO_REPLACE>" #3
  )
}

generate_server.cardinal09_block <- blockr:::generate_server.
transform_block

uiOutputBlock.cardinal09_block <- function(x, ns) {
  gt::gt_output(ns("res"))
}

server_output.cardinal09_block <- function(x, result, output) {
  gt::render_gt(result())
}

register_cardinal_blocks <- function(pkg) {
  register_block(
    constructor = ..., #TBD
    name = "<TO_REPLACE>",
    description = "<TO_REPLACE>",
    classes = c("<TO_REPLACE>"),
    input = "<TO_REPLACE>",
    output = "<TO_REPLACE>",
    package = pkg
  )

  # YOU CAN ADD MORE BLOCKS BELOW
}