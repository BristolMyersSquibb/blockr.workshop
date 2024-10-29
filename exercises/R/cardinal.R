new_cardinal09_block <- function(
  id_var = "USUBJID",
  arm_var = "ARM",
  saffl_var = "SAFFL",
  pref_var = "AEDECOD",
  show_colcounts = TRUE,
  ...) {
  
  all_cols <- function(data) colnames(data) #1 Exercise 1
  
  fields <- list( #1 Exercise 1
    # TO FILL
  )
  
  new_block(
    fields = "<TO_REPLACE>", #1 Exercise 1
    expr = quote({ #2 Exercise 2
      # TO FILL
    }),
    ...,
    class = "<TO_REPLACE>" #3 Exercise 3
  )
}

# Needed so that we can render the table since
# by default, blockr does not know about gt elements
generate_server.cardinal09_block <- blockr:::generate_server.transform_block

uiOutputBlock.cardinal09_block <- function(x, ns) {
  gt::gt_output(ns("res"))
}

server_output.cardinal09_block <- function(x, result, output) {
  gt::render_gt(result())
}

# Exercise 4
register_cardinal_blocks <- function(pkg) {
  register_block(
    constructor = "<TO_REPLACE>",
    name = "<TO_REPLACE>",
    description = "<TO_REPLACE>",
    classes = c("<TO_REPLACE>"),
    input = "<TO_REPLACE>",
    output = "<TO_REPLACE>",
    package = pkg
  )

  # YOU CAN ADD MORE BLOCKS BELOW
}