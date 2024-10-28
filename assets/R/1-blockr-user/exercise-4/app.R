webr::install("blockr", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))
webr::install("blockr.data", repos = c("https://bristolmyerssquibb.github.io/webr-repos/", "https://repo.r-wasm.org"))

library(blockr)

new_customdata_block <- function(selected = character(), ...) {
  new_dataset_block(selected, package = "blockr.data", ...)
}

register_blocks(
  constructor = c(
    new_customdata_block
  ),
  name = c(
   "Custom data block"
  ),
  description = c(
    "Creates a custom data block"
  ),
  classes = list(
    c("customdata_block", "data_block")
  ),
  input = c(
    NA_character_
  ),
  output = c(
    rep("data.frame", 2)
  ),
  package = "blockr.demo",
  category = c(
    "Custom data"
  )
)

set_workspace(
  lab_data  = new_stack(
    name = "lab_data",
    title = "Lab data"
  )
)
serve_workspace(clear = FALSE)
