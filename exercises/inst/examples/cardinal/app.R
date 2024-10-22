pkgload::load_all()
library(blockr)
library(cardinal)

my_stack <- new_stack(
  new_dataset_block("cadae", "random.cdisc.data"),
  new_cardinal09_block()
)

serve_stack(my_stack)