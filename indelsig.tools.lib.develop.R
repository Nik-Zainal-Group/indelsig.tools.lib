# setwd to file directory

load("data/indel_template_type_4.rda")
load("data/indel_template_type_4_full.rda")
load("data/indel_type_4_figurelabel.rda")
load("data/indel_template_type_4_full_figurelabel.rda")

usethis::use_data(indel_template_type_4,
                  indel_template_type_4_full,
                  indel_template_type_4_full_figurelabel,
                  indel_type_4_figurelabel,
                  internal = TRUE,
                  overwrite = TRUE)

devtools::document()
devtools::install()
# devtools::install(dependencies = FALSE)

