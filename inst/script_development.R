

usethis::use_build_ignore("script_development.R")
usethis::use_build_ignore("inst/script_testes_templates.R")

usethis::use_git_ignore("inst/script_development.R")
usethis::use_git_ignore("inst/script_testes_template.R")

#construindo o pacote

devtools::document()
devtools::install()
devtools::load_all()
devtools::check()

devtools::test()


usethis::use_data(dados)
usethis::use_data(dados2, overwrite = T)
