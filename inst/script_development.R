

usethis::use_build_ignore("C:\Users\Usuario\Desktop\R Avançado\Jonathan/inst/script_development.R")
usethis::use_build_ignore("inst/script_testes_templates.R")


#construindo o pacote
devtools::document()
devtools::install()
devtools::load_all()

#checando se o pacote está funcionando
devtools::check()
