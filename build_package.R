# load developer libraries ------------------------------------------------
xfun::gsub_dir(dir = "R", pattern = "presenteR", replacement = "presenter")



library(pacman)
p_load(rstudioapi, devtools, roxygen2, usethis, pkgdown,
       ymlthis, magrittr, fs, covr, gitcreds, credentials,
       badger, hexSticker, gh, tidyverse)

usethis::use_vignette("autostats")
# add this file to .Rbuildignore ------------------------------------------


file_name  <- rstudioapi::getSourceEditorContext()$path %>% fs::path_file()
use_build_ignore(file_name)



# begin pkgdown -----------------------------------------------------------

usethis::use_pkgdown()

# create yaml -------------------------------------------------------------

ymlthis::pkgdown_template() %>%
  ymlthis::use_pkgdown_yml()

# usethis: add packages ---------------------------------------------------

usethis::use_pipe()



usethis::use_package("doMC")
usethis::use_package("flextable")
usethis::use_package("foreach")
usethis::use_package("nnet")
usethis::use_package("parsnip")
usethis::use_package("presenteR")
usethis::use_package("recipes")
usethis::use_package("rsample")
usethis::use_package("tune")
usethis::use_package("workflows")



usethis::use_package("rstudioapi")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("tidyselect")
usethis::use_package("purrr")
usethis::use_package("janitor")
usethis::use_package("tibble")
usethis::use_package("rlang")
usethis::use_package("lubridate")
usethis::use_package("gtools")

usethis::use_package("Matrix")
usethis::use_package("framecleaner")
usethis::use_package("ggeasy")
usethis::use_package("ggplot2")
usethis::use_package("jtools")
usethis::use_package("ggthemes")
usethis::use_package("moreparty")
usethis::use_package("party")
usethis::use_package("patchwork")
usethis::use_package("tidyr")
usethis::use_package("xgboost")
usethis::use_package("lightgbm")





usethis::use_r("tidy_rpart")
usethis::use_package("badger", type = "Suggests")



# edit R profile ----------------------------------------------------------


edit_r_profile()



# add rmd sections with usethis -------------------------------------------

use_readme_rmd()
use_news_md()
use_mit_license()
usethis::use_vignette()



# add badges to readme ----------------------------------------------------

use_lifecycle_badge("experimental")
use_cran_badge()
use_github_actions_badge()
`r badger::badge_cran_download("dataCleaner", "grand-total", "blue")`
`r badger::badge_code_size("Harrison4192/dataCleaner")`
`r badger::badge_last_commit("Harrison4192/dataCleaner")`

# set github token --------------------------------------------------------

# gh_token_help()
create_github_token()
gitcreds_set()
gitcreds_get()
set_github_pat()
# credentials::git_credential_forget()
gh::gh_whoami()
gh_token()

credentials::credential_helper_get()
git_credential_ask()
# git config --global credential.helper osxkeychain
# use github actions and links --------------------------------------------



usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("render-rmarkdown")
usethis::use_github_action("pkgdown")
usethis::use_github_actions()
usethis::use_github_links()
usethis::use_github_pages()


# install TC packages -----------------------------------------------------

# install.packages("devtools")
devtools::install_github("Harrison4192/validata")
# install.packages("devtools")
devtools::install_github("Harrison4192/framecleaner")
# install.packages("devtools")
devtools::install_github("Harrison4192/tidyBins")
# install.packages("devtools")
devtools::install_github("Harrison4192/presenteR")
# install.packages("devtools")
# devtools::install_github("Harrison4192/autoStats")


# build and check ---------------------------------------------------------

devtools::document()
devtools::build_readme()
devtools::build_site()
devtools::check()
devtools::preview_site()
devtools::load_all()
devtools::build_vignettes()
devtools::spell_check()
devtools::release(check = T)

usethis::use_cran_comments(open = rlang::is_interactive())
devtools::check_win_devel()
devtools::check_rhub()

use_r("get_unique_col_names")

p_load(tidyverse)
p_load(treesnip)
iris %>%
  get_unique_col_names() -> ko


