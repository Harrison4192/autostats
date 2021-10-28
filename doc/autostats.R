## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(broom)
library(broom.mixed)

## ----setup--------------------------------------------------------------------
library(autostats)


## -----------------------------------------------------------------------------
iris %>% 
  tidy_formula(target = Species) -> species_formula

species_formula

## ----cache=TRUE---------------------------------------------------------------
iris %>% 
  auto_variable_contributions(species_formula)

## ----cache=TRUE---------------------------------------------------------------
iris %>% 
  auto_model_accuracy(species_formula)

## ----cache=TRUE---------------------------------------------------------------
iris %>% 
  filter(Species != "setosa") %>% 
  auto_variable_contributions(species_formula)

## ----cache=TRUE---------------------------------------------------------------
iris %>% 
  filter(Species != "setosa") -> iris_binary

iris_binary %>% 
  auto_model_accuracy(species_formula)

## -----------------------------------------------------------------------------
iris %>% 
  tidy_formula(target = Petal.Length) -> petal_formula

petal_formula

## ----cache=F------------------------------------------------------------------
iris %>% 
  auto_variable_contributions(petal_formula)

## ----cache=TRUE---------------------------------------------------------------
iris %>% 
  auto_model_accuracy(petal_formula)

## -----------------------------------------------------------------------------
iris %>% 
  auto_anova(Species, matches("Petal"), baseline = "first_level")

