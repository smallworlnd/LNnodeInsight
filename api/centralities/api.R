#!/usr/bin/env Rscript

library(plumber)
library(tidyverse)
library(tidygraph)
library(magrittr)

load('../../graph.Rda')

pr("plumber.R") %>% pr_run(port=8000)
