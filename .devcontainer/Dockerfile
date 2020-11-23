#FROM dsmdmmdev.azurecr.io/base-r:master.20201118.1

FROM rocker/tidyverse:3.6.2


# sudo add-apt-repository -y "ppa:marutter/rrutter3.5"
# sudo add-apt-repository -y "ppa:marutter/c2d4u3.5"
# sudo sh -c 'apt update && apt upgrade'
# sudo apt-get update && sudo apt-get upgrade

#"CXXFLAGS=-g -O2 $(LTO)"

# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, "Makevars")
# if (!file.exists(M)) file.create(M)
# cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
#     "CXX14=g++", # or clang++ but you may need a version postfix
#     "CXXFLAGS=-g -O2 -fdebug-prefix-map=/build/r-base-t3diwe/r-base-3.6.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g $(LTO)",
#     file = M, sep = "\n", append = TRUE)


# rstan, bayesplot, shinyWidgets, stringi, strex



#RUN apt-get update -qq \
# && apt-get -y --no-install-recommends install \
# && install2.r --error \
# --deps TRUE \
# shiny \
# shinyWidgets \
# shinydashboard \
# plyr \
# dplyr \
# knitr