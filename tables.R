library(tidyverse)
library(xtable)
library(readxl)
library(ggplot2)
library(dplyr)

source("common.R")

## produce tables and graphs

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'
sss <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

all = merge(sss,sat,by="instance",suffixes=c(".sss",".sat"))

## playground


ggplot(data=subset(all,solved.sss==1&solved.sat==1))+geom_point(aes(x=planner_memory.sss,y=planner_memory.sat))+geom_abline()+scale_x_log10()+scale_y_log10()+labs(x="OptSearch",y="SAT")
