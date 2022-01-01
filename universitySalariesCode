install.packages("sf")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("sp")
install.packages("RODBC")
install.packages("readxl")
install.packages("naniar")
install.packages("ggplot2")

library(sf)
library(tidyverse)
library(leaflet)
library(sp)
library(RODBC)
library(readxl)
library(naniar)
library(ggplot2)

x <- "hello world"
x

v <-c(1:10)
v

l = list("dog", "cat", "bird", "squirrel")
l


animals <- c("dog", "cat", "bird", "squirrel")
animals

"dog" %in% animals
"fish" %in% animals

match("dog", animals)

rm(list=ls())

setwd("C:/Users/adarcangelo083021/OneDrive - Corporate Technologies Inc/Documents/GitHub/wimlds")
salaries <- read_excel("University Salaries.xlsx")

help(View)
example(View)
View(salaries)

str(salaries)
summary(salaries)
dim(salaries)
length(salaries)
names(salaries)

salaries <- replace_with_na_all(salaries,condition = ~.x == "N/A")

quantile(salaries$'StartingMedianSalary')
plot(salaries$StartingMedianSalary)
boxplot(salaries$StartingMedianSalary)

names(salaries)[2] <- 'schoolType'
point1 <- salaries %>%
  ggplot( aes(x=StartingMedianSalary,y=schoolType)) +
  geom_point(fill="#69b3a2", color="#1478db", alpha=0.9) +
  ggtitle("Starting Median Salary")

point1
#Why or why not is this a good representation of this data?
