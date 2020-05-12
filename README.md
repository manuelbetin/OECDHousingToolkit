# Housing Toolkit 

----

## Description

Provide template for the creation of web chapters (html) and country fiches (pdf) with OECD style

## Authors

- Manuel Betin
- Maxime Nguyen
- Federica Depace

## Version

0.2.1

## Install package

```r

#install and load packages
install.package("devtools")
library(devtools)

#install toolkit package
devtools::install_github("manuelbetin/OECDHousingToolkit")

#load toolkit

library(OECDHousingToolkit)

```

## Open Rmardown templates

Open a new R studio section and load the package

```r
library(OECDHousingToolkit)
```
 Then click on create new file and click on Rmarkdown

![](https://user-images.githubusercontent.com/57252006/81654387-d40caf00-9435-11ea-9a6f-9a1b8cfef911.png)

Click from template and select the template to use as well as the name of the folder and the location where to create the new template

![](https://user-images.githubusercontent.com/57252006/81654868-251ca300-9436-11ea-9ad2-dbcb6616dfb4.png)

Once you selected housing_countryfiches it will create a folder containing the template.

In this folder you will find the template "thenameyougave.Rmd" and the necessary files for compilation. To generate the country fiche:


## Country fiches generator

In order to generate the country fiches please follow the instructions

open new r script and save it in the folder of the template: country_fiche_generator.r

```r
library(OECDHousingToolkit)
setwd(pathofthenewlycreatedfolder)
```

Select the iso3 code of the countries of which you want to create the country fiche and 
change the name of the .Rmd file to the name of the .Rmd file in the folder of the template

```r

ctries=c("USA","FRA")
htk_generate_ctry_fiches(Rmdfile="nameofRmdfile.Rmd", #name of the template of the country fiches
                         country_code_list=ctries, #list of countries of interest
                         path="Country_fiches") #name of the folder where to store the pdf files created
```

run the code

