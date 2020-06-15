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

## Documentation on Linguistic Description of Complex Phenomena

Literature on computational theory of perception
The NL system is stratefed in 4 strata:
 - Context: the meaning of NL depends on the domain of experience
 - Semantic: the meaning ressources stored in the NL system that are acquiredd through experience in terms of meaning feature
 - Lexico-grammar: the grammatical ressources in the NL system to realize the meaning at the semantical stratum as a a text
 - Expression: ressources to realize the final expression of text

Text generation = Context => Expression
Text understanding= Expression => Context

Granular linguistic model of a phenomenon as structured meaning. The structuration is a three step process:
 - A selection of a set of situation types
 - In each situation type select a set of figures
 - For each figure select a set of clauses

Computational perception (CP)
The computational model of a unit of meaning about the phenomenon to be modeled
CP is a couple (A,W) with A={a1,...,aN} a set of linguistic expressions that represents the whole linguistic domain of the CP
W={w1,...wN} set of vqlidity degree, the context of validity depends on the thruthfulness and perceverance of each sentence
in the context of use.

Perception mapping (PM) are used to create new CP
PM is a tupple (U,y,g,T)
U =(u1,...,uN) the input data
y = output CP y=(Ay,Wy)={(a1,w1),...,(an,Wn)}
g = aggregation function Wy=g(wu1,...,Wun) with w1,...,Wn a vector of degree validity
T = Text generqation algorithm that generates the linguistic expression in Ay. T has associated a figure and uses the input DATA to 
choose the most suitable clauses to describe the current state of the monitored phenomenon. 

GLMP consiste of a network of PMs
each PM receives a set of input CPs adn transmits upwards a CP. Each output CP is explained by the PM using a set of input CPs. In the 
network, each CP covers specific aspects of the phenomenon with certqin degree of granularity
1PM: first order perception mapping => those which are input to the GLMP.
PMs which input are CPs are called 2PM and their outputs are 2CP.

### The algorithm and technical background


Gracian Trivino*, Michio Sugeno, 2012, Towards linguistic descriptions of phenomena, 
European Centre for Soft Computing, Mieres, Asturias, Spain


### papers LDCP

P. Conde-Clemente, Jose M. Alonso, G. Trivino. "rLDCP: R package for text generation form data".
In Proceedings of the IEEE International Conference on Fuzzy Systems (FUZZ-IEEE), Naples, Italy, 2017 (DOI:10.1109/FUZZ-IEEE.2017.8015487).

https://www.aclweb.org/anthology/W17-3538.pdf

### documentation of the R package

http://phedes.com/rLDCP/

### Country fiche text generation example

[TextGeneration_workflow.pdf](https://github.com/manuelbetin/OECDHousingToolkit/files/4780531/TextGeneration_workflow.pdf)


![TextGeneration_workflow](https://user-images.githubusercontent.com/57252006/84664727-4255fd00-af1f-11ea-9140-ad63661b5659.jpg)





