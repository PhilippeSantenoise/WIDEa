<p align="center"><img src="https://github.com/PhilippeSantenoise/WIDEa/inst/www/WIDEa_header_img.png" width="350" height="90"></p>
<h1 align="center">WIDEa<br>(Web Interface for Data Exploration)</h1><br />

## Description
WIDEa is R-based software aiming to provide users with a range of  functionalities to explore, manage, clean and analyse "big" environmental and (in/ex situ) experimental data. These functionalities are the following, 
1. Loading/reading different data types: basic (called normal), temporal, infrared spectra of mid/near region (called IR) with frequency (wavenumber) used as unit (in cm-1);
2. Creation of subsets of data by including several conditions;
3. Interactive data visualization from a multitude of graph representations: 2D/3D scatter-plot, box-plot, hist-plot, bar-plot, correlation matrix;
4. Manipulation of variables: concatenation of qualitative variables, transformation of quantitative variables by generic functions in R;
5. Application of statistical methods (linear regression, density curve, hypothesis testing, etc.); 
6. Creation/management of data (named flag data) considered as atypical; 
7. Study of normal distribution model results for different strategies: calibration (checking assumptions on residuals), validation (comparison between measured and fitted values). The model form can be more or less complex: mixed effects, main/interaction effects, weighted residuals. 

## Installation and usage
A detailed description about how to install and use WIDEa is available from the pdf file named [WIDEa_manual](https://github.com/PhilippeSantenoise/WIDEa/blob/main/WIDEa_manual.pdf).

## R version and packages
A R version 3.5 or greater is required before installing WIDEa ([R link](https://cran.r-project.org/bin/)).<br />
WIDEa used functions of several R packages: shiny, shinyBS, shinyjs, shinythemes, shinybusy, V8, plotly, htmltools, htmlwidgets, bindrcpp, scales, data.table, arrangements, car, DT, colourpicker.

## License
Copyright (C) 2021 - INRAE - Philippe Santenoise<br />
WIDEa is an Open Source software distributed under the [GNU General Public License (GPL), Version 3.0](https://github.com/PhilippeSantenoise/WIDEa/blob/main/LICENSE).

## Citation
Santenoise, P. WIDEa: a Web Interface for big Data Exploration, management and analysis. Portail Data INRAE, 2021. DOI: [10.15454/AGU4QE](https://doi.org/10.15454/AGU4QE)

## Application
Some data are provided ([Data folder](https://github.com/PhilippeSantenoise/WIDEa/tree/main/Data)) on the repository to help users to test all functionnalities of WIDEa (several examples are given from the [WIDEa_manual](https://github.com/PhilippeSantenoise/WIDEa/blob/main/WIDEa_manual.pdf) file).
