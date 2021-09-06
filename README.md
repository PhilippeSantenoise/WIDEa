# WIDEa (Web Interface for Data Exploration)
WIDEa is R-based software aiming to provide users with a range of  functionalities to explore, manage, clean and analyse "big" environmental and (in/ex situ) experimental data. These functionalities are the following, 
1. Loading/reading different data types: basic (called normal), temporal, infrared spectra of mid/near region (called IR) with frequency (wavenumber) used as unit (in cm-1);
2. Interactive data visualization from a multitude of graph representations: 2D/3D scatter-plot, box-plot, hist-plot, bar-plot, correlation matrix;
3. Manipulation of variables: concatenation of qualitative variables, transformation of quantitative variables by generic functions in R;
4. Application of mathematical/statistical methods; 
5. Creation/management of data (named flag data) considered as atypical; 
6. Study of normal distribution model results for different strategies: calibration (checking assumptions on residuals), validation (comparison between measured and fitted values). The model form can be more or less complex: mixed effects, main/interaction effects, weighted residuals. 

WIDEa used functions of several R packages: shiny, shinyBS, shinyjs, shinythemes, shinybusy, V8, plotly, htmltools, htmlwidgets, bindrcpp, scales, data.table, arrangements, car.
