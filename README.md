# Pattern Graph

An R script for applying pattern graph approach for handling nonmonotone missing data with the inverse probability weight (IPW) procedure.
- Paper reference: Chen, Y. C. (2020). Pattern graphs: a graphical approach to nonmonotone missing data. arXiv preprint arXiv:2004.00744.
- Contact: yenchic@uw.edu

## PG.R

The main R script containing functions to create a pattern graph object and estimate the weights under the pattern graph. The weight is estimated based on a simple logistic regression model.

### PG
`PG = function(dat_miss, dat_cov=NULL)`

The R script for creating a pattern graph object. The default pattern graph will be the graph corresponding to the complete-case missing value (CCMV) restriction. You can easily modify the pattern graph by changing `parent` in the output. See Section 2 and 3 in `PG_demo_PISA.R`.

- Input:
  - dat_miss: The primary data matrix that may contain missingness (does not include the covariates).
  - dat_cov: The data matrix of the covariates that we wish to adjust, cannot contain missingness.

- Output:
  - dat_miss: The input missing data.
  - dat_cov: The input covariates.
  - info: A list of information on the patterns of each observation.
  - present: The parent pattern of each response pattern.
  - summary: The summary of frequency of each pattern and the corresponding pattern index.
  - labels.pattern: The response pattern label of each observation.


### PGfit
`PGfit = function(PG)`

The function to fit a logistic regresison model of the odds in the pattern graph.

- Input: 
  - PG: The input PG object, created from the `PG` function

- Output:
  - igraph: The igraph object of the pattern graph.
  - igraph_namde: The igraph object of the pattern graph with vertex named after patterns.
  - paths: All paths in the pattern graph.
  - model.par: The fitted logistic parameter at each pattern.
  - comp.idx: The index of complete observations.
  - comp.odds: A matrix of the selection odds of complete cases evaluated at each pattern. Each row is a complete case and each column represents a response pattern.
  - comp.data: The complete case data along with the corresponding weights.
  - weight: The weight of each complete case.



## PG_demo_PISA.R:

An R script for demonstrating how to use the script `PG.R`. It fits a couple of pattern graphs to the PISA data (PISA2009Germany.txt).
- Section 1: is a simple analysis using CCMV (without modifying the pattern graph) based on a single covariate (math).
- Section 2&3: modifying the pattern graphs by changing the parent of a pattern.
- Section 4: applying to cases with two covariates.

## PISA2009Germany.txt

The PISA data at year 2009 of Germany students. It is modified from https://www.oecd.org/pisa/data/pisa2009database-downloadabledata.htm. The preprocessing procedure is described in Appendix C of https://arxiv.org/abs/2004.00744. 

