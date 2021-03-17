# Pattern Graph

An R script for applying pattern graph approach for handling nonmonotone missing data with the inverse probability weight (IPW) procedure.
- Paper reference: Chen, Y. C. (2020). Pattern graphs: a graphical approach to nonmonotone missing data. arXiv preprint arXiv:2004.00744.
- Contact: yenchic@uw.edu

## PG.R

The main R script containing functions to create a pattern graph object and estimate the weights under the pattern graph. The weight is estimated based on a simple logistic regression model.

### PG
`PG = function(dat_miss, dat_cov=NULL)`

- Input:
  - dat_miss: The primary data matrix that may contain missingness (does not include the covariates).
  - dat_cov: The data matrix of the covariates that we wish to adjust, cannot contain missingness.

- Output:
  - dat_miss: The input missing data.
  - dat_cov: The input covariates.
  - info: A list of information on the patterns of each observation.
  - summary: The summary of frequency of each pattern and the corresponding pattern index.
  - labels.pattern: The response pattern label of each observation.


### PGfit
`PGfit = function(PG)`

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


  


