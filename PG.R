# Author: Yen-Chi Chen
# Maintainer: Yen-Chi Chen <yenchic@uw.edu>
# Reference: Pattern Graphs: a Graphical Approach to Nonmonotone Missing Data
# arXiv: https://arxiv.org/abs/2004.00744
# Date: 03/15/2021

### The R script for applying pattern graph based on IPW approach.
#' @import mgcv
library(mgcv)
#' @import igraph
library(igraph)

#' Creating a pattern graph object.
#' @param dat_miss The data matrix that may contain missingness.
#' @param dat_cov The covariates we wish to adjust, cannot contain missingness.
#' @export
PG = function(dat_miss, dat_cov=NULL) UseMethod("PG")

#' Creating a pattern graph object.
#' @param dat_miss The data matrix that may contain missingness.
#' @param dat_cov The covariates we wish to adjust, cannot contain missingness.
#' @return A S4 object of class "PG". A list consisting
#' \item{dat_miss} 
#' The input missing data.
#' \item{dat_cov}
#' The input covariates.
#' \item{info}
#' A list of information on the patterns of each observation.
#' \item{summary}
#' The summary of frequency of each pattern and the corresponding pattern index.
#' \item{labels.pattern}
#' The response pattern labe of each observation.
#' @export
PG.default = function(dat_miss, dat_cov=NULL){
  pg_list = list()
  pg_list[["dat_miss"]] = data.frame(dat_miss)
  pg_list[["dat_cov"]] = data.frame(dat_cov)
  
  miss_pat = uniquecombs(!is.na(dat_miss)+0)
  # finding all response patterns
  n_pat = nrow(miss_pat)
  # number of patterns
  row.names(miss_pat) = c(1:n_pat)
  
  
  pat_comp = which(apply(miss_pat, 1, prod)==1)
  names(pat_comp) = NULL
    # the index of complete case
  idx_comp = attr(miss_pat,"index")==pat_comp
    # index of complete cases
  n_comp = sum(idx_comp)
    # size of complete case
  
  pg_list[["info"]] = miss_pat+0
  for(j in 1:n_pat){
    pat = miss_pat[j,]
    if(sum(pat==c(1,1))<2){
      pg_list[["parent"]][[j]] = pat_comp
      # this is CCMV case
    }
  }
  
  ## summary
  info_tmp = pg_list$info
  pat_obs = attr(pg_list$info, "index")
  attr(info_tmp, "index") = NULL
  info_all = cbind(info_tmp, table(pat_obs))
  colnames(info_all) = c(colnames(info_tmp), "Cases")
  pg_list[["summary"]] = info_all
  pg_list[["labels.pattern"]] = pat_obs
  
  class(pg_list) = "PG"
  return(pg_list)
}
print.PG = function(x){
  n_pat = nrow(x$info)
  
  pat_comp = which(apply(x$info, 1, prod)==1)
  pat_idx = (1:n_pat)[-pat_comp]
  
  pat_names = rep(NA, n_pat)
  for(j in 1:n_pat){
    pat_names[j] = paste(x$info[j,]+0, collapse="")
  }
  
  rel_matrix_named = NULL
  for(j in pat_idx){
    rel_matrix_named = rbind(rel_matrix_named, 
                             cbind( pat_names[x$parent[[j]]], pat_names[j]))
  }
  pg_igraph_named = graph_from_edgelist(rel_matrix_named)
  
  cat("Response Patterns:\n")
  print(x$summary)
  cat("\n")
  plot(pg_igraph_named)
  print(pg_igraph_named)
}


#' Fitting the pattern graph based on IPW approach.
#' @param PG The input PG object.
#' @export
PGfit = function(PG, method="logistic") UseMethod("PGfit")

#' Fitting the pattern graph based on IPW approach. Currently, the propensity score
#' is based on logistic regression.
#' @param PG The input PG object, from the 'PG' function.
#' @return A S4 object of class "PGfit". A list consisting
#' \item{igraph} 
#' The igraph object of the pattern graph.
#' \item{igraph_named}
#' The igraph object of the pattern graph with vertex named after patterns.
#' \item{pths}
#' All paths in the pattern graph.
#' \item{model.par}
#' The fitted logistic parameter at each pattern.
#' \item{odds}
#' A matrix of the selection odds of complete cases evaluated at each pattern.
#' Each row is a complete case and each column represents a response pattern.
#' \item{complete.data}
#' The complete case data along with the corresponding weights.
#' \item{weights}
#' The weights of each complete case.
#' @export
PGfit.default = function(PG){
  miss_pat = PG[["info"]]+0
  dat_miss = PG[["dat_miss"]]
  dat_cov = PG[["dat_cov"]]
  
  ### create base-variables
  pg_fit = list()
  model_par = list()
  # parameter list
  
  n_pat = nrow(miss_pat)
    # total sample size
  pat_comp = which(apply(miss_pat, 1, prod)==1)
  names(pat_comp) = NULL
    # the index of complete case
  idx_comp = attr(miss_pat,"index")==pat_comp
    # index of complete cases
  n_comp = sum(idx_comp)
    # number of complete case
  pat_idx = (1:n_pat)[-pat_comp]
    # all pattern index except complete case
  odds_complete = matrix(1, nrow= n_comp, ncol=n_pat)
    # odds evaluated on complete case
  pat_names = rep(NA, n_pat)
  for(j in 1:n_pat){
    pat_names[j] = paste(miss_pat[j,]+0, collapse="")
  }
    # names of patterns
  
  ### creating igraph object
  rel_matrix = NULL
  rel_matrix_named = NULL
  for(j in pat_idx){
    rel_matrix = rbind(rel_matrix, cbind( PG$parent[[j]], j))
    rel_matrix_named = rbind(rel_matrix_named, cbind( pat_names[PG$parent[[j]]], pat_names[j]))
  }
  pg_igraph = graph_from_edgelist(rel_matrix)
  pg_igraph_named = graph_from_edgelist(rel_matrix_named)
  
  plot(pg_igraph_named)
  pg_path = all_simple_paths(pg_igraph, from=4, mode="out")
  
  pg_fit[["igraph"]] = pg_igraph
  pg_fit[["igraph_named"]] = pg_igraph_named
  pg_fit[["paths"]] = all_simple_paths(pg_igraph_named, from=4, mode="out")
  
  ### fitting the model
  for(j in pat_idx){
    var_idx = as.logical(PG$info[j,])
    if(sum(var_idx)==0&is.null(dat_cov)){
      # totally missing case
      fit = sum(attr(miss_pat,"index")==j)/sum(is.element(attr(miss_pat,"index"),
                                                          PG[["parent"]][[j]]))
      model_par[[j]] = log(fit)
      odds_complete[,j]= fit
    } 
    #### with covariates: always have non-empty variables
    if(sum(var_idx)==0){
      w_cur = attr(miss_pat,"index")==j
      new.dat1 = cbind(1, dat_cov[w_cur,])
      # current
      w_pa = is.element(attr(miss_pat,"index"), PG[["parent"]][[j]])
      new.dat0 = cbind(0, dat_cov[w_pa,])
      # parent
      new.dat = rbind(as.matrix(new.dat1), as.matrix(new.dat0))
      colnames(new.dat) = c("lab", names(dat_cov))
      fit = glm(lab~., family= "binomial", data = data.frame(new.dat))
      model_par[[j]] = fit$coefficients
      
      dat_comp = as.matrix(cbind(1, dat_cov[idx_comp,]))
      odds_complete[,j]  = exp(dat_comp %*% fit$coefficients)
    } 
    
    if(sum(var_idx)>0){
      w_cur = attr(miss_pat,"index")==j
      new.dat1 = cbind(1,dat_miss[w_cur,var_idx], dat_cov[w_cur,])
      # current
      w_pa = is.element(attr(miss_pat,"index"), PG[["parent"]][[j]])
      new.dat0 = cbind(0,dat_miss[w_pa,var_idx], dat_cov[w_pa,])
      # parent
      new.dat = rbind(as.matrix(new.dat1), as.matrix(new.dat0))
      colnames(new.dat) = c("lab", names(dat_pg$dat_miss)[var_idx], 
                             names(dat_cov))
      fit = glm(lab~., family= "binomial", data = data.frame(new.dat))
      model_par[[j]] = fit$coefficients
      
      dat_comp = as.matrix(cbind(1,dat_miss[idx_comp,var_idx], dat_cov[idx_comp,]))
      
      odds_complete[,j]  = exp(dat_comp %*% fit$coefficients)
    }
  }
  colnames(odds_complete) = pat_names
  rownames(odds_complete) = rownames(dat_miss[idx_comp,])
  
  ### computing path odds
  path_odds = matrix(NA, nrow= n_comp,ncol= length(pg_path))
  for(k in 1:length(pg_path)){
    path_val =  rep(1, n_comp)
    for(in_path in 2:length(pg_path[[k]])){
      path_val = path_val*odds_complete[,pg_path[[k]][in_path]]
    }
    path_odds[,k] = path_val
  }
  weight_final = rowSums(path_odds)+1

  # complete data creation
  dat_comp = cbind(weight_final, dat_miss[idx_comp,], dat_cov[idx_comp,])
  names(dat_comp) = c("weight",names(dat_miss), names(dat_cov))
  if(is.null(names(dat_cov))){
    names(dat_comp) = c("weight",names(dat_miss),"covariate")
  }
  
  pg_fit[["model.par"]] = model_par
  
  pg_fit[["comp.idx"]] = which(idx_comp)
  pg_fit[["comp.odds"]] = odds_complete
  pg_fit[["comp.data"]] = dat_comp
  pg_fit[["weight"]] = weight_final
  
  class(pg_fit) = "PGfit"
  return(pg_fit) 
}
print.PGfit = function(x){
  print(x$igraph_named)
  plot(x$igraph_named)
  cat(paste("\n Size of complete cases: ",length(x$comp.idx)))
  cat("\n Complete cases info:\n")
  print(head(x$comp.data))
}


