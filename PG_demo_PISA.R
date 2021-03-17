source("PG.R")
### demo
dat = read.table("PISA2009Germany.txt")
head(dat)


### Section 1: CCMV by default
dat_pg = PG(dat_miss = dat[,c("FA","MA")], dat_cov =  dat$Math)
dat_pg
# The response pattern indicates that pattern 1 = 01 (FA missing), pattern 2 = 00 (FA, MA missing),
# pattern 3 = 10 (MA missing), pattern 4 = 11 (complete case).

names(dat_pg)

head(dat_pg$labels.pattern)
# the number 1 2 2 3 2 2 means that first observation has a response pattern 1 
# (01, i.e.,FA missing), and the second observation has a pattern 2 (00, both FA, MA are missing).

## fit the logistic regressions
fit_pg = PGfit(dat_pg)
fit_pg

names(fit_pg)

fit_pg$paths
  # paths from the complete case 11 to others

fit_pg$model.par
  # logistic regression parameters for selection odds 

head(fit_pg$comp.data)
  # weights and information of the complete cases

fit_lm = lm(dat_cov~FA+MA, data = fit_pg$comp.data, weights = fit_pg$weight)
summary(fit_lm)


### Section 2: change the pattern graph
dat_pg2 = dat_pg

dat_pg

dat_pg2$parent[[2]] = c(1,3,4)
# From dat_pg, pattern 1 = 01 (FA missing), pattern 2 = 00 (FA, MA missing),
# pattern 3 = 10 (MA missing), pattern 4 = 11 (complete case).
# Thus, this command set the paterns of 00 to be 01, 10, 11.

dat_pg2
  # the resulting pattern graph

fit_pg2 = PGfit(dat_pg2)
fit_pg2

fit_pg2$paths
  # we now have more paths

fit_lm2 = lm(dat_cov~FA+MA, data = fit_pg2$comp.data, weights = fit_pg2$weight)
summary(fit_lm2)


### Section 3: change the pattern graph again
dat_pg3 = dat_pg

dat_pg3$parent[[2]] = c(1,3)

dat_pg3
  # a new graph

fit_pg3 = PGfit(dat_pg3)
fit_pg3

fit_pg3$paths
  # paths of this new graph

fit_lm3 = lm(dat_cov~FA+MA, data = fit_pg3$comp.data, weights = fit_pg3$weight)
summary(fit_lm3)


### Section 4: fitting two or more covariates
dat_pg = PG(dat_miss = dat[,c("FA","MA")], dat_cov =  dat[,c("Math","SCIE")])
dat_pg

names(dat_pg)

## fit the logistic regressions
fit_pg = PGfit(dat_pg)
fit_pg

fit_pg$model.par
  # logistic regression parameters for selection odds

fit_lm_new = lm(Math~FA+MA, data = fit_pg$comp.data, weights = fit_pg$weight)
summary(fit_lm_new)





