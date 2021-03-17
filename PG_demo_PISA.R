source("PG.R")
### demo
dat = read.table("PISA2009Germany.txt")
head(dat)



### Section 1: CCMV by default
dat_pg = PG(dat_miss = dat[,c("FA","MA")], dat_cov =  dat$Math)
dat_pg

names(dat_pg)

## fit the logistic regressions
fit_pg = PGfit(dat_pg)
fit_pg

names(fit_pg)

fit_pg$model.par
  # logistic regression parameters for selection odds 

fit_lm = lm(dat_cov~FA+MA, data = fit_pg$comp.data, weights = fit_pg$weight)
summary(fit_lm)


## stratified by different missing pattern
w_hh = which(fit_pg$comp.data$FA==1&fit_pg$comp.data$MA==1)
w_lh = which(fit_pg$comp.data$FA==0&fit_pg$comp.data$MA==1)
w_hl = which(fit_pg$comp.data$FA==1&fit_pg$comp.data$MA==0)
w_ll = which(fit_pg$comp.data$FA==0&fit_pg$comp.data$MA==0)


sum(fit_pg$weight[w_hh]*fit_pg$comp.data[w_hh,4])/sum(fit_pg$weight[w_hh])
sum(fit_pg$weight[w_lh]*fit_pg$comp.data[w_lh,4])/sum(fit_pg$weight[w_lh])
sum(fit_pg$weight[w_hl]*fit_pg$comp.data[w_hl,4])/sum(fit_pg$weight[w_hl])
sum(fit_pg$weight[w_ll]*fit_pg$comp.data[w_ll,4])/sum(fit_pg$weight[w_ll])


### Section 2: change the pattern graph
dat_pg2 = dat_pg

dat_pg2$parent[[2]] = c(1,3,4)

fit_pg2 = PGfit(dat_pg2)
fit_pg2

fit_lm2 = lm(dat_cov~FA+MA, data = fit_pg2$comp.data, weights = fit_pg2$weight)
summary(fit_lm2)


### Section 3: change the pattern graph again
dat_pg3 = dat_pg

dat_pg3$parent[[2]] = c(1,3)

fit_pg3 = PGfit(dat_pg3)
fit_pg3

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





