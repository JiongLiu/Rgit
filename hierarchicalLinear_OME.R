library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dbin(phi[i], n[i])
    logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
  }

  b0 ~ dnorm(0.0, 1.0/5.0^2)
  for (j in 1:4) {
    b[j] ~ dnorm(0.0, 1.0/4.0^2)
  }

} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

mod1_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dbin(phi[i], n[i])
    logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] 
                      + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
  }
  
  for (j in 1:max(ID)){
    a[j] ~ dnorm(mu, prec_a)
  }

  mu ~ dnorm(0.0, 100.0)
  prec_a ~ dgamma(0.5, 2.0)
  tau = sqrt(1/prec_a)
  
  for (j in 1:4) {
    b[j] ~ dnorm(0.0, 1.0/4.0^2)
  }

} "
set.seed(117)

params = c("a", "b", "mu", "tau")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1, variable.names=params, n.iter=5e3)

mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## convergence diagnostic

plot(mod1_sim, ask=TRUE)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)

dic.samples(mod1, n.iter=1e3)

summary(mod1_sim)
