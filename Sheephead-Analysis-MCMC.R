#1. Does Marine Protected Areas influence the size and frequency of fish?
#2. Data was collected from PISCO, with 20 years of monitoring of over 100 sites across California.
#3. The fish used was the game fish "Sheephead", popular with anglers

data = MLPA_kelpforest_fish.final.5
data = data[data$classcode == "SPUL" , ]
data = data[data$reference == "mpa" | data$reference == "reference", ]
data$year = data$year - 1998
library("rjags")

mod_glm = glm(fish_tl ~ reference + count + year + reference*count + reference * year
              + count*year + reference * count * year, data=data, family="gaussian")
#TL ~ a(i) + b(1)*reference(i) + b(2)*count(i) + b(3)*reference*count
#using noninformative priors
X = model.matrix(~ reference + year + fish_tl + count, data=data)
mod_string = " model {
    for( i in 1:2) {
        y[i] ~ dnorm(mu[i], prec[i])
        mu[i] = alpha[i] + beta[1]*reference[i] + beta[2]*count[i]  + 
        beta[3]*reference[i]*count[i]
    }
    
    for (i in 1:2)
    { 
      alpha[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    for (j in 1:3) {
        beta[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    for (i in 1:2)
      {
        prec[i] ~ dgamma(3/2.0, 3*1.0/2.0)  
      }
    for (i in 1:2){
      sig[i] = sqrt(1.0 / prec[i])
    }
    
} "

data_jags = list(reference=X[,"referencereference"],y=X[,"fish_tl"], count=X[,"count"])
params = c("alpha", "beta", "sig", "mu")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e4)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergene diagnostics
plot(mod_sim, ask=TRUE)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)
dic = dic.samples(mod, n.iter=1e4)
(pm_coef <- colMeans(mod_csim)) 
boxplot(fish_tl ~ reference, data = data)
