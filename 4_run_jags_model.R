library(rjags)
library(readr)

#For setting initial values 
load.module("lecuyer")
inits <- replicate(2,
                   list(.RNG.name="lecuyer::RngStream",
                        .RNG.seed=sample(65535,1)),
                   simplify=FALSE)

model <- read_file("Code/3_jags_model_mrp.jags") 

model_data <- readRDS("Data/model_data.rds")
model_data$y <- as.integer(model_data$y) - 1
mrp.file <- tempfile("mrp", fileext=".bug")
cat(model, file=mrp.file)
mrp.jags <- jags.model(mrp.file, data=model_data, inits=inits, n.chains=length(inits), n.adapt=1000)


update(mrp.jags, n.iter=10000)

samples <- jags.samples(mrp.jags, c("predict"), n.iter=20000, thin=10)

saveRDS(samples, "Data/mcmc_samples.rds")

mrp.prediction <- data.frame(state.abb = levels(model_data$state)[],
                             opinion = apply(samples$predict,1,mean),
                             stringsAsFactors=FALSE)

saveRDS(mrp.prediction, "Data/mrp_pred.rds")


