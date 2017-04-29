library(starma)

##
# startma example
##

data(nb_mat) # Get neighbourhood matrices
# Simulate a STARMA model
eps <- matrix(rnorm(94*200), 200, 94)
sim <- eps
for (t in 3:200) {
  sim[t,] <- (.4*diag(94) + .25*blist[[2]]) %*% sim[t-1,] +
    (.25*diag(94) ) %*% sim[t-2,] +
    ( - .3*blist[[2]]) %*% eps[t-1,] +
    eps[t, ]
}
sim <- sim[101:200,]
sim <- stcenter(sim) # Center and scale the dataset
# Autocorrelation functions
stacf(sim, blist)
stpacf(sim, blist)
# Select parameters to estimate
ar <- matrix(0, 2, 2)
ar[ ,1] <- 1 # phi10 and phi20
ar[1,2] <- 1 # phi11
ma <- matrix(c(0,1), 1, 2) # theta11
# Run the Kalman filter algorithm
model <- starma(sim, blist, ar, ma)


##
# my modification
##
model_df <- expand.grid(ar = 0:5, ma = 0:5)[-1, ]
model_df$bic <- NA
for (j in 1:nrow(model_df)) {
  print (j)
  model_df$bic[j] <- starma(sim, blist, model_df$ar[j], model_df$ma[j], iterate = 10)$bic
  #model_df$bic[j] <- fit_model$bic
}

starma_model <- starma(sim, blist, model_df$ar[which.min(model_df$bic)], model_df$ma[which.min(model_df$bic)], iterate = 5)
starma_model <- starma(sim, blist, model_df$ar[which.min(model_df$bic)], 3, iterate = 2)$bic
