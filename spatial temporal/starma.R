library(starma)
data(nb_mat) # Get neighbourhood matrices
# Simulate a STARMA model
eps <- matrix(rnorm(50*1080), 50, 1080)
sim <- eps
for (t in 3:50) {
  sim[t,] <- (.4*diag(94) + .25*blist[[2]]) %*% sim[t-1,] +
    (.25*diag(94) ) %*% sim[t-2,] +
    ( - .3*blist[[2]]) %*% eps[t-1,] +
    eps[t, ]
}
sim <- sim[101:200,]
sim <- stcenter(sim) # Center and scale the dataset
# Select parameters to estimate
ar <- matrix(0, 2, 2)
ar[ ,1] <- 1 # phi10 and phi20
ar[1,2] <- 1 # phi11
ma <- matrix(c(0,1), 1, 2) # theta11
# Run the Kalman filter algorithm
model <- starma(sim, blist, 3, 3)
# Get summary
summary(model)

model_mat <- expand.grid(ar = 0:5, ma = 0:5)[-1, ]
model_mat$bic <- NA
for (j in 1:5) {
  print (j)
  fit_model <- starma(sim, blist, model_mat$ar[j], model_mat$ma[j], iterate = 10)
  model_mat$[j] <- fit_model$bic
  #model_mat$bic[j] <- fit_model$bic
}
