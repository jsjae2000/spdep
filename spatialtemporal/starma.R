library(starma)

##
# starma example ----------------------------------------------------------
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
# forecasting functions ----------------------------------------------------------
##
fcst_starma <- function(model, data, wlist) {
  pred <- fcst_starma_component(model, data, wlist, 'phi') + fcst_starma_component(model, data, wlist, 'theta')
  return(as.numeric(pred))
}

fcst_starma_component <- function(model, data, wlist, component) {
  T <- nrow(data) # temporal  개수
  S <- ncol(data) # spatial point 수
  pred <- matrix(0, nrow = S, ncol = 1) # 예측값 저장 matrix
  
  n_t_lag <- nrow(model[[component]]) # 해당 component의 최대 lag (p or q in package intro notation)
  # spatial weight matrix의 최대 order 
  # (package intro notation 상으로는 0 부터 이므로 실제 최대 order는 n_sp_order - 1)
  n_sp_order <- length(wlist) 
  
  if (n_t_lag == 0) return(pred) # 이 경우 해당 component(ar or ma)가 없으므로 0 matrix를 return
  
  if (component == 'theta') data <- model[['residuals']] # component가 theta인 경우 residual에 대해 아래 연산 수행
  # n_t_lag != 0 즉, 해당 component(ar or ma)가 있으면 pred를 아래와 같이 update
  for (i_sp_order in 1:n_sp_order) {
    W <- blist[[i_sp_order]]
    pred <- pred + W %*% t(as.matrix(data[c(T:(T-n_t_lag+1)), , drop = F])) %*% as.matrix(model[[component]][, i_sp_order])
  }
  
  return(pred)
}

# function test
data <- sim[1:99, ]
model <- starma(sim[1:99, ], blist, 0, 3, iterate = 10)
wlist <- blist
component <- 'phi'
component <- 'theta'


##
# my modification - tunning ar, ma component ----------------------------------------------------------
##
max_ar <- 5
max_ma <- 5
n_iter <- 10
model_df <- expand.grid(ar = 0:max_ar, ma = 0:max_ma)[-1, ]
model_df$bic <- NA
for (j in 1:nrow(model_df)) {
  print (j)
  model_df$bic[j] <- starma(sim, blist, model_df$ar[j], model_df$ma[j], iterate = n_iter)$bic
  #model_df$bic[j] <- fit_model$bic
}

opt_ar <- model_df$ar[which.min(model_df$bic)]
opt_ma <- model_df$ma[which.min(model_df$bic)]
starma_model <- starma(sim, blist, opt_ar, opt_ma, iterate = n_iter)

## 
# check forecasting
##
# 예측 시간
model <- starma(sim[1:100, ], blist, 10, 10, iterate = 2)
tt <- proc.time()
pred1 <- fcst_starma(model, sim[1:99, ], blist)
proc.time() - tt # 예측 시간 = 0

# 예측 성능
plot(sim[100, ], pred1)
cor(sim[100, ], pred1)

# residual과 비교
pkg_res <- model$resi[100, ]
my_res <- sim[100, ] - pred1

print(all.equal(pkg_res, my_res))
print(sum(abs(pkg_res - my_res)))




