library(mgcv)
data(trees)
str(trees)

with(trees,
     y <<- Girth*1 + Height*2 + Volume*3)
gam(y~s(Girth, Height, Volume, bs = c('tp', 'tp', 'tp')), data = trees)
gam(y~s(Girth, bs = c('tp')), data = trees)
