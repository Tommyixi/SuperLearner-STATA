* Code for the actual data simulation


clear
use train
set seed 243

global custom_a = "regress saleprice grlivarea fullbath bedroomabvgr fullbath#bedroomabvgr  kitchenabvgr#bedroomabvgr fireplaces garagecars#bedroomabvgr yrsold  homeage  yrssinceremod"
global custom_b = "regress saleprice familysale"

superlearner saleprice grlivarea fullbath bedroomabvgr kitchenabvgr fireplaces garagecars yrsold singlefamily centralair homeage averagecondition yrssinceremod familysale,  k(10) family("gaussian") library("lassoregress regress custom_a custom_b ridgeregress ") superpredname("predictions") superestname("predictor") evalmetric("rmse") newdata("test.dta") originaldataset("train.dta") libraryglobals("library.do")
