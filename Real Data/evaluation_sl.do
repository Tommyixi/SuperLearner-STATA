* Code for the actual data simulation

clear
cd "/Users/Tommy/Documents/Berkeley/Thesis research/Real Data"
set seed 1234
use https://stats.idre.ucla.edu/stat/stata/dae/binary.dta, clear
gen int admit2 = floor(admit)
drop admit
gen int admit = admit2
drop admit2

* Note, gen salepriceadj = saleprice / 10000

global custom_a = "regress gpa admit gre i.rank"
global custom_b = "regress gpa admit gre"
global custom_c = "regress gpa gre#admit"

superlearner gpa gre admit rank,  k(10) family("gaussian") library("custom_a custom_b custom_c  lassoregress") superestname("predictions")





clear
use train_data
set seed 243

global custom_a = "regress saleprice grlivarea fullbath bedroomabvgr fullbath#bedroomabvgr  kitchenabvgr#bedroomabvgr fireplaces garagecars#bedroomabvgr yrsold  homeage  yrssinceremod"
global custom_b = "regress saleprice familysale"

superlearner saleprice grlivarea fullbath bedroomabvgr kitchenabvgr fireplaces garagecars yrsold singlefamily centralair homeage averagecondition yrssinceremod familysale,  k(10) family("gaussian") library("lassoregress regress custom_a custom_b ridgeregress ") superpredname("predictions") superestname("predictor") evalmetric("rmse") newdata("test_data.dta") originaldataset("train_data.dta") libraryglobals("library.do")
