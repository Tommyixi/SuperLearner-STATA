/*
This file will serve as the simulations for the superlearner program.

Goal here: Create "wild and crazy" simulations where the power of superlearner
can be viwed.
*/

/* 

Simulation 1 (borrowed from pg 52 in TMLE book).

y = 2.83 * sin ((pi/2) * X ) + U

*/

clear
set seed 123
set obs 200

generate x = runiform(-4,4)
generate u = rnormal(0,1)

generate y = 2.83 * sin((_pi/2) * x ) + u

scatter(y x) 

* model 1
regress y x
predict yhat1


* model 2
lassoregress y x
predict yhat2


* model 3
mkspline x1 -4 x2 -3 x3 -2 x4 1 x5 2 x6 3 x7 4 x8 = x
regress y x1 - x8
predict yhat3

* model 4 
generate a1 = _n - 10
generate a2 = a1^2
generate a3 = sqrt(abs(a1))
generate a4 = sin(a1)
generate a5 = a1^3
generate a6 = sin(_pi/3)
regress y a1-a6
predict yhat4

*model 5
mkspline b1 -4 b2 -2 b3 -0 b4 2 b5 4 b6 = x
regress y b1 - b6
predict yhat5


scatter y yhat1 yhat2 yhat3 yhat4 yhat5 x || lowess y x

* Superlearner prediction
global custom_a = "regress y x1 - x8"
global custom_b = "regress y a1-a6"

superlearner y x,  k(10) family("gaussian") library("custom_a custom_b regress ridgeregress") superestname("simpred")



/* 

Simulation 2 (borrowed from pg 52 in TMLE book).

y = 2.83 * sin ((pi/2) * X ) + U

*/

clear
set seed 123
set obs 200

generate x = runiform(-4,4)
generate u = rnormal(0,1)

generate y = 6 + .4* x - (.36*x^2) + (.005*x^3) + u

scatter(y x) 

* model 1
regress y x
predict yhat1


* model 2
lassoregress y x
predict yhat2


* model 3
mkspline x1 -4 x2 -3 x3 -2 x4 1 x5 2 x6 3 x7 4 x8 = x
regress y x1 - x8
predict yhat3

* model 4 
generate a1 = _n - 10
generate a2 = a1^2
generate a3 = sqrt(abs(a1))
generate a4 = sin(a1)
generate a5 = a1^3
generate a6 = sin(_pi/3)
regress y a1-a6
predict yhat4

*model 5
mkspline b1 -4 b2 -2 b3 -0 b4 2 b5 4 b6 = x
regress y b1 - b6
predict yhat5


scatter y yhat1 yhat2 yhat3 yhat4 yhat5 x || lowess y x, msize(huge)

* Superlearner prediction
global custom_a = "regress y x1 - x8"
global custom_b = "regress y a1-a6"

superlearner y x,  k(10) family("gaussian") library("custom_a custom_b regress ridgeregress") superestname("simpred2")

/* 

Simulation 3 (borrowed from pg 52 in TMLE book).

*/

clear
set seed 123
set obs 200

generate x = runiform(-4,4)
generate x_ind = 0
replace x_ind = 1 if x > 0
generate u = rnormal(0,1)

generate y = 4 * sin(1.5 * _pi * x) * x_ind + u
generate y_truth = 4 * sin(1.5 * _pi * x) * x_ind

scatter(y x) 

* model 1
regress y x
predict yhat1


* model 2
lassoregress y x
predict yhat2


* model 3
mkspline x1 -4 x2 -3 x3 -2 x4 1 x5 2 x6 3 x7 4 x8 = x
regress y x1 - x8
predict yhat3

* model 4 
generate a1 = _n - 10
generate a2 = a1^2
generate a3 = sqrt(abs(a1))
generate a4 = sin(a1)
generate a5 = a1^3
generate a6 = sin(_pi/3)
regress y a1-a6
predict yhat4

*model 5
mkspline b1 -4 b2 -2 b3 -0 b4 2 b5 4 b6 = x
regress y b1 - b6
predict yhat5

*model 6
mkspline c1 -4 c2 -3.5 c3 -3 c4 -2.5 c5 -2 c6 -1.5 c7 -1 c8 -.5 c9 0 c10 .5 c11 1 c12 1.5 c13 2 c14 2.5 c15 3 c16 3.5 c17 4 c18 = x
regress y c1 - c18
predict yhat6


scatter y yhat1 yhat2 yhat3 yhat4 yhat5 yhat6 x || lowess y x, msize(huge)

* Superlearner prediction
global custom_a = "regress y x1 - x8"
global custom_b = "regress y a1-a6"
global custom_c = "regress y c1 - c18"

superlearner y x,  k(10) family("gaussian") library("custom_a custom_b custom_c regress ridgeregress") superestname("simpred3")




/* 

Simulation 4 

*/

clear
set seed 123
set obs 400

generate x = runiform(-4,4)
generate x_ind1 = 0
replace x_ind1 = 1 if x > -2 & x <= 0
generate x_ind2 = 0
replace x_ind2 = 1 if x > 2 
generate x_ind3 = 0
replace x_ind3 = 1 if x <= -2

generate u = rnormal(0,1)

generate y = ((2 * x) * x_ind3) + ((x^3) * x_ind1) + (-2*x * x_ind2) + u
label variable y "Simulated Data"

generate y_t = ((2 * x) * x_ind3) + ((x^3) * x_ind1) + (-2*x * x_ind2)
label variable y_t "True Function"


*model 2
mkspline a1 -4 a2 0 a3 4 a4 = x
regress y a1-a4
predict yhat1
label variable yhat1 "Spline 1"


*model5
mkspline b1 -1 b2 2 b3 = x
regress y b1-b3
predict yhat2
label variable yhat2 "Spline 2"

*model 4
mkspline c1 1 c2 2 c3 = x
regress y c1-c3
predict yhat3
label variable yhat3 "Spline 3"

*model 5
mkspline d1 -4 d2 -3 d3 -2 d4 -1 d5 0 d6 1 d7 2 d8 3 d9 4 d10  = x
regress y d1-d10
predict yhat4
label variable yhat4 "Spline 4"

scatter y x, msize("tiny") || line y_t x, lwidth("thick") sort title("Simulation 1") ytitle("Y") xtitle("X")



scatter yhat1 yhat2 yhat3 yhat4 x || connected y_t x, sort

* Superlearner prediction
global custom_a = "regress y a1-a4"
global custom_b = "regress y b1-b3"
global custom_c = "regress y c1-c3"

superlearner y x,  k(10) family("gaussian") library("custom_a custom_b custom_c regress") superestname("simpred3")

label variable y_hat "SL Predictions"
scatter y  x, msize("tiny") || line y_t x, lpattern("dash") lwidth("thick") sort title("Simulation 1 - Superlearner Results") ytitle("Y") xtitle("X") ||  line y_hat x, lwidth("thick") sort  


