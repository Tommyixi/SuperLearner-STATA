/*
This file will serve as a simulation for the superlearner program.

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

superlearner y x,  k(10) family("gaussian") library("custom_a custom_b custom_c regress ridgeregress") superestname("simpred2")


