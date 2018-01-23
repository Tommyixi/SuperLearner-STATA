/*
The purpose of this file is to generate the optimal weights for the superlearner. 
There are many ways to optimize, though there's a useful method created in stanford
	out of the synth package (https://web.stanford.edu/~jhain/synthpage.html). 
	
The synth package was not as helpful, instead we can look at
https://www.stata.com/support/faqs/statistics/linear-regression-with-interval-constraints/#ex6
"How do I fit a linear regression with interval (inequality) constraints in Stata?"


************The  basic premise************

We have an equation:
Y = a1*x1 + a2*x2 + a3*x3 + epsilon

Where a1, a2, and a3 represent our estimates from the Z (prediction) matrix. Of course, 
we must extend this program to include any number of estimates and these coefficients
must add to 1.

First, we define our expression using a transformation of the mlogit command.

We can then set the restrictions on the model using the NL command (non linear least squares estimation)

Of course, we then then use the NLCOM (non linear combination of estimators) command to make weights sum to 1.
	
*/

capture program drop optimize_weights
program define optimize_weights, rclass



end


local ma2 (exp({t2})/(1+exp({t2})+exp({t3})))
local ma3 (exp({t3})/(1+exp({t2})+exp({t3})))
local ma1 (1/(1+exp({t2})+exp({t3})))
nl (mpg = `ma1'*custom_a + `ma2'*custom_b + `ma3'*mpgmean + {a4}), delta(1e-7) nolog


local na2 exp(_b[t2:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na3 exp(_b[t3:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na1 1/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
nlcom (a1: `na1') (a2: `na2') (a3: `na3')
