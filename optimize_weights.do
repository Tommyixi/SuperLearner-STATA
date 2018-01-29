/*
The purpose of this file is to generate the optimal weights for the superlearner. 
There are many ways to optimize, though there's a useful method created in stanford
	out of the synth package (https://web.stanford.edu/~jhain/synthpage.html). 
	
The synth package was not as helpful, instead we can look at
https://www.stata.com/support/faqs/statistics/linear-regression-with-interval-constraints/#ex6
"How do I fit a linear regression with interval (inequality) constraints in Stata?"

Another site I found relevant
https://www.stata.com/support/faqs/statistics/regression-with-interval-constraints/#application


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

	syntax varlist(max=1), [if] [in] predictors(string)
	
	local count = wordcount("`predictors'")
	
	if `count' ==1 {
		display "There is only one learner in your library"
		return 1
	}
	else{
		* First we need to build the denominator of our system of equations
		local counter = 2
		local loop = "TRUE"
		local denom = "(1 "
		local denom2 = "(1 "
		while "`loop'" == "TRUE"{
			local denom = "`denom'" + " + exp({t`counter'})"
			local denom2 = "`denom2'" + " + exp(_b[t`counter':_cons])"
			local counter = `counter' + 1
			if `counter' > `count'{
				local loop = "FALSE"
				local denom = "`denom'" + ")"
				local denom2 = "`denom2'" + ")"
			}
			
		}
		
		* At this point, we should have a denominator that is the same across all equations
		* Now we can coninue on and build the equations
		* First we reset the counter
		local counter = 1
		while `counter' <= `count'{
			if `counter' == 1{
				local ma`counter' (1/(`denom'))
				local na`counter' (1/(`denom2'))
			}
			else{
				local ma`counter' (exp({t`counter'})/(`denom'))
				local na`counter' (exp(_b[t`counter':_cons])/(`denom2'))
			}
			local counter = `counter' + 1			
		}
		
		* The system of equations should be built. Now call the NL command.
		tokenize `predictors'
		local counter = 1
		local exp = "`varlist' = "
		local exp2 = ""
		while `counter' <= `count'{
			if `counter' == 1{
				local exp = "`exp'" + "`ma`counter''*``counter'' "
				local exp2 = "`exp2'" + "(``counter'': `na`counter'')"
			}
			else{
				local exp = "`exp'" + " + `ma`counter''*``counter'' " 
				local exp2 = "`exp2'" + "(``counter'': `na`counter'')"
			}
			local counter = `counter' + 1					
		}
		local exp = "`exp' + {a`counter'}"
		
		display "performing convex optimization"
		qui nl (`exp'), delta(1e-7) nolog
		
		*We can now build the code required to actually get the weights from the optimization
		display "Calculating the weights for each algorith"
		nlcom `exp2', post
		
		*Next, return the coefficients and pass them back to the SL function.
		return list
		matrix list r(b)
		
		
	}
end

gen mpgmean = r(mean)

local ma3 (exp({t3})/(1+exp({t2})+exp({t3})))
local ma2 (exp({t2})/(1+exp({t2})+exp({t3})))
local ma1 (1/(1+exp({t2})+exp({t3})))
nl (mpg = `ma1'*custom_a + `ma2'*custom_b + `ma3'*mpgmean + {a4}), delta(1e-7) nolog


local na2 exp(_b[t2:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na3 exp(_b[t3:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na1 1/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
nlcom (custom_a: `na1') (custom_b: `na2') (average: `na3')





/*
Here's an example using four equations
*/

local ma2 (exp({t2})/(1+exp({t2})+exp({t3}) + exp({t4})))
local ma3 (exp({t3})/(1+exp({t2})+exp({t3}) + exp({t4})))
local ma1 (1/(1+exp({t2})+exp({t3}) + exp({t4})))
local ma4 (exp({t4})/(1+exp({t2})+exp({t3}) + exp({t4})))
nl (mpg = `ma1'*custom_a + `ma2'*custom_b + `ma3'*custom_c + `ma4'*custom_d + {a5}), delta(1e-7) nolog


local na2 exp(_b[t2:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]) + exp(_b[t4:_cons]))
local na3 exp(_b[t3:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]) + exp(_b[t4:_cons]))
local na1 1/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]) + exp(_b[t4:_cons]))
local na4 exp(_b[t4:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]) + exp(_b[t4:_cons]))
nlcom (custom_a: `na1') (custom_b: `na2') (custom_c: `na3') (custom_d: `na4')




(a: (1/((1  + exp(_b[t2:_cons]) + exp(_b[t3:_cons]) + exp(_b[t4:_cons])))))(b: (exp(_b[t2
> :_cons])/((1  + exp(_b[t2:_cons]) + exp(_b[t3:_cons]) + exp(_b[t4:_cons])))))(b: (exp(_
> b[t3:_cons])/((1  + exp(_b[t2:_cons]) + exp(_b[t3:_cons]) + exp(_b[t4:_cons])))))(b: (e
> xp(_b[t4:_cons])/((1  + exp(_b[t2:_cons]) + exp(_b[t3:_cons]) + exp(_b[t4:_cons])))))

