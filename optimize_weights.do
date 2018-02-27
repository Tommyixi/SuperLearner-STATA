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

	syntax varlist(max=1), [if] [in] predictors(string) indvars(string) library(string) [vars(string)] [k(numlist min=1 max=1)] [evalmetric(string)] [originaldataset(string)][superpredname(string)] [superestname(string)] [newdata(string)] [libraryglobals(string)] [loud]
	
	*by default we don't display each model as it is fit.
	if "`loud'" == "" {
		local qui = "qui"
	}
	
	display "********Generating Predictions on Current Dataset Stored in y_hat********"
	tokenize `library'
	local j = 1
	while "`1'" != "" {
		local test_custom = usubstr("``j''",1 ,6)
		if "`test_custom'" == "custom"{
			`qui' $``j'' `weight', `options'
			predict ``j''
		}
		else{
			`qui' ``j'' `vars' `weight' , `options'
			predict ``j''
		}	
		macro shift
	}

	
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
		
		`qui' nl (`exp'), nolog
		
		
		*We can now build the code required to actually get the weights from the optimization
		display "********Calculating the weights for each algorithm********"
		`qui' nlcom `exp2' //, post
		
		
		*Small loop to round our coefficients
		mat b = r(b)
		local colnamesnames : colnames r(b)
		
		local cols = colsof(b)
		matrix C = J(1,`cols',0)
		forvalues i = 1/`cols' {
			matrix C[1,`i']= round(b[1,`i'], .000001)
		}
		
		matrix colnames C = `colnamesnames'
		mat list C		
		
		`qui' predict y_hat
		local j = 1
		while "`1'" != "" {
			drop ``j''
			macro shift
		}		
		
		
		display "********Predictions Generated********"
		
		* Save the estimated model so we can reuse if need be for out of sample predictions
		
		estimates save "`superestname'", replace
		
		display "********Estimation Saved********"
		
		* We have to save the dataset with the predictions
		local predfile = "`superpredname'" + ".dta"
		save "`predfile'", replace
		
		* We should run a cross validated superlearner as well. How does our superlearner hold up against the other predictors?
		display "********Performing Cross Validated SuperLearner********"
		* This needs to be worked on...
		*cross_validate `library', vars(`varlist') k(`k') evalmetric(`evalmetric') superlearner("`superestname'")
		
		
		* If the user wants to make an out of sample prediction, do the following:
		if "`newdata'" != "" {
		
			* Clear out the original dataset and reload.
			clear 
			sysuse `originaldataset'
			
		
			
			*clear the dataset, run predict on the new dataset, save the new dataset, close it, open the original dataset and repeat.
			tokenize `predictors'
			local counter = 1
			local j = 1
			
			
			* Reload all the associated models, if there are any...
			if "`libraryglobals'" != "" {
				do `libraryglobals'
			}			
			
			
			while "`1'" != "" {
				* For each model in the library, we have to load the original dataset
				sysuse `originaldataset'
				
				* Run the model (check if it's a custom method)
				local test_custom = usubstr("``j''",1 ,6)
				if "`test_custom'" == "custom"{
					do `libraryglobals'
					`qui' $``j'' 
				}
				else{
					`qui' ``j'' `varlist' `indvars' 
				}
				* Save estimates
				local estimatemodel = "estimates" + "`counter'"
				estimates save "`estimatemodel'", replace
				
				* Clear the dataset, load the original, run prediction, save dataset
				clear 
				sysuse `newdata'
				
				estimates use `estimatemodel'
				`qui' predict ``j''
				save `newdata', replace
				
				local counter = `counter' + 1
				macro shift
			}
			estimates use `superestname' 
			predict superlearner_prediction
			
		}
		
		
		
		
	}
end


/* Example using three equations
gen mpgmean = r(mean)

local ma3 (exp({t3})/(1+exp({t2})+exp({t3})))
local ma2 (exp({t2})/(1+exp({t2})+exp({t3})))
local ma1 (1/(1+exp({t2})+exp({t3})))
nl (mpg = `ma1'*custom_a + `ma2'*custom_b + `ma3'*custom_c + {a4}), delta(1e-7) nolog


local na2 exp(_b[t2:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na3 exp(_b[t3:_cons])/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
local na1 1/(1+exp(_b[t2:_cons])+exp(_b[t3:_cons]))
nlcom (custom_a: `na1') (custom_b: `na2') (custom_c: `na3')
*/




/*
Here's an example using four equations


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

*/
