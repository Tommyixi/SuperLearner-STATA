************************************************
************************************************
************************************************
**** This is the main superlearner program ****
************************************************
************************************************
************************************************

capture program drop superlearner
program define superlearner, eclass
	
	syntax varlist(min=2) , [if] [in] k(integer) family(string) library(string) [originaldataset(string)] [superpredname(string)] [superestname(string)] [newdata(string)] [libraryglobals(string)] [loud] [evalmetric(string)]
	

	
	/*
		varlist: 			Variables (both Y and X) that are used to fit models.
		k: 					Number of folds desired for the kfold cross validation
		family: 			Currently setup to only handle gaussian, eventually binomial.
		library: 			List of algorithms (both custom and built in) to comprise of the final superlearner algorithm
		originaldataset: 	The name of the original dataset you are using for the superlearner fit. Training set may be a more apropriate name.
		superpredname: 		A user supplied name for the dataset containing the superlearner predictions. 
		superestname: 		A user supplied name for the saving of the estimated model of the superlearner.
		newdata: 			If the user would like to make predictions on a new dataset (with the same variables) they can supply that here.
		libraryglobals: 	This parameter would only be used if the user created a custom library for which they wanted to make predictions.
		loud: 				Should the output from each command be displayed?
		evalmetric:			How should the cross validation be executed (mae, pseudo rsquared, auc, rmse(default))
	*/
	
	* Parameter checks!	   

	cross_validate `library', vars(`varlist') k(`k') evalmetric(`evalmetric')
	
	* need to build in a safeguard that ensures all y variables are the same.
	local depvar = e(depvar)
	local indvars = subinstr("`varlist'","`depvar'", "",.)
	
	optimize_weights `depvar', predictors(`library') vars(`varlist') library(`library') k(`k') evalmetric(`evalmetric') superpredname(`superpredname') superestname(`superestname') indvars(`indvars') newdata(`newdata') libraryglobals(`libraryglobals') originaldataset(`originaldataset')
end


************************************************
************************************************
************************************************
**** This is the cross-validation program ****
************************************************
************************************************
************************************************
* For this code, I need to build in functionality for the cross validated superlearner.
* Basically, for each fold I'll need to load the model from the Superlearner(string) file, and then call the command
* to get the weights for each fold.


/*
Significant help recieved from the existing crossfold package (see citation
below). I've rewritten my own copy as follows: 
 
Ben Daniels, 2012.
"CROSSFOLD: Stata module to perform k-fold cross-validation," Statistical Software Components
S457426, Boston College Department of Economics.
<https://ideas.repec.org/c/boc/bocode/s457426.html>
*/


capture program drop cross_validate
program define cross_validate, rclass

syntax anything [iweight/] [if/] [in], [vars(string)] [k(numlist min=1 max=1)] [custom(string)] [EWeight(varname)] [eif(string)] [ein(string)] [stub(string)] [loud] [evalmetric(string)] [superlearner(string)] [depvar(string)] * 


	* Initalize temporary variables.

	tempname u e A results group yhat average_error new_var 
	marksample touse
	
	* Options and syntax checks.
	
	if "`evalmetric'" == "" {
		local evalmetric = "rmse"
	}
	
	if "`evalmetric'" != "mae" & "`evalmetric'" != "r2" & "`evalmetric'" != "mse" & "`evalmetric'" != "auc" & "`evalmetric'" != "rmse" &  "`evalmetric'" != "" {
		di in red "Evaluation metric must either be mae, mse, r2, auc, or the default, rmse."
		exit 198
	}

	*default the number of folds to 10
	if "`k'" == "" {
		local k = 10
	}
	
	*number of folds cannot be 0 or 1
	if "`k'" <= "1" {
		di in red "Number of folds cannot be less than 2"
		exit 198
	}
		
	*shouldn't have more folds than observations
	if `k' > _N {
		di in red "Number of folds cannot exceed number of observations"
		exit 198
	}
	*should not have more than 100 folds.	
	if `k' > 100 {
		di in red "Number of folds cannot exceed 100"
		exit 198
	}
	*by default we don't display each model as it is fit.
	if "`loud'" == "" {
		local qui = "qui"
	}

	*model weights if specified per variable
	if "`eweight'" != "" {
		local eweight = "[weight=`eweight']"
	}
		
	if "`weight'" != "" {
		local weight = "[weight=`exp']"
	}
		
	if "`eif'" != "" {
		local eif = "& `eif'"
	}
	
	if "`ein'" != "" {
		local ein = "in `ein'"
	}
		
	if "`stub'" == "" {
		local stub = "est"
	}
	
	local title Cross Validation
	display _newline as text "`title'" _continue
	local textpreface _col(40) as text 
	local intpreface  _col(67) "= " as res %10.0fc 
	local realpreface _col(67) "= " as res %10.4f 
	display `textpreface' "Number of observations" `intpreface'  _N		
	display `textpreface' "Evaluation Metric"      `realpreface' "`evalmetric'"
	display `textpreface' "K"   					`intpreface' "`k'"
	
		
* Randomize dataset and initialize results matrix.

	* generate from a uniform distribution between 0 and 1
	gen `u'	= uniform()
	
	* create k number of quantiles and splits the data apropriately. 
	* to view this data, list idcode testing_variable, sep(4)
	* where sep(#) just separates by number of observations.
	xtile `group' = `u', n(`k')
	
	* creates k rows of empty data in one cell
	mat `results' = J(`k',1,.)
	
	* for each value specified by the number of folds, create each row that 
	* reads 'est1, est2, est3' which will be estimating the MSE.
	local rnames
		forvalues i=1/`k' {
			local rnames "`rnames' "`stub'`i'""
		}
	matrix rownames `results' = `rnames'
	
	
	
	* Fit models and calculate errors.
	* Since the folds are already created, we need to loop through each algorithm within each fold.
	* Tokenize allows us to break up the input for a loop through. We store each 
	* model run in the j variable. We then need to keep track of the lowest MSE
	* for the sake of discrete superlearner
	tokenize `anything'
	local j = 1
	local lowmse = .
	local model = ""	
	
	while "`1'" != "" {
		
		* This helps when creating the new variable to hold estimates
		local new_var = ""
	
		* Loop through each of the folds to generate average
		local average_error_sum = 0
		forvalues i=1/`k' {
		
			
			* it's possible for the user to specify a custom method which we need to deal with. 
			* Note, this has been addressed
			local test_custom = usubstr("``j''",1 ,6)
			if "`test_custom'" == "custom"{ 
				`qui' $``j'' `weight' if `group' != `i' & `touse' , `options'
			}
			else{
				* declare the dependent variable and make prediction
				* note, we exclude the current group and use the rest of the sample. 
				`qui' ``j'' `vars' `weight' if `group' != `i' & `touse' , `options'
			}
			
			local depvar = e(depvar)
			*capture estimates and store them in stubi name.
			*cap estimates store `stub'`i'
				
			* Make a prediction on the set not tested (we should have a column of yats).
			`qui' predict ``j''_`i' if `group' == `i'
			local new_var = "`new_var'" + " ``j''_`i'"
			
			* Generate error term- MAE = Mean Absolute Error instead of MSE
				if "`evalmetric'" == "mae" {
					qui gen `e' = abs(``j''_`i'-`depvar') if `group' == `i' `eif' `ein'
					local result ""
					local label  "MAE"
				}
			* Pseudo R2 as the square of the correlation coefficient instead of MSE
				else if "`evalmetric'" == "r2" {
					local label  "Pseudo-R2"
				}
			* Should they select the AUC as an evaluation metric
				else if "`evalmetric'" == "auc" {
					lroc if `group' == `i' `eif' `ein', nograph	
					qui gen `e' = r(area) 
					local label "AUC"
				}
			* Should they select the rmse as an evaluation metric
				else if "`evalmetric'" == "rmse" {
					qui gen `e' = (``j''_`i'-`depvar')*(``j''_`i'-`depvar') if `group' == `i' `eif' `ein'
					local result "sqrt"
					local label  "RMSE"
				}				
			* Generate the MSE (default)
				else {
					qui gen `e' = (``j''_`i'-`depvar')*(``j''_`i'-`depvar') if `group' == `i' `eif' `ein'
					local label  "MSE"
				}
			
			* Tabulate errors
			
				if "`evalmetric'" != "r2" {
					qui tabstat `e' `eweight' if `group' == `i' `eif' `ein', save
					mat `A' 			  = r(StatTotal)
					local mean 		   	  = `A'[1,1]					
					mat `results'[`i',1]  = (`mean')
					local average_error_sum = `average_error_sum' + `result'(`mean')						
				}
				else {
					* Generate psuedo r-squared.
					qui corr ``j''_`i' `depvar'  if `group' == `i'
					mat `results'[`i',1]  = r(rho)*r(rho)
					local average_error_sum = `average_error_sum' + (r(rho)*r(rho))
				}

				
				*drop `yhat'
				cap drop `e'			

			}
			local average_error = `result'(round(`average_error_sum' / `k', .00001))
			
			display "``j'': `average_error' "
			
			if `average_error' < `lowmse' {
				local lowmse = `average_error'
				local model = "``j''"
			}
			
			* This bit of code stores the predicted values for each of the columns.
			* We need these estimates for the optimization
				*egen "``j''_cv" = rowtotal(`new_var')
			if "`superlearner'" == "" {
				drop `new_var'
			}
			macro shift
		}
		
			* This is the case where if we have superlearner and we need predictions, well let's make the superlearner in each fold.
			if "`superlearner'" != "" {
				* We have predictions run off each model, we should loop through the number of folds
				* and create predictions based on the data from those folds.
			local average_error_sum_sl = 0
			forvalues i=1/`k' {
			display "Performing Cross Validation on Fold: `i'"
				local count = wordcount("`anything'")		
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
				tokenize `anything'
				local counter = 1
				local exp = "`depvar' = "				
				local exp2 = ""
				local exp3 = ""
				while `counter' <= `count'{	
					if `counter' == 1{
						local exp = "`exp'" + "`ma`counter''*``counter''_`i' "
						local exp2 = "`exp2'" + "(``counter'': `na`counter'')"
						local exp3 = "`exp3'" + " !missing(``counter''_`i') "					
					}
					else{
						local exp = "`exp'" + " + `ma`counter''*``counter''_`i' " 
						local exp2 = "`exp2'" + "(``counter'': `na`counter'')"
						local exp3 = "`exp3'" + " & !missing(``counter''_`i') "
					}
					local counter = `counter' + 1					
				}
				`qui' nl (`exp') if `exp3'  , eps(1e-10) nolog	
				*We can now build the code required to actually get the weights from the optimization
				`qui' nlcom `exp2', iterate(1000) //, post
				
				*Small loop to round our coefficients
				mat b = r(b)
				local colnamesnames : colnames r(b)
				
				local cols = colsof(b)
				matrix C = J(1,`cols',0)
				forvalues c = 1/`cols' {
					matrix C[1,`c']= round(b[1,`c'], .000001)
				}
				
				matrix colnames C = `colnamesnames'
				
				local pred_name = "y_hat_`i'"
				`qui' predict "`pred_name'" if `group' == `i'
				
				* Now that we have the predictions within each fold, we can now find the evaluation metric for each fold...
				
				* Generate error term- MAE = Mean Absolute Error instead of MSE
				if "`evalmetric'" == "mae" {
					qui gen `e' = abs(y_hat_`i'-`depvar') if `group' == `i' `eif' `ein'
					local result ""
					local label  "MAE"
				}
				* Pseudo R2 as the square of the correlation coefficient instead of MSE
					else if "`evalmetric'" == "r2" {
						local label  "Pseudo-R2"
					}
				* Should they select the AUC as an evaluation metric
					else if "`evalmetric'" == "auc" {
						`qui' lroc if `group' == `i' `eif' `ein', nograph	
						qui gen `e' = r(area) 
						local label "AUC"
					}
				* Should they select the rmse as an evaluation metric
					else if "`evalmetric'" == "rmse" {
						qui gen `e' = (y_hat_`i'-`depvar')*(y_hat_`i'-`depvar') if `group' == `i' `eif' `ein'
						local result "sqrt"
						local label  "RMSE"
					}						
				* Generate the MSE (default)
					else {
						qui gen `e' = (y_hat_`i'-`depvar')*(y_hat_`i'-`depvar') if `group' == `i' `eif' `ein'
						local label  "MSE"
					}
					
				
				* Tabulate errors
				
					if "`evalmetric'" != "r2" {
						qui tabstat `e' `eweight' if `group' == `i' `eif' `ein', save
						mat `A' 			  = r(StatTotal)
						local mean 		   	  = `A'[1,1]					
						mat `results'[`i',1]  = (`mean')
						local average_error_sum_sl = `average_error_sum_sl' + `result'(`mean')
					}
					else {
						* Generate psuedo r-squared.
						qui corr y_hat_`i' `depvar'
						mat `results'[`i',1]  = r(rho)*r(rho)
						local average_error_sum_sl = `average_error_sum_sl' + (r(rho)*r(rho))
					}			
					cap drop `e'
			}
			
			local average_error_sl = `result'(round(`average_error_sum_sl' / `k', .00001))
			display "SuperLearner: `average_error_sl' "
			
******************************************************;				


			}
			
			if "`superlearner'" != "" {
				if `average_error_sl' < `lowmse'{
					local title Cross Validataed SuperLearner Results 
					display _newline as text "`title'" _continue
					local textpreface _col(40) as text 
					local intpreface  _col(67) "= " as res %10.0fc 
					local realpreface _col(67) "= " as res %10.4f 
					display `textpreface' "Number of observations" `intpreface'  _N
					display `textpreface' "Evaluation Metric"      `realpreface' "`evalmetric'"
					display `textpreface' "K"   					`realpreface' "`k'"			
					display "Superlearner: `average_error_sl'"
					display _newline as text "" _continue
					display _newline as text "" _continue
					display _newline as text "" _continue
					display _newline as text "" _continue
				}
				else{
					local title Discrete SuperLearner Results 
					display _newline as text "`title'" _continue
					local textpreface _col(40) as text 
					local intpreface  _col(67) "= " as res %10.0fc 
					local realpreface _col(67) "= " as res %10.4f 
					display `textpreface' "Number of observations" `intpreface'  _N
					display `textpreface' "Evaluation Metric"      `realpreface' "`evalmetric'"
					display `textpreface' "K"   					`realpreface' "`k'"
					display "`model' : `lowmse' " 	
					display _newline as text "" _continue
					display _newline as text "" _continue
					display _newline as text "" _continue
					display _newline as text "" _continue
				}
			}
		
	
	
* Return matrix of results. NOTE: This was edited. I'm surpressing the return
* output so that ONLY the average cross validated risk estimate can be 
* returned.
	
	*mat colnames `results' = "`label'"
	*matlist `results'
	*return matrix `stub'   = `results'
	
	*Note, all I'm returning is the average MSE. That's all we care about.
	*return scalar average_mse = `average_error'
	
end


************************************************
************************************************
************************************************
**** This is the main optimize-weights program ****
************************************************
************************************************
************************************************

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
			`qui' predict ``j''
		}
		else{
			`qui' ``j'' `vars' `weight' , `options'
			`qui' predict ``j''
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
		
		`qui' nl (`exp'), eps(1e-10) nolog
		
		display "********Calculating the weights for each algorithm********"
		`qui' nlcom `exp2' , iterate(10000)
		
		
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
		
		`qui' estimates save "`superestname'", replace
		
		display "********Estimation Saved********"
		
		* We have to save the dataset with the predictions
		local predfile = "`superpredname'" + ".dta"
		`qui' save "`predfile'", replace
		
		* We should run a cross validated superlearner as well. How does our superlearner hold up against the other predictors?
		display "********Performing Cross Validated SuperLearner********"
		* This needs to be worked on...
		cross_validate `library', vars(`vars') k(`k') evalmetric(`evalmetric') superlearner("`superestname'") depvar(`varlist')
		
		
		* If the user wants to make an out of sample prediction, do the following:
		if "`newdata'" != "" {
		
			* Clear out the original dataset and reload.
			clear 
			`qui' sysuse `originaldataset'
			
		
			
			*clear the dataset, run predict on the new dataset, save the new dataset, close it, open the original dataset and repeat.
			tokenize `predictors'
			local counter = 1
			local j = 1
			
			
			* Reload all the associated models, if there are any...
			if "`libraryglobals'" != "" {
				`qui' do `libraryglobals'
			}			
			
			
			while "`1'" != "" {
				* For each model in the library, we have to load the original dataset
				`qui' sysuse `originaldataset'
				
				* Run the model (check if it's a custom method)
				local test_custom = usubstr("``j''",1 ,6)
				if "`test_custom'" == "custom"{
					`qui' do `libraryglobals'
					`qui' $``j'' 
				}
				else{
					`qui' ``j'' `varlist' `indvars' 
				}
				* Save estimates
				local estimatemodel = "estimates" + "`counter'"
				`qui' estimates save "`estimatemodel'", replace
				
				* Clear the dataset, load the original, run prediction, save dataset
				clear 
				`qui' sysuse `newdata'
				
				estimates use `estimatemodel'
				`qui' predict ``j''
				`qui' save `newdata', replace
				
				local counter = `counter' + 1
				macro shift
			}
			`qui' estimates use `superestname' 
			`qui' predict superlearner_prediction
			
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
