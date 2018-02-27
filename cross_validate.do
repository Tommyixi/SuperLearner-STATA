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

syntax anything [iweight/] [if/] [in], [vars(string)] [k(numlist min=1 max=1)] [custom(string)] [EWeight(varname)] [eif(string)] [ein(string)] [stub(string)] [loud] [evalmetric(string)] [superlearner(string)] * 

	* Initalize temporary variables.

	tempname u e A results group yhat average_error new_var 
	marksample touse
	
	* Options and syntax checks.
	
	if "`evalmetric'" == "" {
		local evalmetric = "mse"
	}
	
	if "`evalmetric'" != "mae" & "`evalmetric'" != "r2" & "`evalmetric'" != "mse" & "`evalmetric'" != "auc" &  "`evalmetric'" != "" {
		di in red "Evaluation metric must either be mae, r2, auc, or the default, mse."
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
				`qui' $``j'' `weight' if `group' != `i' & `touse'  , `options'
			}
			else{
				* declare the dependent variable and make prediction
				* note, we exclude the current group and use the rest of the sample. 
				`qui' ``j'' `vars' `weight' if `group' != `i' & `touse'  , `options'
			}
			local depvar = e(depvar)
			*capture estimates and store them in stubi name.
			*cap estimates store `stub'`i'
				
			* Make a prediction on the set not tested (we should have a column of yats).
			`qui' predict ``j''_`i' if `group' == `i'
			local new_var = "`new_var'" + " ``j''_`i'"
			
			* Stack these estimates into a matrix. We want an n X m matrix where n is the number of observations in the dataset and m
			* represents each learner in the library. From there we can create the weighted combination of weights
			* using the synth package.
			
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
					`qui' lroc if `group' == `i' `eif' `ein', nograph	
					qui gen `e' = r(area) 
					local label "AUC"
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
					qui corr ``j''_`i' `depvar'
					mat `results'[`i',1]  = r(rho)*r(rho)
					local average_error_sum = `average_error_sum' + (r(rho)*r(rho))
				}

				
				*drop `yhat'
				cap drop `e'			

			}
			local average_error = round(`average_error_sum' / `k', .00001)
			
			display "``j'': `average_error' "
			
			if `average_error' < `lowmse' {
				local lowmse = `average_error'
				local model = "``j''"
			}
			
			* This bit of code stores the predicted values for each of the columns.
			* We need these estimates for the optimization
			if "`superlearner'" != "" {
				* Initially I thought I would just be able to calculate the superlearner this way but I need
				* to actually call the superlearner on EACH fold. It might be better to do this in separate code.
				egen "``j''" = rowtotal(`new_var')
				drop `new_var'
			}
			else{
				*egen "``j''_cv" = rowtotal(`new_var')
				drop `new_var'
			}
			macro shift
		}
		
		if "`superlearner'" != "" {
			* Cast the predictions on the new cross validated predictions...
			estimates use `superlearner' 
			predict superlearner_cv_pred
			* Question for someone out there...
			* We went ahead and split the data into k folds. 
			* We ran each of the models on each training set and got fitted values on the validation set.
			* (so, each model was trained on each fold and validated on each fold..)
			* Can we then just take our loss function applied to the whole dataset rather than the loss within each fold?
			* Note, something is not right with our MSE calculation..
			
			
		}
		else{
			display "********Discrete Super Learner Results********"
			display "`model' : `lowmse' " 		
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
