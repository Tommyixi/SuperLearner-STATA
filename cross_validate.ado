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
			display "we are at i: `i'"
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
			local average_error_sl = round(`average_error_sum_sl' / `k', .00001)
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
