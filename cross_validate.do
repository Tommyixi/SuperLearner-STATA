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

syntax anything [iweight/] [if/] [in], [vars(string)] [k(numlist min=1 max=1)] [custom(string)] [EWeight(varname)] [eif(string)] [ein(string)] [stub(string)] [loud] [mae] [r2] * 

	* Initalize temporary variables.

	tempname u e A results group yhat average_error new_var 
	marksample touse
	
	* Options and syntax checks.

	if "`mae'" == "mae" & "`r2'" == "r2" {
		di in red "MAE cannot be combined with R2; choose only one evaluation metric."
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
	* reads 'est1, est2, est3' which will be estimating the RMSE.
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
		
			
			* it's possible for the user to specify a custom method which we need to deal with
			local test_custom = usubstr("``j''",1 ,6)
			if "`test_custom'" == "custom"{
				`qui' $``j'' `weight' if `group' != `i' & `touse'  , `options'
			}
			else{
				* declare the dependent variable and make prediction
				* note, we exclude the current group and use the rest of the sample. 
				*`qui' `anything' `weight' if `group' != `i' & `touse'  , `options'
				qui ``j'' `vars' `weight' if `group' != `i' & `touse'  , `options'
			}
			local depvar = e(depvar)
			*capture estimates and store them in stubi name.
			*cap estimates store `stub'`i'
				
			* Make a prediction on the set not tested (we should have a column of yats).
			qui predict ``j''_`i' if `group' == `i'
			local new_var = "`new_var'" + " ``j''_`i'"
			
			* Stack these estimates into a matrix. We want an n X m matrix where n is the number of observations in the dataset and m
			* represents each learner in the library. From there we can create the weighted combination of weights
			* using the synth package.
			
			* Generate error term- MAE = Mean Absolute Error instead of RMSE
				if "`mae'" == "mae" {
					qui gen `e' = abs(``j''_`i'-`depvar') if `group' == `i' `eif' `ein'
					local result ""
					local label  "MAE"
					}
			* Pseudo R2 as the square of the correlation coefficient instead of RMSE
				else if "`r2'" == "r2" {
					local label  "Pseudo-R2"
					}
			* Generate the RMSE (default)
				else {
					qui gen `e' = sqrt((``j''_`i'-`depvar')*(``j''_`i'-`depvar')) if `group' == `i' `eif' `ein'
					local result "sqrt"
					local label  "RMSE"
					}
			
			* Tabulate errors
			
				if "`r2'" != "r2" {
					qui tabstat `e' `eweight' if `group' == `i' `eif' `ein', save
					mat `A' 			  = r(StatTotal)
					local mean 		   	  = `A'[1,1]
					mat `results'[`i',1]  = `result'(`mean')
				}
				else {
					* Generate psuedo r-squared.
					qui corr ``j''_`i' `depvar'
					mat `results'[`i',1]  = r(rho)*r(rho)
					}
					
						
				local average_error_sum = `average_error_sum' + `result'(`mean')		
				
				*drop `yhat'
				cap drop `e'			

			}
			local average_error = `average_error_sum' / `k'
			
			display "``j'': `average_error' "
			
			if `average_error' < `lowmse' {
				local lowmse = `average_error'
				local model = "``j''"
			}
			
			* This bit of code stores the predicted values for each of the columns.
			* We need these estimates for the optimization
			egen ``j'' = rowtotal(`new_var')
			drop `new_var'
			macro shift
						
		}
		display "********Discrete Super Learner Results********"
		display "`model' : `lowmse' " 
		
		* At this point we should take the estimates generated and optimize the weights for the actual SL
	
	
			
		
* Return matrix of results. NOTE: This was edited. I'm surpressing the return
* output so that ONLY the average cross validated risk estimate can be 
* returned.
	
	*mat colnames `results' = "`label'"
	*matlist `results'
	*return matrix `stub'   = `results'
	
	*Note, all I'm returning is the average MSE. That's all we care about.
	*return scalar average_mse = `average_error'
	
end
