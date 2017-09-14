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

syntax model [iweight/] [if/] [in], [k(numlist min=1 max=1)] [EWeight(varname)] [eif(string)] [ein(string)] [stub(string)] [loud] [mae] [r2] * 

	* Initalize temporary variables.

	tempname u e A results group yhat
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
		di in red "Number of folds must cannot be less than 2"
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
	
	* creates 10 rows of empty data in one cell
	mat `results' = J(`k',1,.)
	
	* for each value specified by the number of folds, create each row that 
	* reads 'est1, est2, est3' which will be estimating the RMSE.
	local rnames
		forvalues i=1/`k' {
			local rnames "`rnames' "`stub'`i'""
			}
	matrix rownames `results' = `rnames'

	
	* Fit models and calculate errors.
	
	* Loop through each of the folds.
	forvalues i=1/`k' {
		
		* declare the dependent variable and make prediction
		* note, we exclude the current group and use the rest of the sample. 
		`qui' `model' `weight' if `group' != `i' & `touse'  , `options'
		local depvar = e(depvar)
		
		*capture estimates and store them in stub1 name.
		cap estimates store `stub'`i'
			
		*make a prediction on the set not tested.	
		qui predict `yhat' if `group' == `i' `eif' `ein'
		
		* Generate error term- MAE = Mean Absolute Error instead of RMSE
			if "`mae'" == "mae" {
				qui gen `e' = abs(`yhat'-`depvar') if `group' == `i' `eif' `ein'
				local result ""
				local label  "MAE"
				}
		* Pseudo R2 as the square of the correlation coefficient instead of RMSE
			else if "`r2'" == "r2" {
				local label  "Pseudo-R2"
				}
		* Generate the RMSE
			else {
				qui gen `e' = (`yhat'-`depvar')*(`yhat'-`depvar') if `group' == `i' `eif' `ein'
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
				qui corr `yhat' `depvar'
				mat `results'[`i',1]  = r(rho)*r(rho)
				}
		
			drop `yhat'
			cap drop `e'

		}
	
* Return results.
	
	mat colnames `results' = "`label'"
	matlist `results'
	return matrix `stub'   = `results'
	
end
