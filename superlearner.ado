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
		indvars: 			Independent variables required for weight optimization 
	*/
	
	* Parameter checks!	   

	cross_validate `library', vars(`varlist') k(`k') evalmetric(`evalmetric')
	
	* need to build in a safeguard that ensures all y variables are the same.
	local depvar = e(depvar)
	local indvars = subinstr("`varlist'","`depvar'", "",.)
	
	optimize_weights `depvar', predictors(`library') vars(`varlist') library(`library') k(`k') evalmetric(`evalmetric') superpredname(`superpredname') superestname(`superestname') indvars(`indvars') newdata(`newdata') libraryglobals(`libraryglobals') originaldataset(`originaldataset')
end
