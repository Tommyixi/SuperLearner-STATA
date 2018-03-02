clear
sysuse auto.dta

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
		newdata:			The name of the new dataset you would like to make predictions on (used for OOS prediction)
		libraryglobals:		The name of the file containing the custom global macros for prediction (used for OOS prediction)
		originaldataset:	The name of the original dataset (used for OOS prediction)
	*/
	
	* Parameter checks!
	   

	cross_validate `library', vars(`varlist') k(`k') evalmetric(`evalmetric')
	
	* need to build in a safeguard that ensures all y variables are the same.
	local depvar = e(depvar)
	local indvars = subinstr("`varlist'","`depvar'", "",.)
	
	optimize_weights `depvar', predictors(`library') vars(`varlist') library(`library') k(`k') evalmetric(`evalmetric') superpredname(`superpredname') superestname(`superestname') indvars(`indvars') newdata(`newdata') libraryglobals(`libraryglobals') originaldataset(`originaldataset')
end


*Small example using regression, glm, and mixed models
set seed 1
cd "/Users/Tommy/Documents/Berkeley/Thesis research"
global custom_a = "regress mpg weight trunk price"
global custom_b = "regress mpg weight trunk"  
global custom_c = "regress mpg weight length" 
*superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_b custom_a custom_c regress elasticregress ridgeregress lassoregress ") superpredname("tommy") superestname("estimates") newdata("cars_altered.dta") originaldataset("auto.dta") libraryglobals("library.do")
superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_a custom_b  custom_c regress ridgeregress lassoregress") superpredname("tommy") superestname("estimates") newdata("cars_altered.dta") originaldataset("auto.dta") libraryglobals("library.do")
*superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_b custom_a custom_c regress ridgeregress lassoregress") superpredname("tommy") superestname("estimates") 
*superlearner mpg length price weight,  k(10) family("gaussian") library("custom_b custom_a custom_c regress") superpredname("tommy") superestname("estimates")


* Another example
clear
webuse lbw 

global custom_z = "logistic low age lwt smoke ptl ht ui" 
global custom_x = "logistic low age lwt smoke ptl"
global custom_w = "probit low age "  
superlearner low age lwt smoke ptl ht ui, k(10) family("gaussian") library("custom_w custom_z custom_x probit") evalmetric("auc")


