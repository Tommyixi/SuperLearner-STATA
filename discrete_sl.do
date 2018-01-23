/* 	This document is going to serve as the main file for the discrete superlearner algorithm in Stata.
	General plan for implementing discrete superlearner:
		1. User inputs: data, models (algorithms)
		2. Split the dataset into a specified number of folds (k?). Let's say 10 in this example.
			a. 9 of the folds will be required to train the data, the 10th will be the validation set.
			b. Each fold is responsible for being BOTH a training and validation set in each iteration.
		3. Fit each of the specified algorithms on each of the folds.
			a. If you have 10 folds and three algorithms, you will have thirty models fit.
		4. Predict whatever value is you're trying to predcit with the fitted models.
		5. Within each validation set, calculate the cross validated risk.
			a. Take the 
			a. Average the risk across all simulations for each validation.
		6. Discrete Super Learner will select the algorithm with the lowest cross validated risk.
		7. If users want to declare their own learner they should use a global variable prefaced with custom_
			Note, this will require the user to actually specify the model they want to use. 
			ex: global custom_a = "regress mpg weight"
		
		
		****next steps
		8. Actually return the model fit. 
		9. Create a separate file that creates the convex combination of weights. 
		10. Allow for more customizability (try and add more options).
		11. Plotting and graphing? 
		12. CV superlearner? 

		For this project, we will start with just assuming a continuous outcome for the target parameter.
			- With that said, we will assume the L2 loss first.


SuperLearner <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                         method = 'method.NNLS', id = NULL, verbose = FALSE, control = list(),
                         cvControl = list(), obsWeights = NULL, env = parent.frame()) {			

CV.SuperLearner <- function(Y, X, V = NULL, family = gaussian(), SL.library, method = 'method.NNLS', id = NULL, verbose = FALSE, control = list(saveFitLibrary = FALSE), 
cvControl = list(), innerCvControl = list(), obsWeights = NULL, saveAll = TRUE, parallel = "seq", env = parent.frame()) {
  call <- match.call()
  N <- dim(X)[1L]                         	


*/
clear
sysuse auto.dta

capture program drop discrete_sl
program define discrete_sl, rclass
	
	* As of now, the actual superlearner call does not do a whole bunch. Eventually we'd like to specify for different families, methods, weights, etc...
	* The superlearner method should be a central control flow, delegating responsibilities 
	syntax varlist(min=2) , [if] [in] k(integer) family(string) library(string)
	
	* Options and syntax checks. (to do)
	
	display "********Calculating the average risk using  k = `k' fold cross validation with the mean absolute error evaulation********"

	cross_validate `library', vars(`varlist') k(`k')
	
	* Ideally here we should now have the discrete Superlearner selection (can we display that here?)
	
	/* 
		Next, we'd like to move the control flow of calling the optimize method here.
		The idea here is that the library name (regress, custom_a, etc...) also stores the predictions in the dataset.
		We should be able to use these names to optimize the weights.
	*/
	
	*optimize_weights `library'

end


*Small example using regression, glm, and mixed models
cd "/Users/Tommy/Documents/Berkeley/Thesis research"
global custom_a = "regress mpg weight trunk price"
global custom_b = "regress mpg weight trunk headroom price length"  
global custom_c = "regress mpg length price weight"  
discrete_sl mpg length price weight,  k(10) family("gaussian") library("custom_b regress custom_c custom_a")
