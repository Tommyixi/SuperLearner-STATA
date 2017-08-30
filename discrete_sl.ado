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
			a. Average the risk across all simulations for each validation.
		6. Discrete Super Learner will select the algorithm with the lowest cross validated risk.			
*/ 