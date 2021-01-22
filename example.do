/* This is an example for how to run the super learner (natively) in STATA */ 


*Small example using regression, glm, and mixed models
clear

use "training_data.dta"


do "superlearner.ado"
do "custom_learners.do"

set seed 1

superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_a custom_b  custom_c regress ridgeregress lassoregress") libraryglobals("custom_learners.do") superpredname("predictions") superestname("estimates") originaldataset("training_data.dta") newdata("testing_data.dta")
