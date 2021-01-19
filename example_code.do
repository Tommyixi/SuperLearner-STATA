/* This is an example for how to run the super learner (natively) in STATA */ 


*Small example using regression, glm, and mixed models
clear
sysuse auto.dta

do "superlearner.ado"

set seed 1

superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_a custom_b  custom_c regress ridgeregress lassoregress") superpredname("predictions") superestname("estimates") originaldataset("auto.dta") libraryglobals("library.do")/* This is an example for how to run the super learner (natively) in STATA */ 
