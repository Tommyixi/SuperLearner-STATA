# SuperLearner (in STATA)


This README serves as an example for the `superlearner` program in the STATA programming language. The purpose of this document is to: 

  - Detail what the `superlearner` program can (and cannot) do 
  - Provide details on the use of the program
  - Give a detailed example of how the program can be executed
  - Future directions for the program

### What the Program Can Do

  - Uses the Super Learner algorithm to create predictions on (both) existing and new datasets.
  - Accepts (both) native and custom learners for the super learner algorithm.
  - Displays the cross validated risk estimate using a variety of different evaluation metrics.
  
### What the Program Cannot Do
 - Execute when the outcome is binary or categorical.
 - Accept categorical predictors.

### Example

The following example will use the files that are supplied in this repository. We will walkthrough the example together, however if you would like to jump ahead and execute the code on your own simply run the "superlearner.ado" file and then the subsequent "example.do" file.

For this question, we will be using a subset of STATA's built in cars dataset. Specifically, we will see if we can predict a car's MPG with only knowing the car's: length, price, weight, length, turn, and displacement.

To run this example, carry out the following commands:

```stata
clear

use "training_data.dta"

do "superlearner.ado"

set seed 1

superlearner mpg length price weight length turn displacement,  k(10) family("gaussian") library("custom_a custom_b  custom_c regress ridgeregress lassoregress") libraryglobals("custom_learners.do") superpredname("predictions") superestname("estimates") originaldataset("training_data.dta") newdata("testing_data.dta")
```
We will break down each of the parameters and options as follows:
- `superlearner mpg length price weight length turn displacement` - 
- `k(10)`
- `family("gaussian")` 
- `library("custom_a custom_b  custom_c regress ridgeregress lassoregress")`
- `libraryglobals("custom_learners.do")`
- `superpredname("predictions")`
- `superestname("estimates")`
- `originaldataset("training_data.dta")`
- `newdata("testing_data.dta")`






