capture program drop param_checks

	program define param_checks
	syntax varlist(min=2) , [if] [in] k(integer) family(string) library(string) [originaldataset(string)] [superpredname(string)] [superestname(string)] [newdata(string)] [libraryglobals(string)] [loud] [evalmetric(string)]
	
	display "tommy"
end
