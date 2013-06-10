# FUNCTION TO PRODUCE MEAN/SE OR MEAN/SD COMBINATIONS FOR PRINTING
# PRODUCES A VECTOR OF THE FORM "mean (SE)" FOR OUTPUT (e.g., IN xtable() ) 

# Copyright (C) 2012  Thomas J. Leeper
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.


# val is a vector of values (e.g., regression coefficients)
# par is a vector of parrenthetical values (e.g., standard errors)
# digits is an optional two-item vector indicating the number of digits to which coefficients and errors should be rounded

coefpaste <- function(val,par,digits=c(2,2)){
	if(length(digits)==1)
		digits <- rep(digits,2)
	# internal function
	func <- function(coef,var){
		# handle 'val'/'coef'
		if(digits[1]>0)
			coef <- sprintf(coef,fmt=paste("%#.",digits[1],"f",sep=""))
		else if(digits[1]==0)
			coef <- round(coef,0)
		else if(digits[1]<0){
			cchar <- nchar(as.numeric(strsplit(as.character(coef),"[.]")[[1]][1])) + digits[1]
			coef <- signif(coef,cchar)
		}
		# handle 'par'/'var'
		if(digits[2]>0)
			var <- sprintf(var,fmt=paste("%#.",digits[2],"f",sep=""))
		else if(digits[2]==0)
			var <- round(var,0)
		else if(digits[2]<0){
			vchar <- nchar(as.numeric(strsplit(as.character(var),"[.]")[[1]][1])) + digits[2]
			var <- signif(var,vchar)
		}
		output <- paste(coef," (",var,")",sep="")
		return(output)
	}
	# return
	if(length(val)==1)
		return(func(val,par))		
	else if(length(val)>1)
		return(mapply(func,val,par))
}
