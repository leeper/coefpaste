# FUNCTION TO PRODUCE MEAN/SE OR MEAN/SD COMBINATIONS FOR PRINTING
# PRODUCES A VECTOR OF THE FORM "mean (SE)" FOR OUTPUT (e.g., IN xtable() ) 

# Copyright (C) 2012  Thomas J. Leeper
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.


# val is a vector of values (e.g., regression coefficients)
# par is a vector of parrenthetical values (e.g., standard errors)
# digits is an optional two-item vector indicating the number of digits to which coefficients and errors should be rounded
# stars.val is a function taking two arguments for generating a star (or stars) after the val
# stars.par is a function taking two arguments for generating a star (or stars) after the parentheses

coefpaste <- function(val,par,digits=c(2,2), stars.val=NULL, stars.par=NULL){
	if(length(digits)==1)
		digits <- rep(digits,2)
	# internal function
	func <- function(coef,var){
		# handle 'val'/'coef'
		if(digits[1]>0)
			coefout <- sprintf(coef,fmt=paste("%#.",digits[1],"f",sep=""))
		else if(digits[1]==0)
			coefout <- round(coef,0)
		else if(digits[1]<0){
			cchar <- nchar(as.numeric(strsplit(as.character(coef),"[.]")[[1]][1])) + digits[1]
			coefout <- signif(coef,cchar)
		}
		# handle 'par'/'var'
		if(digits[2]>0)
			varout <- sprintf(var,fmt=paste("%#.",digits[2],"f",sep=""))
		else if(digits[2]==0)
			varout <- round(var,0)
		else if(digits[2]<0){
			vchar <- nchar(as.numeric(strsplit(as.character(var),"[.]")[[1]][1])) + digits[2]
			varout <- signif(var,vchar)
		}
		if(!is.null(stars.val))
			output <- paste(coefout, stars(coef,var), " (",varout,")", sep="")
		else if(!is.null(stars.par))
			output <- paste(coefout, " (",varout,")", stars(coef,var), sep="")
		else
			output <- paste(coefout, " (",varout,")",sep="")
		return(output)
	}
	# return
	if(length(val)==1)
		return(func(val,par))		
	else if(length(val)>1)
		return(mapply(func,val,par))
}
