# MODIFICATION OF SIMPLE PETERSEN FUNCTION FROM BTSPAS PACKAGE

# Computes the Petersen estimator (Chapman correction applied) for the number 
# of UNMARKED animals given n1, m2, and u2. To find the estimate of abundance, 
# you need to add back n1+u2 animals.

# create a wrapper that spits out the total

SimplePetersenMod <- function(n1,m2,u2){
		unmarked <- SimplePetersen(n1,m2,u2)		
		total <- unmarked
		total[,1] <- total[,1] + n1
		if(length(n1==1)){ out.obj <- c(n1=n1,m2=m2,u2=u2,round(unlist(total)) )}
		if(length(n1>1)){ out.obj <- cbind(n1=n1,m2=m2,u2=u2,est=round(total$N.est),se=round(total$N.se)) }

		return(out.obj)
		}





# FUNCTION FOR DROPOUT ADJUSTMENT
# stand-alone implementation of the code from
# Carl Schwarz's function TimeStratPetersenNonDiagErrorNPMarkAvail_fit.R

dropout.adj <- function(Abd,SE_Abd,tags_dropped, tags_total){

dr <- tags_dropped/tags_total
se_dr <- sqrt(dr*(1-dr)/tags_total)
Abd_adj <- Abd * (1-dr)
SE_Abd_Adj  <- sqrt(SE_Abd^2 * se_dr^2+
                     SE_Abd^2 * (1-dr)^2 +
                     Abd^2 * se_dr^2)
					 
out.vec <- c(Abd_adj,SE_Abd_Adj)
names(out.vec) <- c("Abd_adj","SE_Abd_Adj")

return(out.vec)
					 
}

