# m1, m2: the sample means
# sd1, sd2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
options(scipen=10) # set high penalty for scientific display
t.test2 <- function(m1,m2,sd1,sd2,n1,n2,m0=0,equal.variance=FALSE)
{
    if( equal.variance==FALSE ) 
    {
        se <- sqrt( (sd1^2/n1) + (sd2^2/n2) )
        # welch-satterthwaite df
        df <- ( (sd1^2/n1 + sd2^2/n2)^2 )/( (sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1) )
    } else
    {
        # pooled standard deviation, scaled by the sample sizes
        se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2) ) 
        df <- n1+n2-2
    }      
    t   <- (m1-m2-m0)/se 
    dat <- c(m1-m2, se, t, df, 2*pt(-abs(t),df))  
    names(dat) <- c("Difference of means", "Std Error", "t", "df", "p-value")
    return(dat) 
}

n1=8
n2=10
m1=161.25
m2=129
sd1=12.46423
sd2=11.97219
t.test2( m1,m2,sd1,sd2,n1,n2)
t.test2( m1,m2,sd1,sd2,n1,n2, equal.variance=TRUE)