fact <- function(n){
	if(n < 1){
		return(1)
	} else {
		return(prod(1:n))
	}
}

mod_bessel_1 <- function(x, j){
	S <- 0
	for (n in 0:100){
		S <- S+((x/2)^(2*n + j))/fact(n)/fact(n+j)
	}
	return(S)
}

for(x in 1:20){
	for(j in 1:20){
		print( x*(mod_bessel_1(x,j-1) - mod_bessel_1(x, j+1)) )
		print( 2*j*mod_bessel_1(x,j) )
	}
}

for(j in 1:2000){
	print( mod_bessel_1(20,j-1) - mod_bessel_1(20, j) )
}
