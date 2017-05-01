// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

// #this function handles allele vectors, all it is doing is removing an allele from the vector, so it should be beautifully simple
// [[Rcpp::export]]
StringVector killGenemat (StringVector alvec,Rcpp::Nullable<Rcpp::String> targetAllele = R_NilValue, bool printIt=true){
	// pick random node if no target node is specified, choose a node that is not an environment node
	if (targetAllele.isNotNull()) {
        Rcpp::Rcout << "you have specified a target allele, if you write me a function, I can remove it"<<std::endl;
	}
	else {
        StringVector alvec_NE = alvec;
        int n = alvec.size();
        for( int i= n-1; i >= 0; i-- ) {
           std::string myString = as<std::string>(alvec[i]);
           if (myString[0] == 'e'){
                 alvec_NE.erase(i);
           }
        }
        NumericVector prob = NumericVector::create();
        StringVector targetAllele = RcppArmadillo::sample(alvec_NE,1,true,prob);
        std::string targetAllele2 = as<std::string>(targetAllele);
        if (printIt){
           Rcpp::Rcout <<"removing allele "<< targetAllele << std::endl;
        }
        for( int i= n-1; i >= 0; i-- ) {
           std::string myString = as<std::string>(alvec[i]);
           if (myString == targetAllele2){
                 alvec.erase(i);
           }
        }
	}
	return alvec;
}


/*
// [[Rcpp::export]]
StringVector get.genes (StringVector alvec){

get.genes<-function(alvec){
	g<-strsplit(alvec,split="_")
	return(as.character(g[1]))}

	*/
