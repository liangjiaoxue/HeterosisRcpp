// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

/* this function handles allele vectors. It duplicates an allele in the vector, the input matrix does not need to be changed,
 no input expressions are affected. Should be a breeze :-) */
// [[Rcpp::export]]
StringVector dupGenemat (StringVector alvec,Rcpp::Nullable<Rcpp::StringVector> parentAllele = R_NilValue, bool printIt=true){
	// pick random node if no target node is specified, choose a node that is not an environment node
	if (parentAllele.isNotNull()) {
        Rcpp::Rcout << "you have specified a target allele, if you write me a function, I can duplicate it"<<std::endl;
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
        if (printIt){
           Rcpp::Rcout <<"duplicating allele"<< targetAllele << std::endl;
        }
        Rcpp::String targetAllele2 = as <Rcpp::String> (targetAllele);
        alvec.push_back(targetAllele2);
	}
	return alvec;
}
