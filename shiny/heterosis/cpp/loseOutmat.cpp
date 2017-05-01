// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

//This function handles allele vectors. It generates a new allele and sets all the inputs of the alleles
//of the lost output gene from the new allele to 0.
// [[Rcpp::export]]
List loseOutmat_C (Rcpp::StringVector alvec,Rcpp::DataFrame alspace,
                 Rcpp::NumericMatrix inMat,
                 Nullable<Rcpp::String> targetAllele = R_NilValue,
                 Nullable<Rcpp::String> outputGene = R_NilValue,  bool printIt=true){

    CharacterVector alnames = alspace["name"];
	//pick random node if no target node is specified, choose a node that is not an environment node
	int envnumber = 0;
	int n = alvec.size();
	StringVector alvec_NE = alvec;
    for( int i= n-1; i >= 0; i-- ) {
        std::string myString = as<std::string>(alvec[i]);
           if (myString[0] == 'e'){
                envnumber++;
                 alvec_NE.erase(i);
           }
    }

    // Function extract matrix
    template <int RTYPE> inline Matrix<RTYPE>
    Subset1D(const Matrix<RTYPE>& x, CharacterVector crows) {
    R_xlen_t i = 0, j = 0, rr = crows.length(), rc = ccols.length(), pos;
    Matrix<RTYPE> res(rr, rc);

    CharacterVector xrows = rownames(x), xcols = colnames(x);
    IntegerVector rows = match(crows, xrows), cols = match(ccols, xcols);

    for (; j < rc; j++) {
        // NB: match returns 1-based indices
        pos = cols[j] - 1;
        for (i = 0; i < rr; i++) {
            res(i, j) = x(rows[i] - 1, pos);
        }
    }

    rownames(res) = crows;
    colnames(res) = ccols;

    return res;
}



    if (! targetAllele.isNotNull()) {
       bool skip = false


	}




    tryCatch({notEnv<-alvec[-grep("e",alvec)]
            },
            error=function(err){
              if (printIt){cat("there are no alleles in this alvec with outputs to each other. Skip mutation\nThe error message is:",err)}
              skip<-T
            },
            finally={
              outNum<-apply(inMat[notEnv,],1,sum)
              targetAllele<-sample(notEnv[which(outNum>0)],1)
            })



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





	if (!skip){
	#pick a gene that this allele is currently outputting (putting out? - probably not) to.
	outputals<-which(inMat[targetAllele,]==1)
	outsplit<-strsplit(alnames[outputals],split="_")
  genes<-unique(sapply(outsplit, function(vec) vec[1]))

	if (is.null(outputGene)){
		outputGene<-sample(genes,1)
	}

	#finding the last index in the allele space, the ID of the newly formed allele will be that +1
	splitNames<-strsplit(alspace$name,split="_")
	lastIndex<-max(sapply(splitNames,function(vec) as.integer(vec[2])))

	#the new allele has the same in input expression and linkage group to the original, only the name is different
	inExp<-alspace$exp[alnames==targetAllele]
	newName<-paste(strsplit(targetAllele,split="_")[[1]][-2],lastIndex+1,sep="_")
	group<-alspace$group[alnames==targetAllele]
	alspace[length(alnames)+1,]<-c(newName,inExp,group)

	#copy column and row in the input Matrix
	inMat<-rbind(inMat,inMat[targetAllele,])
	inMat<-cbind(inMat,inMat[,targetAllele])
	#and re-write the column and row names, they will now include the new allele name
	colnames(inMat)<-as.character(alspace$name)
	rownames(inMat)<-as.character(alspace$name)

	#This is the important bit: find all the alleles of the picked output gene and set their inputs from the new allele to 0
	alsWorld<-grep(outputGene,alspace$name,value=T)
	inMat[newName,alsWorld]<-0

	#now put the new allele into the allele vector
	alvec[which(alvec==targetAllele)[1]]<-newName

	if (printIt){cat(targetAllele," loses its output to ",outputGene," the new allele is named ",newName,"\n",sep="")}
	}
	return(list(inMat=inMat,alspace=alspace,alvec=alvec))
}
