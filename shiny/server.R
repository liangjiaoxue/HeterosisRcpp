library(shiny)

# Define server logic required to draw a histogram
# by Jake Reeves
# Modified from https://github.com/VeraPancaldi/Heterosis-GRN-simulation


shinyServer(function(input, output) {
	
	# place to put all events that only need to happen once
	#setwd("F:\\Projects\\Heterosis\\shiny")

	dep_packages = c("parallel","BoolNet","sfsmisc","igraph","compiler","Rcpp","RcppArmadillo")

	for (pack in dep_packages){
	  if(pack %in% rownames(installed.packages()) == FALSE) {install.packages(pack)}
	}
	
			### run the job
	library("parallel")
	library("BoolNet")
	library("sfsmisc")
	library("igraph")
	library("compiler")
	library("Rcpp")
	library("RcppArmadillo")
	
	source("heterosis\\sf.sw.directed_matrix.r")
	source("heterosis\\cleanWorld.r")
	source("heterosis\\evolveAlvec_dip.r")
	source("heterosis\\setEnv.r")
	source("heterosis\\natSelect.r")
	source("heterosis\\phenoEmph.r")
	source("heterosis\\calcAttr.r")
	source("heterosis\\Recmat.r")
	source("heterosis\\mutateGen.r")
	#source("heterosis\\killGenemat.r")
	Rcpp::sourceCpp("heterosis\\cpp\\killGenemat.cpp")
	#source("heterosis\\dupGenemat.r")
	Rcpp::sourceCpp("heterosis\\cpp\\dupGenemat.cpp")
	source("heterosis\\addEdgemat.r")
	source("heterosis\\loseOutmat.r")
	source("heterosis\\loseInmat.r")
	source("heterosis\\build.ntwk.r")
	source("heterosis\\netPlotmat.r")
	source("heterosis\\remPH.r")
	source("heterosis\\toNet.r")
	source("heterosis\\quantDomEpis.R")
	
	


	# click 'New Gametes' button
	observeEvent(input$gametes, {
		nodes <- input$nodes
		adjust <- input$adjust
		gaps <- input$gaps
		rep <- input$rep
		pops <- input$pops
		max.popn <- input$max.popn


		
		no_cores <- detectCores() - 1
		cl <- makeCluster(no_cores)
		proc.time()

		### run scripts


		prob=c(1,1,1,1,1,1)
		envmts=NULL
		emph=NULL
		fname="test"
		printInherit=FALSE
		evolve="MH"
		testDom=FALSE
		savePops=TRUE

		emph = NULL
		if (is.null(emph)){
			startEmph<-c(1,1,1)
			newEmph<-c(1,1,1)
			newnewEmph<-c(1,1,1)
		}else{
			startEmph<-emph[[1]]
			newEmph<-emph[[2]]
			newnewEmph<-emph[[3]]
		}

		envmts = NULL
		if (is.null(envmts)){
			envmts<-list(c(0,1,0),c(1,0,0),c(0,0,1))
		}
		envNumber<-length(envmts[[1]])
		reps<-100
		daughters<-4

		#fix starting states of length N=nodes
		startStates<-matrix(0,nodes,reps)
		for (i in 1:reps){
			startStates[,i]<-sample(0:1,nodes,replace=T)
		}

		#create initial gametes, set output genes etc
		startWorld<-create.allele.ntwk(nodes,2,2,0.5,0.5,envNumber)
		inMat<-startWorld$inMat
		origamete<-startWorld$alvec
		alspace<-startWorld$alspace
		#ntwk<-build.ntwk(origamete,alspace,inMat,plotIt=TRUE)

		output$networkOut <- renderPlot({
		    par(mfrow=c(1,1))
			build.ntwk(origamete,alspace,inMat,plotIt=TRUE)
		},
		height = 750, width = 650
		)

  	})

})