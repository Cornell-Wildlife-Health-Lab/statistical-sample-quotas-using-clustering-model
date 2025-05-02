# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Title:          Statistical Sample Size Quotas Using Clustering Model
# Programmer:     J. Booth (Original Code)
#			            B. Hanley (Error Handling)
# Date:           2025
# Location:       Cornell Wildlife Health Lab
# License:        MIT
#
# This script converts the computations in 
# Hanley et al. (2024) into a CWD Data Warehouse model. 
#
# The citation for Hanley et al. (2024) is: 
# Hanley, B.J., Booth, J.G., Hodel, F.H., Thompson, N.E., 
# Bloodgood, J.C.G., Dion, J.P., Van de Berg, S., 
# Gonzalez-Crespo, C., Huang, Y., Wang, J., Miller, L.A., 
# Hollingshead, N.A., Peaslee, J.L., Schuler, K.L. 2024. 
# Sample size calculator for declaring a population free 
# of infectious disease (Version 1) [Software]. Cornell 
# University Library eCommons. doi: 10.7298/ka5p-bj90
#
# This code was written in R version 4.4.1 
# (2024-06-14 ucrt) -- "Race for Your Life"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x645. Refer to the README 
# for additional information and other technical details. 
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Load the R Packages. 
    library(cubature) 
    library(VGAM) 
    library(purrr)
    library(ipc)
    library(future)
    library(promises)
    library(extraDistr)
    library(tidyverse) 

# Reusable Functions

add_item_to_json_array=function(file_path, new_item) {
    # This is a bespoke function that adds a string representing a JavaScript
    # Object to the attachments.json file containing an array listing the model
    # outputs. Although this function has error handling for a missing file and
    # improperly formed file, the existence of the file and a list enclosed in
    # brackets in that file are expected.

    # Check if the file exists.
    if (!file.exists(file_path)) {
        # Write to error log and exit script with an error.
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' not found.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Read the file content.
    file_content=readChar(file_path, file.info(file_path)$size)

    # Check if the file is empty.
    if (nchar(file_content) == 0) {
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' is empty.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Remove the last closing bracket.
    file_content=substr(file_content, 1, nchar(file_content) - 1)

    # Create a function for adding double quotes around text.
    double_quote=function(x) {paste0('"', x, '"')}

    # Create the new item as a JSON string using shQuote with double_quote.
    new_item_json=paste0(
        "{",
        paste(
            sapply(names(new_item), double_quote),
            sapply(as.character(new_item), double_quote),
            sep=":", collapse=","
        ),
        "}"
    )

    # Add comma, new item, and closing bracket to the file content
    file_content=paste0(file_content, ",", new_item_json, "]")

    # Write the updated data back to the file
    writeLines(file_content, file_path, sep="")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initiate the functions. 

# See equation after eq. 5.
# This function is for the case X=0.
probX0=function(x,n,Pse){
  p=x[1]
  r=x[2]
  Qse=1-Pse
  k=0:n
  alpha=p*(1-r)/r
  beta=(1-p)*(1-r)/r
  sum(Qse^k*dbbinom(k,n,alpha,beta))}

# Get the probability. 
probX02=function(x,n,Pse){
  a=x[1]*(1-x[2])/x[2]
  b=(1-x[1])*(1-x[2])/x[2]
  Qse=1-Pse
  k=0:n
  p=(k+a)/(a+n+b); r=1/(1+a+n+b)
  sum(Qse^k*dbbinom(k,n,a,b))}

# Get the probability. 
predys=function(p,r,n,Pse){
  Qse=1-Pse
  k=0:n
  alpha=p*(1-r)/r
  beta=(1-p)*(1-r)/r
  pr=Qse^k*dbbinom(k,n,alpha,beta)
  pr/sum(pr)}

# Equation 7 with modification for sensitivity less that 1.
probX0cl=function(x,cts,Pse,prior){
  d=dim(cts)[2]
  pX0=0
  for (i in 1:d) {pX0=pX0+cts[2,i]*log(probX0(x,cts[1,i],Pse))}
  ap=prior$ap
  bp=prior$bp
  ar=prior$ar
  br=prior$br
  exp(pX0)}

# Start value of pi=0.5 and rho=0.5 and minimize the negative log of 
# the function (same as maximizing the function).
max_probX0cl=function(cts,Pse,prior){
  min_neg_log_lik=
    optim(
      par=c(0.5, 0.5), 
      fn=function(x) { -log(probX0cl(x,cts,Pse,prior)) },
      lower=c(1e-7,1e-7),
      upper=c(1-1e-7,1-1e-7),
      method="L-BFGS-B")
  max_pdf=exp(-min_neg_log_lik$value) # Convert back to linear scale.
  return(max_pdf)}

# Calculate the sample size. 
calcn=function(n,prob,a) {
  ss=rep(0,length(dim(prob)[1]))
  for (i in 1:dim(prob)[1]) {
    warn=getOption("warn")
    options(warn=-1)
    k=min(which(prob[i,]>a))
    options(warn=warn)
    # Need to take care of the case when k=1
    if (k==1) ss[i]=min(n)
    # and the case where none of the sample sizes are enough.
    if (k==Inf) {
      ss[i]=max(n)}
    else {
      b1=(prob[i,k]-prob[i,k-1])/(n[k]-n[k-1])
      b0=prob[i,k]-b1*n[k]
      ss[i]=(a-b0)/b1}}
  round(ss)}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Continue the log started with the Python script. 

    # Model log file started with Python data processing script. 
    model_log_filepath=file.path("", "data", "attachments", "info.html")
    line='<h3>Model Execution</h3>'
    write(line,file=model_log_filepath,append=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the Required Data. 
    
# There is one required file that must always be imported. 
# 1. A .csv file named model_input.csv. 
    
# Read in the (Required) Parameters file. 
    filepath=file.path("", "data", "model_input.csv")
    SubAdmin_Standard=readr::read_csv(filepath)
    # Note: The model_input has to exist b/c Python generated it and b/c model 
    # has to create it. Therefore, this error handling is in the python code and 
    # this R script will not run if it does not exist. 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Conduct preliminary cleaning. 
    
  # Create data frame.
  SubAdmin_Standard=as.data.frame(SubAdmin_Standard) 

	# Name the columns. 
	colnames(SubAdmin_Standard)=c("SubAdminID","name","full_name","Area","N","Correlation","ClusterSize","Sensitivity")
	
	# Get a comprehensive vector of IDs. 
	SubAdmin_Standard_IDs=as.data.frame(SubAdmin_Standard$SubAdminID)
	colnames(SubAdmin_Standard_IDs)=c("SubAdminID")
	
	# Remove name, full name, and area. 
	ModelMatrix=SubAdmin_Standard[,-c(2,3,4)]

    # Report that the data loaded. 
    line="<p>Matrix successfully loaded.</p>"
    write(line,file=model_log_filepath,append=TRUE) 

  # Get the cluster size. 
  ModelMatrix$ClusterSize=as.numeric(floor(ModelMatrix$ClusterSize))
    # Log the message.
    line='<p>The model loaded cluster sizes.</p>'
    write(line,file=model_log_filepath,append=TRUE)

  # Get the sensitivity. 
  ModelMatrix$Sensitivity=as.numeric(ModelMatrix$Sensitivity)
    # Log the message.
    line='<p>The model loaded sensitivities.</p>'
    write(line,file=model_log_filepath,append=TRUE) 

  # Get the correlation. 
  ModelMatrix$Correlation=as.numeric(ModelMatrix$Correlation)
    # Log the message.
    line='<p>The model loaded correlations.</p>'
    write(line,file=model_log_filepath,append=TRUE) 

  # Get the initial N size. 
  ModelMatrix$N=as.numeric(floor(ModelMatrix$N))
    # Log the message.
    line='<p>The model loaded total population size.</p>'
    write(line,file=model_log_filepath,append=TRUE) 

  # Create the first new variable. 
  # Adjust the N size to be divisible by cluster size.  
  ModelMatrix$Nclust=floor(ModelMatrix$N/ModelMatrix$ClusterSize)

  # Address the division. 
  # If cluster size is zero, remove division by zero and replace it with 0. 
  # If both cluster size and n size are zero, and replace it with 0.    
  for (tt in 1:length(ModelMatrix$Nclust)){
    if (is.infinite(ModelMatrix$Nclust[tt])|is.nan(ModelMatrix$Nclust[tt])){
      ModelMatrix$Nclust[tt]=0
    }
  }
	    
  # Create another new variable. This time, adjust the total population to contain only whole clusters. 
  ModelMatrix$N_Adjusted=ModelMatrix$Nclust*ModelMatrix$ClusterSize
    # Log the message.
    line='<p>Successfully adjusted population sizes to be divisible by whole clusters.</p>'
    write(line,file=model_log_filepath,append=TRUE) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Conduct checks for each area. 
      # Get the number of areas. 
      NumberAreas=length(ModelMatrix$SubAdminID)

      # Check #1. 
    	# Remove the row if there are no hosts (i.e., if N_Adjusted is zero). 
	    CasesAdjusted=0 
      for (d in 1:NumberAreas){
	    if (ModelMatrix$N_Adjusted[d]==0){
	    CasesAdjusted=CasesAdjusted+1}}
	    ModelMatrix2=ModelMatrix[ModelMatrix$N_Adjusted!=0,]
	    
          # Log the message about check #1.
          line=paste("<p>Out of ",NumberAreas," subadministrative areas, ", CasesAdjusted," did not have sufficient host populations. We removed these areas from the computation. </p>")
          write(line,file=model_log_filepath,append=TRUE) 
          
		          # If no rows are left after check #1, kill the code.  
		          Dim=as.numeric(nrow(ModelMatrix2))
		          
		          if (Dim==0){
		              # Print error message to log file.
		              line="<p>We're sorry. You don't have a single area that has sufficient hosts to run this sample size model.</p>"
		              write(line,file=model_log_filepath,append=TRUE) 
		              # Kill the code.  
		              quit(status=70)} 

	    # Clean up the environment. 
	    rm(ModelMatrix)
 
      # Check #2. 
	    # Make sure that the adjusted population has at least 100 individuals. 
	    # If not, adjust the population to be equivalent to the lowest whole number of clusters needed to reach 100 hosts. 
	    # Tell the user how many cases were adjusted.
	        # First initialize the new variables. 
	        ModelMatrix2$N_Adjusted_2=ModelMatrix2$N_Adjusted
	        ModelMatrix2$Nclust_Adjusted=ModelMatrix2$Nclust
	        # Then adjust as necessary.
	        CasesAdjusted=0 
          for (d in 1:Dim){
	          if (ModelMatrix2$N_Adjusted[d]<100){
		        # Find the lowest combination of whole clusters that exceed 100.
		        # Start with 1 cluster.
		        multiplier=1
		        while (multiplier*ModelMatrix2$ClusterSize[d]<100){
		        multiplier=multiplier+1}
	          ModelMatrix2$N_Adjusted_2[d]=multiplier*ModelMatrix2$ClusterSize[d]
	          ModelMatrix2$Nclust_Adjusted[d]=multiplier
	          CasesAdjusted=CasesAdjusted+1}}
                # Log the message about Check #2.
                line=paste("<p>Out of ",Dim," subadministrative areas that had 
                           hosts, ", CasesAdjusted," had host populations with fewer 
                           than 100 animals. We adjusted the total population in 
                           these areas to satisfy the need for 100 hosts in the 
                           population as well as the need to have complete clusters. </p>")
                write(line,file=model_log_filepath,append=TRUE)

      # Check #3. 
    	# Make sure cluster size is at most the population size. Adjust as necessary.
    	# Tell the user how many cases were adjusted.
      # First start by defining new variables. 
	    ModelMatrix2$ClusterSize_Adjusted=ModelMatrix2$ClusterSize 
	    ModelMatrix2$Nclust_Adjusted_2=ModelMatrix2$Nclust_Adjusted
	    # Then adjust as necessary. 
	    CasesAdjusted=0   
    	for (d in 1:Dim){
    	  if (ModelMatrix2$ClusterSize[d]>ModelMatrix2$N_Adjusted_2[d]){
    	  ModelMatrix2$ClusterSize_Adjusted[d]=ModelMatrix2$N_Adjusted_2[d]
      	ModelMatrix2$Nclust_Adjusted_2[d]=1
	      CasesAdjusted=CasesAdjusted+1}}
            # Log the message about Check #3.
            line=paste("<p>Out of ",Dim," subadministrative areas that had 
                       hosts, ", CasesAdjusted," had cluster size that exceeded 
                       population size. We adjusted cluster size to be at most 
                       the population size.</p>")
            write(line,file=model_log_filepath,append=TRUE) 

      # Pass only the clean matrix to the model. 
      CleanModelMatrix=as.data.frame(cbind(
      ModelMatrix2$SubAdminID,
      ModelMatrix2$Correlation,
      ModelMatrix2$Sensitivity,
      ModelMatrix2$N_Adjusted_2,
      ModelMatrix2$ClusterSize_Adjusted,
      ModelMatrix2$Nclust_Adjusted_2))
      colnames(CleanModelMatrix)=c("SubAdminID","Correlation","Sensitivity","N_checked","Csize_checked","Nclust_checked")    
 
	  # Clean up the environment. 
	  rm(ModelMatrix2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Print the clean model input matrix. 

	    # Set the working directory. 
      setwd("/data/attachments")

      # Write the matrix.
      write.csv(CleanModelMatrix, "SampleSizeQuotasInput.csv", row.names=FALSE)
      
      # Modify the attachments.json file to include the Model Matrix.
      setwd("/data")
      
      # Define the new item.
      attachment_item=list(
            filename="SampleSizeQuotasInput.csv", 
            content_type="text/csv", 
            role="downloadable")
            add_item_to_json_array("attachments.json", attachment_item)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preserve the Seed ahead of the randomized simulation. 
    set.seed(123)
    
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
# Get the sample quotas for every sub-admin area and disease level. 

	# Initialize desired disease levels. 
	DISEASE_LEVEL=c(0.01,0.015,0.020,0.030,0.040,0.050)

  # Initialize a storage matrix.  
  SampleSizeNeeded=matrix(0,length(CleanModelMatrix$SubAdminID),length(DISEASE_LEVEL))

	# Run the outer loop for the DL disease levels. 
	for (DL in 1:length(DISEASE_LEVEL)){
    
  		# Run the inner loop for the d subadmin areas.     
  		for (d in 1:length(CleanModelMatrix$SubAdminID)){
  		  
    			# Get the inputs for the dth subadmin area. .
    			N=as.numeric(CleanModelMatrix$N_checked[d])
    			Csize=as.numeric(CleanModelMatrix$Csize_checked[d])
    			Pse=as.numeric(CleanModelMatrix$Sensitivity[d])
    			rho=as.numeric(CleanModelMatrix$Correlation[d])
    			pi0=DISEASE_LEVEL[DL]
			    Nclust=as.numeric(CleanModelMatrix$Nclust_checked[d])

    			# Hard coded simulation parameters. 
    			Nsim=1000
    			Nsub=1
    			alevel=0.95

    			# Make sure not to take more samples than items to sample. 
    			n=unique(c(min(1,N),min(100,N),min(200,N),min(300,N)))
    			prob=matrix(0,length(pi0),length(n))
    
    			# If the user enters rho=0. 
    			if (rho==0) {for (pp in 1:length(pi0)) {prob[pp,]=pbbinom(N*pi0[pp],N-n,1,n+1)}}
    
    			# If the user enters 0<rho<1. 
    			if (0<rho) {
      			prior=list(ap=1.0,bp=9.0,ar=10*rho,br=10*(1-rho))
      			j=0
      			for (k in n) {
        			j=j+1
        			# Simulate posterior predictive distribution.
        			Y=NULL
        			count=c()
        			for (s in 1:Nsim) {
          			CLS=rep(Csize,Nclust)
          			cl=rep(1:Nclust,times=Csize) # population 
          			# Simple random sampling.
          			sam=sample(cl,k)
          			tcl=c(tabulate(sam),rep(0,Nclust-max(sam))) # tabulate cluster sizes
          			UCLS=CLS[tcl==0] # size of unobserved clusters
          			# How many clusters were sampled zero times, once, twice, etc
          			cts=rbind(sort(unique(tcl)),as.numeric(as.matrix(table(tcl))[,1]))
          			if (cts[1,1]==0) {
            		# Number of unobserved clusters.
            		cts0=cts[2,1] 
            		cts=cts[,2:dim(cts)[2]]}
          			if (length(unique(sam))==k) {
            		cts=matrix(cts,2,1)}
          			# Generate p and r from posterior.
          			x=c(rbeta(1,prior$ap,prior$bp),rbeta(1,prior$ar,prior$br))
          			w=runif(1,0,1)
          			M=max_probX0cl(cts,Pse,prior)
          			count[s]=0
          			while(w>probX0cl(x,cts,Pse,prior)/M) {
            		x=c(rbeta(1,prior$ap,prior$bp),rbeta(1,prior$ar,prior$br))
            		w=runif(1,0,1)
            		count[s]=count[s]+1}
          			p=x[1]
          			r=x[2]
          			a=p*(1-r)/r
          			b=(1-p)*(1-r)/r
          			# Initiate the sub-sampling loop.
          			for (u in 1:Nsub) {
            		s1=0
            		s2=0
            		# Predict the sampled clusters. 
            		for (i in 1:dim(cts)[2]) {
              		pr=predys(p,r,cts[1,i],Pse)
              		y1=sample(0:cts[1,i],cts[2,i],prob=pr,replace=TRUE)
              		number=as.numeric(cts[2,i])
              		size=as.numeric(CLS[tcl==cts[1,i]]-cts[1,i])
              		y2=rbbinom(number,size,y1+a,cts[1,i]+b-y1) 
              		s1=s1+sum(y1)
              		s2=s2+sum(y2)}
            		# Predict the un-sampled clusters (if un-sampled clusters exist).
            		if (exists("cts0")){
              		y0=rbbinom(cts0,UCLS,a,b)
              		s2=s2+sum(y0)}
            		Y=rbind(Y,c(s1,s2))
          			} # End of sub-sampling loop.
          			if (exists("cts0")){rm(cts0)}
        			} # End of simulation loop.
        			y=apply(Y,1,sum)
        			for (pp in 1:length(pi0)) {prob[pp,j]=sum(y<=sum(CLS)*pi0[pp])/(Nsim*Nsub)}
      			} # End of number of locations to compute on the plot loop. 
 				} # End of 0<rho loop.
    
    			# Calculate the sample sizes needed at each location of interest. 
    			ss=calcn(n,prob,alevel)
    			SampleSizeNeeded[d,DL]=ss[1]
    
		} # End subadmin loop.   
	} # End disease level loop. 
  	
  	# Log the message.
  	line="<p>Model ran as expected.</p>"
  	write(line,file=model_log_filepath,append=TRUE) 

    # Label the rows and columns.
    SampleSizeNeeded=as.data.frame(cbind(CleanModelMatrix$SubAdminID,SampleSizeNeeded))
    colnames(SampleSizeNeeded)=c("SubAdminID","1%","1.5%","2%","3%","4%","5%")

    # Append the outputs back to the standard matrix. 
    ComprehensiveOutput=left_join(SubAdmin_Standard_IDs,SampleSizeNeeded,by='SubAdminID')

    # Write the output the attachments working directory.
    setwd("/data/attachments")
    # TODO can we turn off row names?
    # TODO Can we add subadmin area names
    
    write.csv(ComprehensiveOutput, "SampleSizeQuotasOutput.csv", row.names=FALSE)
		
    # Modify the attachments.json file to include the Model Matrix.
    setwd("/data")
		
    # Define the new item.
    attachment_item=list(
      filename="SampleSizeQuotasOutput.csv", 
      content_type="text/csv", 
      role="downloadable")
      add_item_to_json_array("attachments.json", attachment_item)

# bh