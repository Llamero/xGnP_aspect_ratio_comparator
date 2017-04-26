#Set working directory to file directory with data
setwd("E:/Garg Lab/Server output 1/Final Figure Subset")

#Set the desired volume cutoff (cubic microns)
minAreaCutoff<-700

#number of bins in histogram
nBins<-20

#This function counts the total number of bundles in each file and returns the count
countObjects<-function(fileName){
	#Load the file into a temporary matrix
	fileMatrix<-read.table(fileName, sep="\t", header=TRUE)
	
	#Return the number of samples in the file
	count<-nrow(fileMatrix)
	return(count)
}

#This function fills a matrix with the corresponding measurements in an excel file
excelToMatrix<-function(fileName, maxCount){
	#Load the file into a temporary matrix
	fileMatrix<-read.table(fileName, sep="\t", header=TRUE)
	
	#Retrieve the desired measurements
	volumeVector<-fileMatrix$Volume
	boundingBoxWidthVector<-fileMatrix$B.width #Hyphens are replaced with "."
	boundingBoxHeightVector<-fileMatrix$B.height #Hyphens are replaced with "."
	
	
	#Fill the remaining vector with NA so that sapply creates a matrix (all returned vectors are of equal length)
	for (a in length(volumeVector):maxCount){
		volumeVector[a+1]<-NA
		boundingBoxWidthVector[a+1]<-NA
		boundingBoxHeightVector[a+1]<-NA
	}
	
	#Calculate the area and aspect ration of each bounding box
	boundingBoxArea<-boundingBoxWidthVector*boundingBoxHeightVector
	boundingBoxAspectRatio<-boundingBoxHeightVector/boundingBoxWidthVector
	
	#Concatenate the results into one vector
	resultsVector<-cbind(volumeVector, boundingBoxArea, boundingBoxAspectRatio)
	
	#Return the desired measurements as one concatenated vector
	return(resultsVector)
}

createBoxplot<-function(fileNameList, dataMatrix, measurement, logAxis, outlierBoolean, yLimits, histogramBins, csvTitle){
	#Extract the sample ID for each file by removing the prefix and extension and set as the column names in the new matrix
	sampleIDvector<-sapply(fileNameList, gsub, pattern="\\.xls$", replacement="")
	sampleIDvector<-sapply(sampleIDvector, gsub, pattern="\\.lif", replacement="")
	colnames(dataMatrix)<-sampleIDvector

	#Create a vector of all sample IDs so that replicates can be pooled together
	#initize variables and vectors
	index<-1 
	sampleNameVector<-vector(,0)
	sampleDataList<-list()

	for(a in 1:length(sampleIDvector)){
		#Remove the numeric sample identifier suffix from the name
		sampleID<-gsub(" - Series [0-9]+", "", sampleIDvector[a])
		#On the first iteration, add the sample and count up the index
		if(a == 1){
			sampleNameVector[index]<-sampleID
			sampleDataList<-dataMatrix[,a]
		}
		
		#On all following iterations, check to see if the same sample is found, and if so, add it to the vector 
		else if(sampleID == sampleNameVector[index]){
			sampleDataList<-c(sampleDataList, dataMatrix[,a])
		}
		
		#If a new sample is found, create a new list and add it to the list of lists
		else{
			#If index is 1 then create a new list of lists
			if(index == 1){
				listOfLists<-list(sampleDataList)
			}
			#Otherwise, add the current list to the list of lists
			else{
				listOfLists[[length(listOfLists)+1]]<-sampleDataList
			}
			
			#Increase the index counter
			index<-index+1
			sampleNameVector[index]<-sampleID
			sampleDataList<-dataMatrix[,a]
					
		}
	}
	#Add the last list to the list of lists
	listOfLists[[length(listOfLists)+1]]<-sampleDataList

	#convert the list of lists to a matrix
	sampleDataMatrix<-do.call(cbind, lapply(listOfLists, unlist))
	colnames(sampleDataMatrix)<-sampleNameVector

	#Remove all volume measurements that are below the set cutoff
	sampleDataMatrix<-replace(sampleDataMatrix, sampleDataMatrix < minVolumeCutoff, NA)
	
	#Create a histogram of the data
	
	histogramMatrix<-matrix(data = 0, nrow = length(histogramBins)-1, ncol = ncol(sampleDataMatrix))
	for(a in 1:ncol(sampleDataMatrix)){
		cutVector<-cut(sampleDataMatrix[,a], histogramBins, right = FALSE)
		histogramVector<-table(cutVector)
		normalizedHistVector<-histogramVector/sum(histogramVector, na.rm = TRUE)
		histogramMatrix[,a]<-normalizedHistVector
	}
	
	csvMatrix<-cbind(histogramBins[2:length(histogramBins)], histogramMatrix)
	colnames(csvMatrix)<-c("Bins", sampleNameVector)
	write.csv(csvMatrix, file = csvTitle)

	#Create a notched box plot for each sample
	dev.new()
	par(mar = c(15,6,3,3))
	boxplot(sampleDataMatrix, log=logAxis, outline = outlierBoolean, las = 2, names = sampleNameVector, notch = TRUE, ylim = yLimits, ps = 1, cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1, bty="n")
	mtext( measurement, side=2, line = 3, cex = 1)
	
	dev.new()

	par(mar=c(4.2, 3.8, 0.2, 0.2))
	plot(histogramMatrix[,1], type="o", ylim=range(histogramMatrix, na.rm=TRUE), axes=F, ann=T, xlab=measurement, ylab="Freq", cex.lab=1, lwd=2, pch=1)
	axis(1, at=seq(0.5, (length(histogramBins)-0.5), 1), labels=histogramBins, cex.axis = 1)
	axis(2,  labels=TRUE, cex.axis = 1)
	for(a in 2:ncol(sampleDataMatrix) ){
		lines(histogramMatrix[,a], type="o", lty=a, lwd=2, pch=a)
	}
	box()
	
}

#Get all file names that end with a *.xls file extension
fileNameList<-list.files(pattern = "\\.xls$",  ignore.case=TRUE)

#Count the number of results in each file
fileObjectCounts<-sapply(fileNameList, countObjects, simplify = TRUE)

#Find the largest number of object counts (this value will be used to setup a matrix to store all volume measurements)
maxCount<-max(fileObjectCounts)

#Add the volume measurements to  each corresponding column in the a new matrix
allResultsMatrix<-sapply(fileNameList, excelToMatrix, maxCount, simplify = TRUE, USE.NAMES = FALSE)

#Parse the results matrix into sub matrices containing single measurements
allVolumeMatrix<-allResultsMatrix[1:(maxCount+1),]
allAreaMatrix<-allResultsMatrix[(maxCount+2):(2*(maxCount+1)),]
allAspectRatioMatrix<-allResultsMatrix[(2*(maxCount+1)+1):(3*(maxCount+1)),]

#Remove all mesurements the correspond to below the bounding box minimum area cutoff
for(a in 1:nrow(allVolumeMatrix)){
	for(b in 1:ncol(allVolumeMatrix)){
		area<-allAreaMatrix[a,b]
		if(is.na(area)){
			area<-0
		}
		if (area < minAreaCutoff){
			allAreaMatrix[a,b]<-NA
			allAspectRatioMatrix[a,b]<-NA
		}
	}
}

#Scale the area to microns
allAreaMatrix<-allAreaMatrix*0.0144

histogramBins = seq(0,5,5/nBins)
createBoxplot(fileNameList, allAspectRatioMatrix, "Aspect Ratio Distribution", "", FALSE, c(1,4), histogramBins, "Aspect Ratio Distribution Histogram")
histogramBins = round(10^(seq(log10(10), log10(200), length.out = nBins)))
createBoxplot(fileNameList, allAreaMatrix, expression(paste(plain("Graphene Particle Bounding Box Area (μm") ^ plain(" 2"), plain(")"))), "y", FALSE, c(10, 200), histogramBins, "Graphene Particle Bounding Box Area")




#c(min(sampleDataMatrix, na.rm = TRUE), max(sampleDataMatrix, na.rm = TRUE))

