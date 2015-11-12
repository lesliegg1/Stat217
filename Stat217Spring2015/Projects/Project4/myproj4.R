
#require(dplyr)

### The following builds the dataset
#fracture <- matrix(c(rep("Progressive Compression",180), rep("Resistant Planar",79), 
 #                    rep("Sudden Planar", 153), rep("Sudden Collapse", 193),
  #                   rep("Break", 30), rep("yes", 18), rep("no", 162), rep("yes", 6),
                      rep("no", 73), rep("yes", 66), rep("no", 87), rep("yes", 96), 
   #                   rep("no", 97), rep("yes",1), rep("no", 29)), nrow=635, ncol=2, 
    #               byrow=FALSE)

#fracture <- as.data.frame(fracture)
#names(fracture) <- c("fracture", "trigger")
#fracture <- arrange(fracture, trigger)


#taps <- c(rep("Easy",85), rep("MedEasy",80), rep("Medium",81), rep("MedHard",94), 
 #               rep("Hard",108), 
  #        rep("Easy", 41), rep("MedEasy", 47), 
   #       rep("Medium",45), rep("MedHard",32), 
    #      rep("Hard", 22))
#taps <- as.data.frame(taps)
#names(taps) <- c("taps", "trigger")
#taps <- arrange(taps, trigger)

#slides <- cbind(taps,fracture)
#slides$taps <- factor(slides$taps, levels=c("Easy", "MedEasy", "Medium", "MedHard", "Hard"))
#slides$fracture <- factor(slides$fracture, levels=c("Sudden Collapse", "Sudden Planar",
 #         "Resistant Planar", "Progressive Compression", "Break"))

#slides <- slides[sample(1:nrow(slides)), ]

require(mosaic)

slides <- read.csv("~/Documents/Stat217Spring2015/Projects/Project4/slides.csv", head=T)


                      
#For fracture
frac.tally <- tally(~fracture+trigger, data=slides)
frac.tally
mosaicplot(frac.tally)
chisq.test(frac.tally)
chisq.test(frac.tally)$residuals
mosaicplot(frac.tally, shade=T)
                      
                      #For number of taps
                      taps.tally <- tally(~taps+trigger, data=slides)
                      taps.tally
                      mosaicplot(taps.tally)
                      chisq.test(taps.tally)
                      chisq.test(taps.tally)$residuals
                      mosaicplot(taps.tally, shade=T)
                      
                      #tableplot
                      require(tabplot)
                      tableplot(slides[,c(2,1,3)])
                      tableplot(slides)
                      
##In a stratified random systematic sample, one reach within each stream is randomly 
                      chosen and subsequent reaches are sampled systematically. 
                      A stratified random systematic sample achieves spatial balance 
                      in this context by ensuring that the reaches sampled are spread 
                      out evenly across the stream. The sample estimates are calculated 
                      as shown in the previous section \ref{sreq}.
