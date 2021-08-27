### this script is used for th output file from FociQuant analysis 
### for direct import into R.
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('ggbeeswarm')
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
###### This defines input file, please change the filename to your file if needed.
inputfile = read_delim("resultsctf19x.txt", delim = "\t", col_names = TRUE)


###### arrange data for plotting
FociQuant <- function (inputfile) {
x = inputfile
x2 = x[c(2,4)] #### select Mean (can be changed to median by replacing 4 with 12)
x3 = separate(x2, col = Label, into = c("Label", "frame"),sep = "--")
x4 = separate(x3, col = Label, into = c("Label", "image"),sep = "-")
v1 = gsub( "2nd channel of",  "channel2", x4$frame)
Frame = gsub( "2nd channel background of",  "channel2backgorund", v1)
x5 = mutate(x4,Frame)
x6 = x5[c(1,2,4,5)]
x7 = separate(x6, col = Frame, into = c("Frame", "number"),sep = " ")
x
x8 = spread(x7, key = "Frame", value = "Mean")
x7
x9 = mutate (x8, Channel1 = x8$SPB - x8$SPB_background, Channel2 = x8$channel2 - x8$channel2backgorund)
}

H = FociQuant(inputfile)
##### plotting Channel1 intensity angainst groups ######
p <- ggplot(H,aes(x=Label, y=Channel1, fill = Label)) + 
  geom_violin() + theme_classic()
p
p + geom_quasirandom(shape=16, position=position_jitter(0.2))+ scale_fill_brewer(palette = "BuPu")


#### to compare channel1 and channel2 intensities####
H2 = gather (H, key = channel, value = Intensity, 8:9)
q <- ggplot(H2, aes(x=Label, y=Intensity, fill=channel)) + 
  geom_violin()+
  facet_wrap(~channel, scale="free")
q + geom_quasirandom(shape=16, position=position_jitter(0.2))+ scale_fill_brewer(palette = "BuPu")

##### to download dataframe #####
write_csv(H, "H.csv")
