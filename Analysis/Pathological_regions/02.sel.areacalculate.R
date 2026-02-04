args <- commandArgs();
if(length(args) != 9 || !file.exists(args[6])){
        write(paste("Rscript",unlist(strsplit(args[4],"="))[2],"<FI:type.pos.txt>","<FI:image>","<STR:target_type>","<STR:prefix>",sep=" "),stderr());
        q();
}
fileName <- args[6]
imageName <- args[7]
target <- args[8]
prefix <- args[9]

#fileName <- "d.txt"; imageName <- "test.fill.png"; target <- "N"; prefix <- "test"
#fileName 
library(ggplot2)
library(png)
dat <- read.table(fileName,sep = "\t")
msk <- readPNG(imageName)
colorCutoff <- 10/255

r1 <- range(dat$V2)
r2 <- range(dat$V3)
s <- dim(msk)
ox <- 0
oy <- 0
k <- 1

if(s[1]!=s[2] || s[1]!=1000){
	write(paste(imageName,"Wrong picture sizes"),file=stderr())
	q()
}

if(r1[2]-r1[1]>r2[2]-r2[1]){
	k <- (r1[2]-r1[1])/(s[1])
}else{
	k <- (r2[2]-r2[1])/(s[2])
}


idx <- unlist(lapply(seq(nrow(dat)),
                     function(i){
                       x <- floor((dat$V2[i]-r1[1])/k+ox);y <- s[2]-floor((dat$V3[i]-r2[1])/k+oy)
                       x <- max(1,x);y <- max(1,y);
                       if(msk[y,x,1]+msk[y,x,3]+msk[y,x,2]<=colorCutoff){T}else{F}}))


dat$V4 <- idx

pic <- ggplot(dat[order(dat$V4),],aes(V2,V3,color=V4))+geom_point(cex=0.1)+coord_fixed()+theme_void()+theme(legend.position = "none")+scale_colour_manual(values=c("#EEEEEE","#FFDD00"))
ggsave(paste0(prefix,".sel.png"),width=1000,height=1000,plot=pic,units="px")

dat <- dat[idx,c(1,2,3)]
write.table(dat,file=paste0(prefix,".sel.txt"),sep="\t",quote=F,row.names=F,col.names=F)



center2center <- 500 ### nm

#### modify site 1
ratio <- 1e7


l <- max((r1[2]-r1[1]),(r2[2]-r2[1]))
physicalArea <- l*center2center/ratio #width/height for the image, unit: cm
print(physicalArea)
physicalArea <- physicalArea*physicalArea

RGB <- as.numeric(msk[,,1])+as.numeric(msk[,,2])+as.numeric(msk[,,3])
imageArea <- sum(RGB<=colorCutoff)
print(length(RGB))
#### modify site 2
write(paste(imageName,format(100*imageArea/length(RGB)*physicalArea,digits=4),"mm2"),file=stdout())
##Output: region area



