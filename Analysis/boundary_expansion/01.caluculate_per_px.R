#Conversion ratio between pixels and micrometers (µm) for 1000×1000 pixel images
args <- commandArgs();
fileName <- args[6]
prefix <- args[7]

dat <- read.csv(fileName,header=T)

r1 <- range(dat$V2)
r2 <- range(dat$V3)

if(r1[2]-r1[1]>r2[2]-r2[1]){
  leng='x'
}else{
  leng='y'
}

if(r1[2]-r1[1]>r2[2]-r2[1]){
  r2[2]=r2[1]+r1[2]-r1[1]
}else{
  r1[2]=r1[1]+r2[2]-r2[1]
}

l <- max((r1[2]-r1[1]),(r2[2]-r2[1]))


ratio <- 1e7
center2center <- 500 ### nm
pxpercm <- l*center2center/ratio/1000 ####cm/px
pxperum <- pxpercm*1e4 ### um/px
write(paste(prefix,leng,pxperum,sep = "\t"),file=stdout())


