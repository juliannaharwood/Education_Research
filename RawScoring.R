### BEMA RAW SCORING ###

# After filling in steps 1-3, highlight this entire page and press "run." Once run,
# the global environment will have a data set "finaldata" with each student's raw pre and 
# post test score and Normalized Change. The console window will give the average pre and 
# post test scores as well as the average Normalized Change. If the data includes
# a section variable, by section data will also appear in the console window. For two sections,
# p-values comparing their pre and post test means has well as their Normalized Change means
# will appear. For three sections, results from TukeyHSD post-hoc tests will appear. 

# 1. below set the working directory and install package to read in excel documents
setwd("/Users/juliannaharwood/Desktop/SUMMER/DATA")
install.packages("readxl")
library("readxl")

# 2. below replace "DOC NAME" with the pre test excel document name followed by .xlsx 
# and "SHEET" with the pre test sheet name, either the number without quotations or 
# character name with quotations
pre<-read_excel("DOC NAME",sheet="SHEET")

# 3. repeat step two below for the post test document and post test sheet
post<-read_excel("DOC NAME",sheet="SHEET")

#merge pre and post
merge<-merge(pre,post,by="id",all=FALSE)

#for loop to create dummy variables indicating whether the question was answered correctly
num<-c(1:31)
key <- c("A","A","B","E","A","D","E","B","B","F",
         "E","E","D","B","G","B","D","B","B","G",
         "A","E","E","A","D","D","C","B","C","F",
         "D")
dummy <- paste0("dummyq", num)
dummypq<-paste0("dummypq",num)
var <- paste0("q", num)
varp<-paste0("qp",num)

for (i in num){
  merge[dummy[i]] <- ifelse(merge[var[i]] == key[i], 1, 0)
}
for (i in num){
  merge[dummypq[i]] <- ifelse(merge[varp[i]] == key[i], 1, 0)
}

#mark blank answers as wrong
merge[is.na(merge)]<-0

#delete original variables
if(is.null(merge$Section)==TRUE) rfinaldata<-merge[c(1,66:127)]
if(is.null(merge$Section)==FALSE) rfinaldata<-merge[c(1,3,67:128)]

#score pretest
rfinaldata$totalpre<-rowSums(
  subset(rfinaldata,
         select=which(colnames(rfinaldata)=="dummyq1"):which(colnames(rfinaldata)=="dummyq31")))/30*100
rfinaldata$roundedprescore<-round(rfinaldata$totalpre,digits=0)

#score post test
rfinaldata$totalpost<-rowSums(
  subset(rfinaldata,
         select=which(colnames(rfinaldata)=="dummypq1"):which(colnames(rfinaldata)=="dummypq31")))/30*100
rfinaldata$roundedpostscore<-round(rfinaldata$totalpost,digits=0)

#calcuate hakes
rfinaldata$hakes<-(rfinaldata$totalpost-rfinaldata$totalpre)/(100-rfinaldata$totalpre)

#set infintie and non-real Hake's gains to NA 
rfinaldata$hakes[is.nan(rfinaldata$hakes)]<-NA
rfinaldata$hakes[rfinaldata$totalpost<rfinaldata$totalpre]<-
  (rfinaldata$totalpost[rfinaldata$totalpost<rfinaldata$totalpre]-
     rfinaldata$totalpre[rfinaldata$totalpost<rfinaldata$totalpre])/
  rfinaldata$totalpre[rfinaldata$totalpost<rfinaldata$totalpre]
avggainnum<-mean(rfinaldata$hakes,na.rm=TRUE)
avggainnumround<-round(avggainnum,digits=2)
avggain<-paste("Average Hake's Gain:",avggainnumround)

#average score on the pre test
avgprenum<-mean(rfinaldata$totalpre)
avgprenumround<-round(avgprenum,digits=0)
avgpre<-paste("Average Pre Score:",avgprenumround,"%")

#average score on the post test
avgpostnum<-mean(rfinaldata$totalpost)
avgpostnumround<-round(avgpostnum,digits=0)
avgpost<-paste("Average Post Score:",avgpostnumround,"%")

#standard deviation pre
sprenum<-sd(rfinaldata$totalpre)
spreround<-round(sprenum,2)
spre<-paste("Pretest Standard Deviation:",spreround)

#standard deviation post
spostnum<-sd(rfinaldata$totalpost)
spostround<-round(spostnum,2)
spost<-paste("Posttest Standard Deviation:",spostround)

#standard deviation hakes
shakesnum<-sd(rfinaldata$hakes,na.rm=TRUE)
shakesround<-round(shakesnum,2)
shakes<-paste("Hake's Gain Standard Deviation",shakesround)

#by section analysis (if applicable)
if(is.null(merge$Section)==FALSE) {premeanbysec<-tapply(rfinaldata$totalpre,
                                                        rfinaldata$Section,mean);
premeanbysec[]<-lapply(premeanbysec,round,2);presdbysec<-tapply(rfinaldata$totalpre,
                                                                rfinaldata$Section,sd);
presdbysec[]<-lapply(presdbysec,round,2);postmeanbysec<-tapply(rfinaldata$totalpost,
                                                               rfinaldata$Section,mean);
postmeanbysec[]<-lapply(postmeanbysec,round,2);postsdbysec<-tapply(rfinaldata$totalpost,
                                                                   rfinaldata$Section,sd);
postsdbysec[]<-lapply(postsdbysec,round,2);hakesmeanbysec<-tapply(rfinaldata$hakes,
                                                                  rfinaldata$Section,mean);
hakesmeanbysec[]<-lapply(hakesmeanbysec,round,2);hakessdbysec<-tapply(rfinaldata$hakes,
                                                                      rfinaldata$Section,sd);
hakessdbysec[]<-lapply(hakessdbysec,round,2);
}

if(any(merge$Section==3)==FALSE&is.null(merge$Section)==FALSE) {
  pret<-t.test(rfinaldata$totalpre[rfinaldata$Section==1],
               rfinaldata$totalpre[rfinaldata$Section==2]);
  pretnum<-as.numeric(pret[3])
  pretnum<-round(pretnum,3)
  postt<-t.test(rfinaldata$totalpost[rfinaldata$Section==1],
                rfinaldata$totalpost[rfinaldata$Section==2]);
  posttnum<-as.numeric(postt[3])
  posttnum<-round(posttnum,3)
  hakest<-t.test(rfinaldata$hakes[rfinaldata$Section==1],
                 rfinaldata$hakes[rfinaldata$Section==2]);
  hakestnum<-as.numeric(hakest[3])
  hakestnum<-round(hakestnum,3);
  a0<-paste("Section 1 Pretest Mean:",premeanbysec[1],"(Stan.dev:",presdbysec[1],")");
  a1<-paste("Section 2 Pretest Mean:",premeanbysec[2],"(Stan.dev:",presdbysec[2],")");
  a2<-paste("P Value Between Pretest Means =",pretnum);
  b0<-paste("Section 1 Posttest Mean:",postmeanbysec[1],"(Stan.dev:",postsdbysec[1],")");
  b1<-paste("Section 2 Posttest Mean:",postmeanbysec[2],"(Stan.dev:",postsdbysec[2],")");
  b2<-paste("P Value Between Posttest Means =",posttnum);
  c0<-paste("Section 1 Hake's Mean:",hakesmeanbysec[1],"(Stan.dev:",hakessdbysec[1],")");
  c1<-paste("Section 2 Hake's Mean:",hakesmeanbysec[2],"(Stan.dev:",hakessdbysec[2],")");
  c2<-paste("P Value Between Hake's Means =",hakestnum);
}

if(any(merge$Section==3)==TRUE) {
  preaov<-aov(totalpre~Section,rfinaldata)
  tukeypreaov<-TukeyHSD(preaov)
  postaov<-aov(totalpost~Section,rfinaldata)
  tukeypostaov<-TukeyHSD(postaov)
  hakesaov<-aov(hakes~Section,rfinaldata)
  tukeyhakesaov<-TukeyHSD(hakesaov)
  a0<-paste("Section 1 Pretest Mean:",premeanbysec[1],"(Stan.dev:",presdbysec[1],")");
  a1<-paste("Section 2 Pretest Mean:",premeanbysec[2],"(Stan.dev:",presdbysec[2],")");
  a2<-paste("Section 3 Pretest Mean:",premeanbysec[3],"(Stan.dev:",presdbysec[3],")");
  b0<-paste("Section 1 Posttest Mean:",postmeanbysec[1],"(Stan.dev:",postsdbysec[1],")");
  b1<-paste("Section 2 Posttest Mean:",postmeanbysec[2],"(Stan.dev:",postsdbysec[2],")");
  b2<-paste("Section 3 Posttest Mean:",postmeanbysec[3],"(Stan.dev:",postsdbysec[3],")");
  c0<-paste("Section 1 Hake's Mean:",hakesmeanbysec[1],"(Stan.dev:",hakessdbysec[1],")");
  c1<-paste("Section 2 Hake's Mean:",hakesmeanbysec[2],"(Stan.dev:",hakessdbysec[2],")");
  c2<-paste("Section 3 Hake's Mean:",hakesmeanbysec[3],"(Stan.dev:",hakessdbysec[3],")");
  show(c(avgpre,spre,avgpost,spost,avggain,shakes));
  show(c(a0,a1,a2,b0,b1,b2,c0,c1,c2));
  print("Tukey HSD Test of Pretest Scores Between Sections:")
  show(tukeypreaov)
  print("Tukey HSD Test of Posttest Scores Between Sections:")
  show(tukeypostaov)
  print("Tukey HSD Test of Hake's Gain Between Sections:")
  show(tukeyhakesaov)
}

#give values
if(any(merge$Section==3)==FALSE&is.null(merge$Section)==FALSE) {
  show(c(avgpre,spre,avgpost,spost,avggain,shakes));
  show(c(a0,a1,a2,b0,b1,b2,c0,c1,c2))}
if(is.null(merge$Section)==TRUE) show(c(avgpre,spre,avgpost,spost,avggain,shakes))



