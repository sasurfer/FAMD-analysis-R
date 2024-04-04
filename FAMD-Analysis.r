#Perform FAMD analysis on data
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(ggpubr)#for ggpar
library(plotly)#for 3d visualization
library(corrplot)
#read file into dataframe
df <- read.csv("./SurveyAnswers.csv", row.names = 1)
dim(df)
df = subset(df, select = -c(commercial,opensource) ) 
dim(df)
head(df,4)

#apply famd 
res.famd <- FAMD(df, ncp=13,axes=c(1,2),graph = FALSE)
summary(res.famd)

#Extract and show the variances retained by each dimension (scree plot)
eig.val <- get_eigenvalue(res.famd)
eig.val

fviz_screeplot(res.famd,ncp=13,addlabels=TRUE,barfill='lightgray',linecolor='red',ylim=c(0,40))

#eigenvalues plot
Dimensions <- 1:10
Eigenvalues <- eig.val[,1]
data <- data.frame(Dimensions,Eigenvalues)
ggplot(data,aes(x=Dimensions,y=Eigenvalues),xlab='Dimensions',ylab='Eigenvalue',lab) +
  geom_line(color="black") +
  geom_point(size=3,color='lightgray')+
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=10))+
  theme(axis.text.y = element_text(face="bold", color="#993333", 
                                   size=10))+
  theme(axis.line.x=element_line(color="black"))+
  theme(axis.line.y=element_line(color="black"))+
  scale_x_continuous(breaks=seq(1,10))+
  theme_bw()

#biplots individuals+qualitative variables
a<-fviz_famd_ind(res.famd, col.ind = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)
ggpar(a,main='')
a<-fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,axes=c(1,3))
ggpar(a,main='')
  
  
# Plot of variables
fviz_famd_var(res.famd, repel = TRUE,axes=c(1,2))
fviz_famd_var(res.famd, repel = TRUE,axes=c(1,3))
# Contributions to the first dimension
a<-fviz_contrib(res.famd, "var", axes = 1,fill='lightgray')
ggpar(a,main='Contribution of variables to dimension 1')
# Contributions to the second dimension
b<-fviz_contrib(res.famd, "var", axes = 2,fill='lightgray')
ggpar(b,main='Contribution of variables to dimension 2')
# Contributions to the third dimension
c<-fviz_contrib(res.famd, "var", axes = 3,fill='lightgray')
ggpar(c,main='Contribution of variables to dimension 3')

#Contributions to the first three dimensions
c<-fviz_contrib(res.famd, "var", axes = 1:3,fill='lightgray')
ggpar(c,main='Contribution of variables to the first 3 dimensions')

#Another way to express contribution to PC that is correlations with that axis
res.desc <- dimdesc(res.famd, axes = c(1,2,3), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2
res.desc$Dim.3

#Quantitative variables graphs
d<-fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")
ggpar(d,main='Quantitative variables')
d<-fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black",axes=c(1,3))
ggpar(d,main='Quantitative variables')

#color by contribution
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,axes=c(1,2))
ggpar(e,main='Quantitative variables color by contribution')
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE,axes=c(1,3))
ggpar(e,main='Quantitative variables color by contribution')
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE,axes=c(2,3))
ggpar(e,main='Quantitative variables color by contribution')

# Color by cos2 values: quality on the factor map
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE),axes=c(1,2))
ggpar(e,main='Quantitative variables color by cos2')
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE,axes=c(1,3))
ggpar(e,main='Quantitative variables color by cos2')
e<-fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = TRUE,axes=c(2,3))
ggpar(e,main='Quantitative variables color by cos2')

#Qualitative variables graphs
e<-fviz_famd_var(res.famd, "quali.var", col.var='black',
                 ,axes=c(1,2),xlim=c(-6,2),ylim=c(-3,2))
ggpar(e,main='Qualitative variables')
#ggpar(e,main='Qualitative variables correlation with dimensions 1-2')
e<-fviz_famd_var(res.famd, "quali.var", col.var = "black", 
                 ,axes=c(1,3),xlim=c(-6,2),ylim=c(-1.5,1.5))
ggpar(e,main='Qualitative variables')
e<-fviz_famd_var(res.famd, "quali.var", col.var = "black", 
                 ,axes=c(2,3),xlim=c(-4,2),ylim=c(-1.5,1.5))
ggpar(e,main='Qualitative variables')

#color by cos2
e<-fviz_famd_var(res.famd, "quali.var", col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
              ,axes=c(1,2),xlim=c(-6,2),ylim=c(-3,2))
ggpar(e,main='Qualitative variables color by cos2')
e<-fviz_famd_var(res.famd, "quali.var", col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
,axes=c(1,3),xlim=c(-6,2),ylim=c(-1.5,1.5))
ggpar(e,main='Qualitative variables color by cos2')
e<-fviz_famd_var(res.famd, "quali.var", col.var = "cos2", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                 ,axes=c(2,3),ylim=c(-1.5,1.5))
ggpar(e,main='Qualitative variables color by cos2')

#corrplot cos2 on all dimensions
var <- get_famd_var(res.famd)
var
corrplot(var$cos2,is.corr=FALSE)
#corrplot contribution on all dimensions
corrplot(var$contrib,is.corr=FALSE)

#3d plot for the first three dimensions
famd3d <- FAMD(df, ncp=3,axes=c(1,2),graph = FALSE)
summary(famd3d)
famd3d.ind<-get_famd_ind(famd3d)
ind=as.data.frame(famd3d.ind$coord)
ind
mycolors <- ~c('cluster4','cluster4','cluster3','cluster3','cluster4','cluster1','cluster4','cluster2','cluster4','cluster2','cluster4')
fig2 <- plot_ly(ind, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, text=rownames(ind),
                mode='markers+text',type='scatter3d' ,color = mycolors, 
                colors=c('black','blue','green','red'),symbol=mycolors,
                symbols=c('cross','diamond','square','circle')
                ) 
fig2
famd3d.ind$cos2
#color by cos2
cos2tot<-rowSums(famd3d.ind$cos2)
individuals<-famd3d.ind$cos2
individuals<-as.data.frame(individuals)
individuals$costot <- as.vector(cos2tot)
fig2 <- plot_ly(individuals, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, text=rownames(ind),
                mode='markers+text',type='scatter3d' ,color = ~costot, 
                colors=c('black','blue','green','red'),symbol=mycolors,
                symbols=c('cross','diamond','square','circle')
) 
fig2
