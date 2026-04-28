#Going through the book chapter by chapter for help.

#### Chapter 1 ####
install.packages("phangorn")
library(ape)
library(phangorn)

#Reading in a "Toy" tree
text.string <- "(((((Robin,Iguana),((((Cow,Whale), Pig),Bat),(Lemur, Human))),
Colecanth), Goldfish), Shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, no.margin = TRUE)

#Learning to ask for help
help("plot.phylo")
args(plot.phylo)

#Using that info, let's plot our tree in a bunch of different ways.
par(mfrow=c(2,2),mar=c(1.1,1.1,3.1,1.1)) #This is what divided our output into a 2x2 grid.
#Bottom, left, top, right. Order for the numbers for margins.
plot(vert.tree)
mtext("(a)", line=1, adj=0)
plot(vert.tree, type="cladogram")
mtext("(b)", line=1, adj=0)
plot(unroot(vert.tree),type="unrooted", lab4ut="axial", x.lim=c(-2,6.5), y.lim=c(-3,7.5))
mtext("(c)", line=1, adj=0)

#learning about the "phylo" list.
vert.tree
#we see a summary rather than the structure.
#Must use the str function instead.
str(vert.tree)
#Parts of the structure:
#edge - a 20x2 (in this case) matrix containing startinga nd ending indices for the nodes subtending each branch of the phylogeny.
#Nnode - an integer value giving the total number of internal nodes in the tree.
#tip.label - a character vector of length N containign the lablels for all the tips or terminal taxa in the phylogeny.

#Node Indices
library(phytools)
plotTree(vert.tree, offset=1, type="cladogram")
labelnodes(1:(Ntip(vert.tree)+vert.tree$Nnode), 1:(Ntip(vert.tree)+vert.tree$Nnode), interactive = FALSE, cex=0.8)
#plotTree is from phytools
vert.tree$edge
vert.tree$tip.label
vert.tree$Nnode

#using another tree data
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
anolis.tree<-read.tree(file="Anolis.tre")
anolis.tree
plotTree(anolis.tree, ftype="i", fsize=0.4, lwd=1)
Ntip(anolis.tree)
write.tree(vert.tree,file="example.tre")
cat(readLines("example.tre"))

#Plotting and Manipulating Trees
#We're going to learn how to use grep to help us subset and trim the tree
pr.species<-c("cooki", "poncensis", "gundlachi","pulchellus","stratulus","krugi","evermanni"
              , "occultus","cuvieri","cristatellus")
nodes<-sapply(pr.species, grep, x=anolis.tree$tip.label)
nodes
#sapply made it so I didn't need to write a loop for this
#ie, what we did here was: "apply to the elements of pr.species the function grep with the argument x of grep set to anolis.tree$tip.label"
plotTree(anolis.tree, type="fan", fsize=0.6, lwd=1, ftype="i")
add.arrow(anolis.tree, tip = nodes, arrl=0.15, col="red", offset=2)

anolis.noPR<-drop.tip(anolis.tree, pr.species)
plotTree(anolis.noPR, type="fan", fsize=0.6, lwd=1, ftype="i")
#Now we're extracting a clade, rather than dropping it.

node<-getMRCA(anolis.tree, pr.species[-which(pr.species%in%c("cuvieri","occultus"))])
node
#we're getting fancy with it now
plot(paintSubTree(anolis.tree, node, "b","a"), type="fan", fsize=0.6, lwd=2, 
     colors=setNames(c("gray","blue"), c("a","b")), ftype="i")
arc.cladelabels(anolis.tree,"clade to extract", node, 1.35, 1.4, mark.node=FALSE, cex=0.6)

pr.clade<-extract.clade(anolis.tree, node)
pr.clade

#Now we keep only the small clade, and cut everything else
pr.tree<-keep.tip(anolis.tree,pr.species)
pr.tree

par(mfrow=c(1,2))
plotTree(pr.clade, ftype="i", mar=c(1.1,1.1,1.3,1.1), cex=1.1)
mtext("(a)", line=0, adj=0)
plotTree(pr.tree, ftype="i", mar=c(1.1,1.1,1.3,1.1), cex=1.1)
mtext("(b)", line=0, adj=0)

#Getting really fancy with it now
par(mfrow=c(1,1))
anolis.pruned<-collapseTree(anolis.tree)
#huh, that's fun to mess around with

#Multiple Trees in a Single Object
anolis.trees<-c(anolis.tree, anolis.noPR, pr.clade, pr.tree)
print(anolis.trees, details=TRUE)
write.tree(anolis.trees, file="example.trees")
cat(readLines("example.trees"),sep="\n")

#Managing Trees adn Comparative Data
anole.data<-read.csv(file="anole.data.csv", row.names = 1, header=TRUE)
ecomorph<-read.csv(file="ecomorph.csv", row.names = 1, header = TRUE, stringsAsFactors = TRUE)
head(anole.data)
dim(anole.data)
head(ecomorph)
dim(ecomorph)
library(geiger)
name.check(anolis.tree, anole.data)
chk<-name.check(anolis.tree, ecomorph)
chk
summary(chk)
#applying the pruining.
ecomorph.tree<-drop.tip(anolis.tree, chk$tree_not_data)
ecomorph.tree
ecomorph.data<-anole.data[ecomorph.tree$tip.label,]
head(ecomorph.data)
name.check(ecomorph.tree, ecomorph.data)

#Doing a very simple PCA now
ecomorph.pca <-phyl.pca(ecomorph.tree, ecomorph.data)
ecomorph.pca
par(mar=c(4.1,4.1,2.1,1.1),las=1)
plot(ecomorph.pca, main="")
#flipping the sign
ecomorph.pca$Evec[,1]<--ecomorph.pca$Evec[,1]
ecomorph.pca$L[,1]<--ecomorph.pca$L[,1]
ecomorph.pca$S<-scores(ecomorph.pca, newdata=ecomorph.data)

par(cex.axis=0.8, mar=c(5.1,5.1,1.1,1.1))
phylomorphospace(ecomorph.tree, scores(ecomorph.pca)[,1:2], ftype="off", node.size=c(0,1),
                 bty="n",las=1, xlab="PC1 (Overall Size)", ylab=expression(paste("PC2 ("%up%"lamellae number,"%down%"tail length")))
eco<-setNames(ecomorph[,1], rownames(ecomorph))
ECO<-to.matrix(eco,levels(eco))
tiplabels(pie=ECO[ecomorph.tree$tip.label,], cex=0.5)
legend(x="bottomright", legend=levels(eco),cex=0.8, pch=21,pt.bg = rainbow(n=length(levels(eco))),pt.cex=1.5)

#Practice Problems

#1
#And you know what, I give up on this section. We're only doing this one problem, and that's it.
pheldat<-read.csv("phel.csv", row.names = 1, header = TRUE, stringsAsFactors = TRUE)
pheltree<-read.tree(file="phel.phy")
name.check(pheltree, pheldat)
chk2<-name.check(pheltree, pheldat)
length(intersect(pheltree$tip.label, rownames(pheldat)))
pheltreeprun <- drop.tip(pheltree, chk2$tree_not_data)
plotTree(pheltreeprun, ftype="i", cex=1.1)


####Chapter 2####
#Oh no.
#Not Felsenstein. 
#I had to read one of his papers and ahhhhhhhhhhh
tree<-read.tree(text="((A,B,C,D,E,F,G,H,I,J,K,L,M),(N,O,P,Q,R,S,T,U,V,W,X,Y,Z));")
##Set branch lengths on the tree
tree<-compute.brlen(tree, power=1.8)
##simmulate data, idependently for x and y 
x<-fastBM(tree)
y<-fastBM(tree) 
## Plot the results iwth clades A and B labled.
##split the plotting area
par(mfrow=c(1,2))
#graphing the tree
plotTree(tree, type="cladogram", ftype="off", mar=c(5.1,4.1,3.1,2.1), color="darkgray",
         xlim=c(0,1.3), ylim=c(1,Ntip(tree)))
##add pointsa t the tips of the tree to match those on our scatterplot.
points(rep(1,13), 1:13, pch=21, bg="lightgray", cex=1.2)
points(rep(1,13), 14:26, pch=22, bg="black", cex=1.2)
##add clade labels to the treec
cladelabels(tree, "A", node=28, offset=2)
cladelabels(tree, "B", node=29, offset=2)
mtext("(a)", line=1, adj=0, cex=1.5)
##mking the scatter plot of the x and y
par(mar=c(5.1,4.1,3.1,2.1))
plot(x,y,bty="n",las=1)
points(x[1:13],y[1:13],pch=21, bg="lightgray", cex=1.2)
points(x[14:26],y[14:26],pch=22, bg="black", cex=1.2)
mtext("(b)", line=1, adj=0, cex=1.5)

#I'll be so real man, I barely grasp what's going on in this chapter already.
#I'll get through it still, but idk how much I'll undersatnd

#Reviewing linear models in R
mammalHR<-read.csv("mammalHR.csv",row.names = 1)
head(mammalHR)
options(scipen=100)
par(mar=c(5.1,5.1,1.1,1.1))
plot(homeRange~bodyMass, data=mammalHR, xlab="body mass (kg)", 
     ylab=expression(paste("home range (km"^"2",")")), 
     pch=21, bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,cex.lab=0.9, bty="n")
#Now let's mess with the models
fit.ols<-lm(log(homeRange)~log(bodyMass),data=mammalHR)
fit.ols
#Don't get much just by putting in the .ols file
summary(fit.ols)
#overlaying our model on the plot
par(mar=c(5.1,5.1,1.1,1.1))
plot(homeRange~bodyMass, data=mammalHR, xlab="body mass (kg)", 
     ylab=expression(paste("home range (km"^"2",")")), 
     pch=21, bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,cex.lab=0.9, bty="n")
lines(mammalHR$bodyMass, exp(predict(fit.ols)),lwd=2, col="darkgray")

#Now we're messing with the tree.
mammal.tree<-read.tree("mammalHR.phy")
plotTree(mammal.tree, ftype="i", fsize=0.7, lwd=1)
nodelabels(bg="white", cex=0.5, frame="circle")

##pull our home range and body mass as numeric vectors
homeRange<-setNames(mammalHR[,"homeRange"], rownames(mammalHR))
bodyMass<-setNames(mammalHR[,"bodyMass"], rownames(mammalHR))
##compute PICs for home range adn body size
pic.homerange<-pic(log(homeRange),mammal.tree)
pic.bodymass<-pic(log(bodyMass),mammal.tree)
head(pic.homerange, n=20)


fit.pic<-lm(pic.homerange~pic.bodymass+0)
fit.pic
summary(fit.pic)
#Making whatever this thing is.
par(mfrow=c(1,1))
par(mar=c(5.1,5.1,1.1,1.1))
plot(pic.homerange~pic.bodymass, xlab="PICs for log (mody mass)", 
     ylab="PICs for log (range size)", 
     pch=21, bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,cex.lab=0.9, bty="n")
abline(h=0,lty="dotted")
abline(v=0,lty="dotted")
clip(min(pic.bodymass),max(pic.bodymass), min(pic.homerange),max(pic.homerange))
abline(fit.pic, lwd=2,col="darkgray")

#We're working with seeds now
set.seed(1001)

##making a complicated tree from the book
##set starting tree to NULL
tree<-NULL
##repeat simulation until non-NULL (ie, non extinct) tree is obatined.
while(is.null(tree))
  tree<-pbtree(n=100,b=1,d=0.8,extant.only=TRUE)
##plot the simulated tree
plotTree(tree,fytpe="off",color="darkgray",lwd=1)

#simulating evolution on the tree.
x<-fastBM(tree)
y<-fastBM(tree)
par(mar=c(5.1,4.1,1.1,1.1))
plot(x,y,cex=1.2,pch=21,bg="gray",las=1,cex.axis=0.7, cex.lab=0.9,bty="n")
grid()
clip(min(x),max(x),min(y),max(y))
fit.ols<-lm(y~x)
abline(fit.ols,lwd=2,col="darkgray")
fit.ols
summary(fit.ols)
#This example shows that it's not difficult at all for the phylogeny to generate a Type I Error

#Making the really funky plot again to show a visual on how this error occurs
par(mar=c(5.1,5.1,1.1,1.1),cex.axis=0.7,cex.lab=0.9)
phylomorphospace(tree,cbind(x,y),label="off",node.siz=c(0,0),bty="n",las=1)
points(x,y,pch=21,bg="gray",cex=1.2)
grid()
clip(min(x),max(x),min(y),max(y))
abline(fit.ols,lwd=2)

#Now we're going to try to resolve the Type I Error
##Compute PICs for x and y
ix<-pic(x,tree)
iy<-pic(y,tree)
##Fit PIC regression through the origin
fit.pic<-lm(iy~ix+0)
fit.pic
##setting plotting margins
par(mar=c(5.1,4.1,1.1,1.1))
##graphing it
plot(ix,iy,cex=1.2,pch=21,bg="gray",las=1,xlab="PICs for x", ylab="PICs for y",
     cex.axis=0.7,cex.lab=0.9,bty="n")
grid()
clip(min(ix),max(ix),min(iy),max(iy))
abline(fit.pic,lwd=2,col="darkgray")
summary(fit.pic)
#Now the statistical significance has dissapeared.
#also I keep reading fit.pic and feet Pic in my head and I hate it
#also also, I'm re-reading a childhood book series (my brother is reading it 
#and I'm reading along with him to talk about it) and there's a creature in the books called
#ols and it's tripping me up. Also, Ol with the two dots above the "o" is beer in swedish
#the more you know *insert gif here*

#haha, we get to use the lapply,sapply, and mapply functions
#there names make me chuckle, idk man

##Let's take this to the next level and make a custom function to do a bunch of what we 
#just did, but all at once
foo<-function(){
  tree<-NULL
  while(is.null(tree))
  tree<-pbtree(n=100,b=1,d=0.8,
               extant.only=TRUE,quiet=TRUE)
  tree
}
##Okay, so we're now doing this 500 times.
trees<-replicate(500,foo(),simplify=FALSE)
class(trees)<-"multiPhylo"
##Simulating data for x and y independaently using lapply. 500 times
x<-lapply(trees,fastBM)
y<-lapply(trees,fastBM)
foo<-function(x,y) lm(y~x)
#mapply function call to fit y~x to each pair
#My head is spinning with like 4 different fit.ols we've made, so you know what
#we're naming this one after one of those ol creatures from the books I mentioned.
#Say hi to Dain now
Dain<-mapply(foo,x,y,SIMPLIFY=FALSE)
#If you chose to google this character for whatever reason, ignore the anime
#there is no anime adaptation of Deltora Quest in Ba Sing Se.

#Apparently all we care about is the P-value, so...
#Now we right another function to get that
foo<-function(fit) anova(fit) [["Pr(>F)"]] [1]
#ah dang, okay, another .ols variable.
#We're gonna call this one Fallow, also after a character from the books
Fallow<-sapply(Dain,foo)

#And now we repeat everything apparently.
pic.x<-mapply(pic,x,trees,SIMPLIFY=FALSE)
pic.y<-mapply(pic,y,trees,SIMPLIFY=FALSE)
##Custom function to fit linear model without intercept
foo<-function(x,y) lm(y~x+0)
#fits.pic is having the same issue as .ols to me.
#feetpic it is now.
feetpic<-mapply(foo,pic.x,pic.y,SIMPLIFY=FALSE)
##We're now going to pull the p value out like before.
foo<-function(fit) anova(fit) [["Pr(>F)"]] [1]
##iterate over fitted models
pval.pic<-sapply(feetpic,foo)



##Compute histograms for OLS without plotting
h1<-hist(Fallow,breaks=20,plot=FALSE)
##convert counts to relative frequencies
h1$counts<-h1$counts/sum(h1$counts)
h2<-hist(pval.pic,breaks=20,plot=FALSE)
h2$counts<-h2$count/sum(h2$counts)
##subdivide plotting area and adjust margins
par(mfrow=c(1,2),mar=c(5.1,4.1,2.1,1.1))
##Plotting the first OLS histogram
plot(h1,ylim=c(0,0.6),col="gray",main="",cex.axis=0.5,cex.lab=0.7,las=1)
mtext("(a)",line=1,adj=0,cex=0.8)
##show nominal alpha level of 0.05
lines(c(0,1),rep(0.05,2),lwd=1,lty="dotted")
##plot second histogram of contrast regression p values
plot(h2,ylim=c(0,0.6),col="gray",main="",cex.axis=0.5,cex.lab=0.7,las=1)
mtext("(b)",line=1,adj=0,cex=0.8)
lines(c(0,1),rep(0.05,2),lwd=1,lty="dotted")


#Good gracious I did not understand that chapter hardly at all.
#Not doing the extra problems with that one yet.
#I don't think I have neough stats knowledge for that

####Chapter 3####
#We're working with PGLS this time.
#Say by to Dain and Fallow I guess.
primate.data<-read.csv("primateEyes.csv", row.names = 1, stringsAsFactors = TRUE)
head(primate.data, 4)
primate.tree<-read.tree("primateEyes.phy")
print(primate.tree, printlen = 2)

#Extracting data from the trees
orbit.area<-setNames(primate.data[,"Orbit_area"], rownames(primate.data))
skull.length<-setNames(primate.data[,"Skull_length"], rownames(primate.data))
##Now we're computing PICs on the log-transformed values of both traits
pic.orbit.area<-pic(log(orbit.area),primate.tree)
pic.skull.length<-pic(log(skull.length), primate.tree)
##Fitting a linear regrression to the orbit as a function of skull.
pic.primate<-lm(pic.orbit.area~pic.skull.length+0)
summary(pic.primate)
## This gives us a highly significant slope.

##Let's plot it for visualization, like last chapter.
par(mfrow=c(1,2), mar=c(5.1,4.6,2.1,1.1))
##plot our raw data into the original space
plot(orbit.area~skull.length, log="xy",pch=21,bg=palette()[4],cex=1.2,bty="n",
     xlab="skull length (cm)", ylab=expression(paste("orbit area(",mm^2,")")),
     cex.lab=0.8,cex.axis=0.7,las=1)
mtext("(a)", line=0,adj=0, cex=0.8)
##Now we plot our phylogenetic contrasts
plot(pic.orbit.area~pic.skull.length,pch=21,bg=palette()[4],cex=1.2,bty="n",
     xlab="PICs for log (skull length)", ylab="PICs for log (orbit area)",
     cex.lab=0.8,cex.axis=0.7, las=1)
mtext("(b)", line=0,adj=0,cex=0.8)
##limit the plotting area to the range of our two traits
clip(min(pic.skull.length),max(pic.skull.length), min(pic.orbit.area),max(pic.orbit.area))
##add our fitted contasts regression line
abline(pic.primate, lwd=2)


##Fitting the linear regression model using PGLS instead.
library(nlme)
#Converting our tree to a correlation structure so we can use the GLS on it
#apparently we're using the ape function "corBrownian"
spp<-rownames(primate.data)
corBM<-corBrownian(phy=primate.tree, form=~spp)
corBM
#the spp thing was to specify the order of the taxa in our data.
pgls.primate<-gls(log(Orbit_area)~log(Skull_length), data=primate.data, correlation=corBM)
#it doesn't seem that differnt from what we were doing last chapter.
#the correlation variable is set to the corBM which we made earlier.
#It's our corStruct object we made.
summary(pgls.primate)

#Comparing PICs and PGLS
coef(pic.primate)
coef(pgls.primate)
abs(coef(pic.primate)[1]-coef(pgls.primate)[2])
#Because our absolute value returned is very very small, we can conclude that the 
#methods have returned the same estimated model slopes
#We can also compare our p-values directly
summary(pic.primate)$coefficients[1,4]
summary(pgls.primate)$tTable[2,4]
#To at least 7 digits of precision, our models match perfectly

##This aparently isn't nessicary, but I'm going to do it anyway, because
#I'm hoping it will help me.

##set the random number generator seed
set.seed(88)
##simulate a random 5-taxon tree
tree<-pbtree(n=5,scale=10,tip.label=LETTERS[5:1])
##Subdivide our plotting area into two panels
par(mfrow=c(2,1))
##plot the tree
plotTree(tree,mar=c(3.1,1.1,4.1,1.1),fsize=1.25,ylim=c(0.5,5.4))
##add a horizontal axis
axis(1)
##add edge labels giving the branch lengths
edgelabels(round(tree$edge.length,2),pos=3, frame="none", cex=0.9)
mtext("(a)", line=1,adj=0)
##Switch to the second pannel
plot.new()
##Set new plot margins and plot dimensions
par(mar=c(3.1,1.1,4.1,1.1))
plot.window(xlim=c(0,6),ylim=c(0,6))
##add a grid of lines for our correlation matrix
lines(c(0,6,6,0,0),c(0,0,6,6,0))
for(i in 1:5) lines(c(i,i),c(0,6))
for(i in 1:5) lines(c(0,6), c(i,i))
##compute the assumed correlation structure
V<-cov2cor(vcv(tree)[LETTERS [1:5], LETTERS[1:5]])
##print it into the boxes of our grid
for(i in 1:5) text(i+0.5,5.5,LETTERS[i],cex=1.1)
for(i in 1:5) text(0.5,5.5-i, LETTERS[i], cex=1.1)
for(i in 1:5) for(j in 1:5) text(0.5+i, 5.5-j, round(V[i,j],2),cex=1.1)
mtext("(b)", line=1, adj=0)
#The book goes over it and gives formulas, but basically this table shows how correlated 
#each branch of the phylogey is to another
#ie, A to A is 1 (duh)
#and C to D is 0.73

##We're messing around with Pagel's Lambda now.
##This is, I'm honestly not to sure what's going on.
corLambda<-corPagel(value=1,phy=primate.tree,form=~spp)
corLambda
#very similar to the corBrownian output, but with the added argument of value
#we've assigned this 1 here
#This is just the starting value. The ending value will be estimated along with the model
pgls.Lambda<-gls(log(Orbit_area)~log(Skull_length), data=primate.data, correlation=corLambda)
summary(pgls.Lambda)
#The ML estimate of lamda i 1.01 in this cas,e super clos to 1. 
#It seems like things are even more so correlated than in our original model
#which would lead to a conclusion that there is evolutionary correlation between the two
#traits

#Okay, so now we're messing with ANOVA, or more specifically, ANCOVA
#It doesn't say in the book, but I'm assuming that
#ANCOVA stands for Analysis of Covariates
#But I could be very wrong
primate.ancova<-gls(log(Orbit_area)~log(Skull_length)+ Activity_pattern, data=primate.data,
                    correlation = corBM)
anova(primate.ancova)
#There is a signficicant effet on activity pattern on orbit area. 
#We see this after also controlling for the significant effect of skull size

#Now let's make a plot to see this pattern.
par(mfrow=c(1,1),mar=c(5.1,5.1,2.1,2.1))
##setting the point colors for the different levels of our factor
pt.cols<-setNames(c("#87CEEB","#FAC358","black"),levels(primate.data$Activity_pattern))
plot(Orbit_area~Skull_length, data=primate.data,pch=21,
     bg=pt.cols[primate.data$Activity_pattern],log="xy",bty="n",
     xlab="skull length (cm)", ylab=expression(paste("orbit area (",mm^2,")")),
     cex=1.2,cex.axis=0.7,cex.lab=0.8)
##adding a legend
legend("bottomright",names(pt.cols),pch=21,pt.cex=1.2,pt.bg=pt.cols,cex=0.8)
##create a common set of x values to plot our different lines for each level of 
##the factor
xx<-seq(min(primate.data$Skull_length),max(primate.data$Skull_length),length.out=100)
##add lines for each level of the factor
lines(xx,exp(predict(primate.ancova, newdata=data.frame(Skull_length=xx,
                                                        Activity_pattern=as.factor(rep("Cathemeral",100))))),
      lwd=2,col=pt.cols["Cathemeral"])
lines(xx,exp(predict(primate.ancova, newdata = data.frame(Skull_length=xx,
                                                          Activity_pattern=as.factor(rep("Diurnal",100))))),
      lwd=2,col=pt.cols["Diurnal"])
lines(xx,exp(predict(primate.ancova, newdata = data.frame(Skull_length=xx,
                                                          Activity_pattern=as.factor(rep("Nocturnal",100))))),
      lwd=2,col=pt.cols["Nocturnal"])

#Again, I won't be doing the practice problems here, because I didn't do chapter 2's
#and they're dependant on chapter 2's.

####Chapter 4 ####
#Actually learning about the BM (Brownian Motion) this chapter
#Dr. Hjelmen described it to me as a drunkard's walk.
#Which makes more sense than the big math words the book is throwing at me.
#I may have really really needed to take more stats before doing this
#But it's too late to quit now.

##set values for the time steps and sigma squared parameter
t<-0:100
sig2<-0.01
##simulate a set of random changes
x<-rnorm(n=length(t)-1,sd=sqrt(sig2))
##compute their cummulative sum.
x<-c(0,cumsum(x))
##create a plot with nice margins
par(mar=c(5.1,4.1,2.1,2.1))
plot(t,x,type="l",ylim=c(-2,2),bty="n",xlab="time",ylab="trait value",las=1,cex.axis=0.8)

#Okay, we made a plot of the brownian motion. Interestingly, it seems to be the same plot 
#as that in the book.
#i would've assumed it would be different, since we didnt set a seed. Interesting.

#Now let's do the same thing though, a bunch of times.
##simulation number
nsim<-100
##create a matrix of random normal deviates
X<-matrix(rnorm(n=nsim*(length(t)-1),sd=sqrt(sig2)),nsim,length(t)-1)
##Calculate the comulative sum of these deviates
##aparently this makes it a simulation of Brownian Motion
X<-cbind(rep(0,nsim),t(apply(X,1,cumsum)))
##Plot the first one
par(mar=c(5.1,4.1,2.1,2.1))
plot(t,X[1,],ylim=c(-2,2),type="l",bty="n",xlab="time",ylab="trait value",las=1,cex.axis=0.8)
##Plot the rest?
invisible(apply(X[2:nsim,],1,function(x,t) lines(t,x),t=t))

#Even to me, It's clear that there doesn't appear to be a trend towards up, or down movement.
#Just expansion in general if we get enough lines going
#I'm pretty sure this is just saying random evolution
#Not like, pressured stuff
#but idk

##Let's make the value 1/10 as large as our last time (sigma^2) to show
#how much it impacts it

##create a small matrix of rndom normal deviateswith a smaller sd
X<-matrix(rnorm(n=nsim*(length(t)-1),sd=sqrt(sig2/10)), nsim,length(t)-1)
X<-cbind(rep(0,nsim),t(apply(X,1,cumsum)))
par(mar=c(5.1,4.1,2.1,2.1))
plot(t,X[1,],ylim=c(-2,2),type="l",bty="n",xlab="time",ylab="trait value",las=1,cex.axis=0.8)
invisible(apply(X[2:nsim,],1,function(x,t) lines(t,x),t=t))

#Now let's show how Brownian Motion is actually just sigma^2*t
##Calculate variance of columns
v<-apply(X,2,var)
plot(t,v,ylim=c(0,0.1),type="l",xlab="time",ylab="variance", bty="n",las=1,cex.axis=0.8)
lines(t,t*sig2/10,lwd=3,col=rgb(0,0,0,0.1))
legend("topleft",c("Observed variance","expected variance"),lwd=c(1,3),
       col=c("black",rgb(0,0,0,0.1)),bty="n",cex=0.8)

##Apparently at the end of the simulation, the variance there should just about be sigma^2.
##Whcih in our case is 0.001, multiplied by the total time elasped, 100 in our case, or
#about 0.1 for our grap
#Which we do see
var(X[,length(t)])
#Within a reasonable margin of error

#uhoh, we're getting a little crazy with it now
#We're doing this on phylogenies now
library(phytools)
object<-simBMphylo(n=6,t=100,sig2=0.1,fsize=0.8,cex.axis=0.6,cex.lab=0.8,las=1)
#ooooh, interesting
#So this one IS different than the one in the book.
#Both the tree and the graph.
#And it changes each time I run it. 
#I wonder why the last one didn't seem be so affected.

#We're doing it for 1000 now, so we're using a different function
##Pull the phylogeny out of the object we simulated
tree<-object$tree
##Simulate 1000 instances of BM on that tree
X<-fastBM(tree,nsim=100)
#Alright, now we're graphing it
par(las=1)
#We're making a scatter plot with this, I'm not 100% on the logic
#But it sounds like we'redoing good, according to the book.
pairs(t(X)[,tree$tip.label[6:1]],pch=19,col=make.transparent("blue",0.5),cex.axis=0.9)
#Okay, so maybe it's just because my trees were coming out a bit funky, but I'm not 
#getting the "easy" and "clear" that the book talks about
#I think C,D, and B all seem to be closely related based on their dots.
#And then like, A is mildly related to all the others, but less strongly
#And then E and F are almost all clouds rather than lines
#I believe that's why I'm seeing here.
#This will change each time I run the lines though, so IDK anymore

#Now we're going to practice fitting a Brownian model using likelihood in R
#Which the book says is easy
#And that we're going to practice with a bacteria mutation data set

bacteria.data<-read.csv("bac_rates.csv",row.names = 1)
head(bacteria.data)
bacteria.tree<-read.tree("bac_rates.phy")
print(bacteria.tree,printlen=2)
plotTree(bacteria.tree, ftype="i",fsize=0.5,ldw=1,mar=c(2.1,2.1,0.1,1.1))
axis(1,at=seq(0,1,length.out=5),cex.axis=0.8)
library(geiger)
name.check(bacteria.tree,bacteria.data)
genome_size<-bacteria.data[,"Genome_Size_Mb"]
genome_size
#oopsies, now our vector doesn't have names, just the numbers.
#Let's fix that.
names(genome_size)<-rownames(bacteria.data)
head(genome_size)
#Now we can fit the BM to our data.

#We're using Geiger's function of fitContinuous, which uses GM as the default
fitBM_gs<-fitContinuous(bacteria.tree,genome_size)
fitBM_gs
#sigsq is the MLE (Maximum Likliehood Estimate)
#The way we interprit this is that in one unit of time, we expect the independantly
#evolving lineages to accumlate about 25Mb worth of variance
#Our MLE's X0 value is close to 1.98 (the Z0 here)
#This suggests that the most likily state at the root of the model is a genome size of
#1.98 Mb

#Okay, we're doing the same thing, but with mutation rate.
mutation<-setNames(bacteria.data[,"Accumulation_Rate"],rownames(bacteria.data))
head(mutation)
#Okay, the book is talking about plotting it into a histogram, and getting a left skewed
#result, but if I plot it (and what the book shows) clearly shows a right skewed distribution
#That's how my stat's class taught me to read a graph like that
#So what gives?
#Let's make the plots anyway ig
par(mfrow=c(1,2),mar=c(6.1,4.1,2.1,1.1))
##Histogram of mutation accumulation rates on original scale
hist(mutation,main="",las=2,xlab="",cex.axis=0.7,cex.lab=0.9,
     breaks=seq(min(mutation),max(mutation),length.out=12))
mtext("(a)",adj=0,line=1)
mtext("rate",side=1,line=4,cex=0.9)
##histogram of mutation accumulation rates on a log scale
ln_mutation<-log(mutation)
hist(ln_mutation,main="",las=2,xlab="",cex.axis=0.7,cex.lab=0.9,
     breaks=seq(min(ln_mutation),max(ln_mutation),length.out=12))
mtext("(b)",adj=0,line=1)
mtext("ln(rate)",side=1,line=4,cex=0.9)
#Also in addition to my earlier complain about this being a right tailed test
#Uh, it might just be my eyes playing tricks on me but I'm like 50% sure that
#graph b is a couple pixels higher than graph a.
#Which I've somehow managed to also plot a "b" label on as well. Huh.
#Well, reran the code and it's gone now. So....
#Regardless of the book's flipping of the terminology I was taught, such a strongly
#skewed graph indicates that we don't really want to use BM on it
#Good thing we did the log version, cause that one looks a lot better to use
fitBM_ar<-fitContinuous(bacteria.tree,ln_mutation)
fitBM_ar
#okay, book is saying we pretty much use the same method of interpritation with 
#this one.
#BUT! Don't compare this one with th eprevious genome estimate thing. That's in a different
#unit. This one is in log scale

#Now we're looking in to phylogenetic signals.
#Specifically Bloomberg's K and Pagel's Lambda

#K compares variance among the clades vs variance within the clades on the tree.
#Variance among clades higher - High Phylogenetic Signal
#Variance within Clades higher - Low Phylogenetic signal
phylosig(bacteria.tree, genome_size)
#So, here we got 0.349525
#This is lower than we'd expect under evolution by Brownian Motion (1.0)
#Now we're hypothesis testing for it, which I don't fully understand
#We're running a bunch of simulations and comparing it, that much I get
K_gs<-phylosig(bacteria.tree,genome_size, test=TRUE, nsim=10000)
K_gs
#Wait, that's the same number we got, so what's the point, why did we do that?
par(mfrow=c(1,1),cex=0.8,mar=c(5.1,4.1,2.1,2.1))
##plot the null distribution and observed value of K
#is that what we did? Find the null distribution?
plot(K_gs,las=1,cex.axis=0.9)
#Ohhhh, okay.
#With this plot, the book is explaining to me, the observed value we got for K
#is still much higher than anything we'd expect to find if our data for genome size
#was random with respect to the phylogeny
#So it's still a strong phylogenetic signal, from my understanding.

#Now we do the same thing but for our mutation log thing we made
K_ar<-phylosig(bacteria.tree,ln_mutation,test=TRUE,nsim=10000)
K_ar
#Oh wow, 0.08, that's very really small.
#Let's plot it.
par(cex=0.8,mar=c(5.1,4.1,2.1,2.1))
plot(K_ar,las=1,cex.axis=0.9)
#Okay, so a very different plot.
#Here we see that not only is our K value very low, but its within the expected area
#Which means we can't reject the null hypothesis. 
#Thins are mutating randomly across the tree

#But what if we wanted to test for a K value that's smaller than the expected?
#There's no automatic way to do this in R, but it's still pretty easy

##simulate 10000 datasets
nullX<-fastBM(bacteria.tree,nsim=10000)
##for eadch carry out a test for phylogenetic signal and accumulate these into a vetor
#using sapply
nullK<-apply(nullX,2,phylosig,tree=bacteria.tree)
##calculate the p-values
Pval_gs<-mean(nullK<=K_gs$K)
Pval_gs
Pval_ar<-mean(nullK<=K_ar$K)
Pval_ar
#So, intersetingly, my output for the Pval_gs is very similar to the book's (0.2172)
#but my Pval_ar is not. (6e-04)
#I'll be so real chief, the book is telling me that we did the smaller calculations,
#But I'm confused AF with this
##let's visualize it
par(mfrow=c(1,2))
hist(c(nullK,K_gs$K),breaks=30,col="lightgrey",border="lightgrey",main="",xlab="K",
     las=1,cex.axis=0.7,cex.lab=0.9,ylim=c(0,4000))
##Actual value as an arrow
arrows(x0=K_gs$K,y0=par() $usr[4],y1=0,length=0.12,col=make.transparent("blue",0.5),lwd=2)
text(K_gs$K,0.96*par() $usr[4],paste("observed value of K 
                                     (P = ",round(Pval_gs,4),")",sep="",pos=4,cex=0.8))
#Oops, I messed that formatting up
#oh well, the legends/labels aren't the important thing I'm doing, so idc
mtext("(a)",line=1,adj=0)
##Now plot for mutation accumulation rate
hist(c(nullK,K_ar$K),breaks=30,col="lightgrey",border="lightgrey",main="",
    xlab="K",las=1,cex.axis=0.7,cex.lab=0.9,ylim=c(0,4000))
arrows(x0=K_ar$K, y0=par()$usr[4],y1=0,length=0.12,col=make.transparent("blue",0.5),lwd=2)
text(K_ar$K,0.96*par()$usr[4],paste("observed value of K (P=",round(Pval_ar,4),")",sep="",
                                    pos=4,cex=0.8))
#nice, that one's even worse looking
mtext("(b)",line=1,adj=0)
#Maybe i'm dumb, but this seems like it's still within the null hypothesis range
#not fully lower

#Well, onto the Lambda instead.
#Ah, and that's what the Lamda is for. Looking for values that are smaller than
#expected under BM
#We can also use phylosig for this, we just have to specifiy
phylosig(bacteria.tree,genome_size,method="lambda")
phylosig(bacteria.tree,ln_mutation,method="lambda")
#Both traits have lamda estimates that are close to 1. 
#Though it is higher in genome_size than for the mutation accumulation
#While this is similar to K in our case, this is not and should not
#ever be treated as a One to One comparison.
#in face, they probably SHOULD be different
#Okay, so apparently lambda=0 is a viable hypothesis test here, unlike K
#So we're going to do that now.
lambda_gs<-phylosig(bacteria.tree,genome_size,method="lambda",test=TRUE)
lambda_gs
lambda_ar<-phylosig(bacteria.tree,ln_mutation,method="lambda",test=TRUE)
lambda_ar
##Now plot the liklihood structures
##first we set plot parameters
par(mfrow=c(2,1),mar=c(5.1,4.1,2.1,2.1),cex=0.8)
##Plot the likelihood surfaces of lambda for each of our two traits
plot(lambda_gs,las=1,cex.axis=0.9,bty="n",xlim=c(0,1.1))
mtext("(a)",line=1,adj=0)
plot(lambda_ar,las=1,cex.axis=0.9,bty="n",xlim=c(0,1.1))
mtext("(b)",line=1,adj=0)
#What am I looking at?
#Book, bestie, you can't just drop a graph like this and expect me to figure it out
#Are we rejecting the null because the lambda goes over our values we got both times?
#Is that what I'm meant to take from this?
#Cause that appears to be where Lamda=0 is located on each graph

#Apparently now we're doing that for Lamda=1
#And the book says it's easier than when we did it for BM
LR_gs<--2*(lambda_gs$lik(1)-lambda_gs$logL)
LR_gs
#Now we're pulling in X^2? Why? What's that doing here?
#Bestie, you can't just throw this at me.
#And I hate the book variable names, so we're chaning this crap
#Back to using book character names
#Lief will be the Pval_lambda_gs
#And Jasmine will be the Pval_lambda_ar
#Cause thos suck to type out and I can't autofill them because they're all the same
Lief<-pchisq(LR_gs,df=1,lower.tail=FALSE)
Lief
#The book says that it tells us we cannot reject a null hypothesis of Lambda=1
#Is that because again, it's smaller than 1? So it's smaller than the critical value?
LR_ar<--2*(lambda_ar$lik(1)-lambda_ar$logL)
Jasmine<-pchisq(LR_ar,df=1,lower.tail=FALSE)
Jasmine
#pchisq gives us the probability of observing an equally or more extreme value of 
#our liklihood ratio than the other one we calculated
#Idk what that means bestie. 

##Other models of continous chaaracter evolution on phylogenies
#The Early Bust (EB) model
#This one decays over time. Sort of like a half-life in chemistry
#I should go play Half-Life. I got it on sale like a year ago
#And I haven't had time to check it out  yet ):
#anyway
##Set parameters of the EB process
sig2.0<-1.0
a<-0.04
t<-100
##Compute sigma^2 as a function of time under this process
sig2.t<-sig2.0*exp(-0.04*0:t)
##graph sigma^2 through time
par(mfrow=c(1,1),mar=c(5.1,4.1,2.1,2.1))
plot(0:t,sig2.t,type="l",xlab="time",ylab=expression(sigma^2),bty="l",las=1,cex.axis=0.9)
#The declining rate of evolution depicted here results in bigger differences between clades
#And smaller differnces within clades
#I assume this is like an idea for humans and monkies. 
#We're pretty darn differnt from a Chimp or Gorilla, but among ourselves (past hominins
#or however you spell it) there's not that much difference
#I mean, we were close enough to interbreed with these other groups
#We can also graph evolutionary change through time with this one too.
object<-simBMphylo(6,100,sig2=sig2.t[1:100],fsize=0.8,cex.axis=0.6,cex.lab=0.8,las=1)
#Like before, this one changes each time I run it
#We're not going over the Ornstein-Uhlenbeck (OU), model yet

#But! WE can compare all three methods to one another. So we're gonna do that
#with EB and OU.

##fit EB model to genome size
#Back to the awful variabe names
#Alright, I'm sick of _ and . and wahtever.
#The fitEB_gs from the book is now Barda
Barda<-fitContinuous(bacteria.tree,genome_size,model="EB")
Barda
#So, the book is saying that I should've gotten an error, but I didn't.
#Maybe that was fixed in an update to R Studio.
#I'm getting the same vablues otherwise
#like a being really relaly close to zero.

#fitOU_gs is going to be called Jarred
Jarred<-fitContinuous(bacteria.tree,genome_size,model="OU")
#This time I did get the error message, interesting.
Jarred
#Apparently this error this time indicates that our Jarred thing is at the upper bounds of
#the default optimization
#So we change them.
Jarred<-fitContinuous(bacteria.tree,genome_size,model="OU",bounds=list(alpha=c(0,10)))
Jarred
#Ther's no warning now, and both are optimized alpha value and our log liklihood are higher
#I think that makes sense, but I'm not sure. 
#I think it's unfare of the book to just throw that towards us
#when we haven't talked about OU yet
#I don't know what this means yet

#COmpare the EB and OU, and BM, we can just use an information criterion like the AIC
##accumulate AIC scores from our three models into a vector
#Gonna call this one Anna
Anna<-setNames(c(AIC(fitBM_gs),AIC(Barda),AIC(Jarred)),c("BM","EB","OU"))
Anna
#Here we want the LOWEST AIC score.
#In this case, that's OU, or Jarred.

#WE can also calculate Akaike weights, which I'll be so eal, I have no freaking idea 
#what that is, but I'm doing it anyway.
#Okay, this guy is the mutation one now. Doom is its name.
Doom<-fitContinuous(bacteria.tree,ln_mutation,model="EB")
Doom
#The OU model will be called Endon
Endon<-fitContinuous(bacteria.tree,ln_mutation,model="OU",bounds=list(alpha=c(0,100)))
##Put all the scores in a vector
#this is gonna be called Sharn
Sharn<-setNames(c(AIC(fitBM_ar),AIC(Doom),AIC(Endon)),c("BM","EB","OU"))
aic.w(Sharn)
##So, apparently this distribution means the weight falls 100% (nearly) onto the OU model
Endon
#something something stociastic model is better for our mutation accumulation.
#I'm out of time for this project
#So while I'll continue doing this book over the summer
#This is where I'm stopping so I ccan make the write up.
#Thank you for your time