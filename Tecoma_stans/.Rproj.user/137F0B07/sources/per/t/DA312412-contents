#------------------------------------------------
# Loading - Hoja_datos_proyecto
#------------------------------------------------
library("readxl", "openxlsx")
Hoja_datos_proyecto <-read_excel("Data/Hoja_datos_proyecto.xlsx", sheet = "Datos")
Hoja_datos_proyecto<-data.frame(Hoja_datos_proyecto)
#------------------------------------------------

Hoja_datos_proyecto<-na.omit(Hoja_datos_proyecto)

#------------------------------------------------
# Fig. Ternary plot: RMR, SMR, LMR, NAR, and TDM.
#------------------------------------------------
library(ggtern)
df<-data.frame(Hoja_datos_proyecto$Trat, 
               Hoja_datos_proyecto$NAR,
               Hoja_datos_proyecto$RGR,
               Hoja_datos_proyecto$RMR,
               Hoja_datos_proyecto$SMR,
               Hoja_datos_proyecto$LMR,
               Hoja_datos_proyecto$TDM)
names(df) <- c("Tratamiento", "NAR", "RGR", "RMR", "SMR", "LMR", "TDM")
df = df[with(df, order(-TDM)), ]
df<-na.omit(df)


#------------------------------------------------
pdf(file = "Results/Fig. Ternary plot.pdf", width = 8, height = 6)
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1,3,1,1))
a<-ggtern(data = df, aes(x = RMR, y = SMR, z = LMR)) +
  #the layers
  geom_mask() + #MASK UNDER POINTS
  geom_point(aes(fill = NAR,
                 size = TDM,
                 shape = Tratamiento)) +
  #scales
  scale_shape_manual(values = c(21,22,23,24,25,3)) +
  scale_size_continuous(range = c(1,  7)) +
  scale_fill_gradient(low = 'red', high = 'green') +
  #theme tweaks
  theme_bw()  +
  theme_showarrows() + 
  theme(legend.position      = c(0, 1),
        legend.justification = c(0, 1),
        legend.box.just      = 'left') +
  #tweak guides
  guides(shape= guide_legend(order   =1,
                             override.aes=list(size=5)),
         size = guide_legend(order   =3),
         fill = guide_colourbar(order=2)) +
  #labels and title
  labs(size = 'TDM',
       fill = "NAR") +
  ggtitle('') 
a<-a + scale_T_continuous(limits=c(.0,1))  + #(A,BC)
  scale_L_continuous(limits=c(0,1))  +    #(B,AC)
  scale_R_continuous(limits=c(0,1))       #(B,C)
print(a)
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. Relationship: RMR versus LMR
#------------------------------------------------
d <- Hoja_datos_proyecto[, c(4,3,5,6,7,8,10,14,16,17)]
hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
#hc.order <- order.dendrogram(as.dendrogram(hc))
#d <- d[ ,hc]#d[ ,hc.order]
gr <- as.factor(Hoja_datos_proyecto$Trat)

cols.key <- scales::muted(c('black', 'black', 'black', "black"))
cols.key <- adjustcolor(cols.key, alpha.f=1)
pchs.key <- c(19,15,18,17,24,25)

panel.hist <- function(x, ...) {
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col='gray', ...)
}
panel.cor <- function(x, y, ...){
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
  zcol <- lattice::level.colors(r, at=seq(-1, 1, length=81), col.regions=colorRampPalette(c(scales::muted('red'),'white',scales::muted('blue')), space='rgb')(81))
  ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
  polygon(ell, col=zcol, border=zcol, ...)
  text(x=.5, y=.5, lab=100*round(r, 2), cex=2, col='black')
  # pval <- cor.test(x, y, method='spearman', use='pairwise.complete.obs')$p.value
  # sig <- symnum(pval, corr=FALSE, na=FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', ' '))
  # text(.6, .8, sig, cex=2, col='gray20')
}
panel.scatter <- function(x, y){
  points(x, y, col=cols.key[gr], pch=pchs.key[gr], cex=1.15)
  lines(lowess(x, y))
}
#------------------------------------------------
pdf(file = "Results/Fig. Spearman correlations.pdf", width = 6, height = 6)
pairs(d,
      diag.panel=panel.hist,
      lower.panel=panel.scatter,
      upper.panel=panel.cor,
      gap=0.5,
      labels=gsub('\\.', '\n', colnames(d)),
      label.pos=0.7,
      cex.labels=1.4
)
#-------
library(ggplot2)
library(GGally)
ggpairs(d, columns = c("LDM","RDM","SDM","SLA","NAR"), title = "", upper = list(continuous = wrap("cor",method = "spearman", size = 3)),
        lower = list(continuous = wrap("smooth",alpha = 0.3,size = 0.1)),
        mapping = aes(color = Hoja_datos_proyecto$Trat))

dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. Relationships between DAP versus Height
#------------------------------------------------
library(cowplot)
lm_eqn = function(df, x, y){
  m=lm(x ~ y, df)#3rd degree polynomial
  summary_m<-summary(m)
  pvalue<-pf(summary_m$fstatistic[1],summary_m$fstatistic[2], summary_m$fstatistic[3],lower.tail = FALSE)
  pvalue<-ifelse(pvalue<0.001,"***", ifelse(pvalue<0.01, "**", ifelse(pvalue<0.05, "*","n.s.")))
  eq <- substitute(LDM == a + b %.% RDM*","~~italic(R)^2~"="~r2*p,
                   list(a = format(coef(m)[1], digits = 3),
                        b = format(coef(m)[2], digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 2),
                        p = format(pvalue)))
  as.character(as.expression(eq))
}
library(ggplot2)

#### RGR vs NAR
pmain <- ggplot(Hoja_datos_proyecto, aes(x = NAR, y = RGR, color = Trat))+
  geom_point(aes(size = TDM), alpha = 0.5, ) +
  ggpubr::color_palette("jco")+
  geom_smooth(method ="lm", formula = y ~ x) +
  ggpubr::color_palette("jco")+
  scale_size(expression(paste("TDM",)), range = c(0.1, 10)) + 
  xlab(label="NAR") + ylab(label="RGR") +
  # Adjust the range of points size
  ggpubr::color_palette("jco")

#pmain<-pmain + geom_text(data=piangua2[1,],aes(x = x, y = y,label =label), size=6, family="Times", parse = TRUE)

pmain<-pmain + annotate("text", x=0, y=0.25, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control","NAR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control","RGR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
pmain<-pmain + annotate("text", x=0, y=0.23, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro","NAR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro","RGR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
pmain<-pmain + annotate("text", x=0, y=0.21, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio","NAR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio","RGR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
pmain<-pmain + annotate("text", x=0, y=0.19, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro","NAR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro","RGR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
pmain

pmain<-pmain + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                  axis.text.x = element_text(color="black", size=12, angle=0),
                                  axis.text.y = element_text(color="black", size=12, angle=0), text = element_text(size = 13))
pmain <- pmain +ggpubr::color_palette("jco")

print(pmain)

##### RMR vs LMR
bmain <- ggplot(Hoja_datos_proyecto, aes(x = RMR, y = LMR, color = Trat))+
  geom_point(aes(size = TDM), alpha = 0.5, ) +
  ggpubr::color_palette("jco")+
  geom_smooth(method ="lm", formula = y ~ x) +
  ggpubr::color_palette("jco")+
  scale_size(expression(paste("TDM",)), range = c(0.1, 10)) + 
  xlab(label="RMR") + ylab(label="LMR") +
  # Adjust the range of points size
  ggpubr::color_palette("jco")

#pmain<-pmain + geom_text(data=piangua2[1,],aes(x = x, y = y,label =label), size=6, family="Times", parse = TRUE)

bmain<-pmain + annotate("text", x=0, y=0.25, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control","RMR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Control","LMR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
bmain<-pmain + annotate("text", x=0, y=0.23, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro","RMR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Hierro","LMR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
bmain<-pmain + annotate("text", x=0, y=0.21, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio","RMR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio","LMR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)
bmain<-pmain + annotate("text", x=0, y=0.19, label=lm_eqn(Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro",],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro","RMR"],Hoja_datos_proyecto[Hoja_datos_proyecto$Trat=="Silicio+Hierro","LMR"]), hjust=0, size=4,family="Times", face="italic", parse=TRUE)

bmain

bmain<-pmain + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                                  axis.text.x = element_text(color="black", size=12, angle=0),
                                  axis.text.y = element_text(color="black", size=12, angle=0), text = element_text(size = 13))
bmain <- pmain +ggpubr::color_palette("jco")
print(bmain)










# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = Hoja_datos_proyecto, aes(x = NAR, fill = Trat),
               alpha = 0.7, size = 0.7)+
  ggpubr::fill_palette("jco")

xdens2 <- axis_canvas(pmain, axis = "x")+
  geom_boxplot(data = Hoja_datos_proyecto, aes(x = NAR, fill = Trat),
               alpha = 0.7, size = 0.7)+
  ggpubr::fill_palette("jco")
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = Hoja_datos_proyecto, aes(x = RGR, fill = Trat),
               alpha = 0.7, size = 0.7)+
  coord_flip()+
  ggpubr::fill_palette("jco")

ydens2 <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_boxplot(data = Hoja_datos_proyecto, aes(x = RGR, fill = Trat),
               alpha = 0.7, size = 0.7)+
  coord_flip()+
  ggpubr::fill_palette("jco")
p1 <- insert_xaxis_grob(pmain, xdens2, grid::unit(.3, "null"), position = "top")
p1 <- insert_xaxis_grob(p1, xdens, grid::unit(.175, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens2, grid::unit(.3, "null"), position = "right")
p2 <- insert_yaxis_grob(p2, ydens, grid::unit(.175, "null"), position = "right")
ggdraw(p2)
#------------------------------------------------
pdf("Results/Fig. NAR versus RGR.pdf", width=10/1.157, height=6/1.157)
ggdraw(p2)
dev.off()
#------------------------------------------------









#------------------------------------------------
# Fig. PCA plot.
#------------------------------------------------
library(factoextra)
df.PCA<-Hoja_datos_proyecto[,-c(1)]
df.PCA = df.PCA[with(df.PCA, order(-TDM)), ]
df.PCA<-na.omit(df.PCA)

res.pca1 <- prcomp(df.PCA[, c(-1)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[,1]) # Only treatment

a1<-fviz_pca_ind(res.pca1, geom = c("point"), title="",
                 habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(19,15,18,17,24,25))

b1<-fviz_pca_var(res.pca1, col.var="coord", title="") 
b1<- b1 +  scale_color_gradient2(low="blue", mid="green", high="red", midpoint=0.50)
b1<- b1 + theme_minimal()

pdf(file = "Results/Fig. PCA.pdf", width = 9, height = 3.75)
plot_grid(b1,a1,
          labels=c(""),
          ncol = 2, nrow = 1)
dev.off()
#------------------------------------------------
library("FactoMineR")
library("factoextra")
pca <- PCA(Hoja_datos_proyecto[,-c(1,2)], graph = FALSE)

df<-data.frame(Hoja_datos_proyecto$Trat, pca$ind$coord[,1], pca$ind$coord[,2],pca$ind$coord[,3])
names(df)<-c("Tratamiento", "PC1", "PC2", "PC3")
str(df)

library(rgl)
library(car)
scatter3d(x = df$PC1, y = df$PC2, z = df$PC3, groups = as.factor(df$Tratamiento),
          xlab="PC1", ylab="PC2", zlab="PC3",axis.scales=FALSE,
          surface=FALSE, ellipsoid = TRUE,level=0.95, grid = TRUE, ellipsoid.alpha= 0)
#------------------------------------------------

