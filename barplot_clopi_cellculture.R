library(ggpubr)
library(rstatix)

getwd()

data=read.delim("dataset.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "25","50","100"))

####Clopidogrel S9 24h
p <- ggplot(data, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
               position=position_dodge(.9))

p + scale_fill_brewer(palette="Reds") + 
    ggtitle("Clopidogrel doses by supplementaion S9 fraction 24h - HEPG2") +
    xlab("Doses (µM)") + ylab("DNA fragmentation (%)")+
 
    theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
    annotate("text", x = c(2,2.5), y = c(21,47), label = "italic(P) < 0.05 ",
           parse = TRUE)+
    annotate("segment", x = c(1,1), xend = c(3,4), y = c(20,45), yend = c(20,45),
           colour = "black")+
    ylim(0, 60)


####Clopidogrel S9 48h

data=read.delim("dataset48.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "25","50","100"))
p1 <- ggplot(data, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))
p1 + scale_fill_brewer(palette="Reds") + 
  ggtitle("Clopidogrel doses by supplementaion S9 fraction 48h - HEPG2") +
  xlab("Doses (µM)") + ylab("DNA fragmentation (%)")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
  annotate("text", x = c(2,2.5), y = c(21,57), label = "italic(P) < 0.05 ",
           parse = TRUE)+
  annotate("segment", x = c(1,1), xend = c(3,4), y = c(20,55), yend = c(20,55),
           colour = "black")+
  ylim(0, 60)

####Clopidogrel S9 24h - HUVEC

data=read.delim("dataset_huvec.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "25","50","100"))


p <- ggplot(data, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Reds") + 
  ggtitle("Clopidogrel doses by supplementaion S9 fraction 24h - HUVEC") +
  xlab("Doses (µM)") + ylab("DNA fragmentation (%)")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  ylim(0, 60)
 #+
  
 # annotate("text", x = c(2,2.5), y = c(21,47), label = "italic(P) < 0.05 ",
 #          parse = TRUE)+
 #annotate("segment", x = c(1,1), xend = c(3,4), y = c(20,45), yend = c(20,45),
 #          colour = "black")

####Clopidogrel S9 48h - HUVEC

data=read.delim("dataset48_huvec.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "25","50","100"))
p1 <- ggplot(data, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))
p1 + scale_fill_brewer(palette="Reds") + 
  ggtitle("Clopidogrel doses by supplementaion S9 fraction 48h - HUVEC") +
  xlab("Doses (µM)") + ylab("DNA fragmentation (%)")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
  annotate("text", x = c(2.5), y = c(37), label = "italic(P) < 0.05 ",
           parse = TRUE)+
  annotate("segment", x = c(1), xend = c(4), y = c(35), yend = c(35),
           colour = "black")+
  ylim(0, 60)





data=read.delim("dataheatmapfoldchange.txt", header = F, sep = "\t")
rownames(data)=c("miR-26a", "HMGA2", "EIF4G2", "miR-15b")
colnames(data)=c("0µM",	"25µM",	"50µM",	"100µM")
head(data)
library(pheatmap)

pheatmap(log(data), cutree_cols = 1, cluster_rows = F, cluster_cols = F,
         show_colnames =T,
         cellwidth = 30, cellheight = 30, fontsize = 12)
         # annotation_col=myannotation,
         #main= "Heatmap log Fold-Change miRNA and mRNA expression")



data=read.delim("boxplot_expression.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "25","50","100"))
data$Genes <- factor(data$Genes, levels=c("miR-26a", "HMGA2", "EIF4G2", "miR-15b"))

p1 <- ggplot(data, aes(x=dose, y=log(len,2), fill=Genes)) + 
  geom_bar(stat="identity", position=position_dodge())
  #geom_errorbar(aes(ymin=log(len-sd,2), ymax=log(len+sd,3)), width=.2,
  #              position=position_dodge(.9))
p1 + scale_fill_brewer(palette="Reds") + 
  ggtitle("miRNA and mRNA expression 24h - HEPG2") +
  xlab("Doses (µM)") + ylab("log2 Fold-change")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
 # annotate("text", x = c(2.5), y = c(37), label = "italic(P) < 0.05 ",
 #           parse = TRUE)+
  annotate("segment", x = c(0,0), xend = c(5,5), y = c(0.5,-0.5), yend = c(0.5,-0.5),
           colour = "red")+
  ylim(log(0.5,2), log(2.5,2))




data=read.delim("dataheatmapfoldchange_antimiRNA.txt", header = F, sep = "\t")
rownames(data)=c("miR-26a", "HMGA2")
colnames(data)=c("0nM",	"50nM",	"100nM")
head(data)
library(pheatmap)

pheatmap(log(data), cutree_cols = 1, cluster_rows = F, cluster_cols = F,
         show_colnames =T,
         cellwidth = 30, cellheight = 30, fontsize = 12)
# annotation_col=myannotation,
#main= "Heatmap log Fold-Change miRNA and mRNA expression")



data=read.delim("dataheatmapfoldchange_mimicmiRNA.txt", header = F, sep = "\t")
rownames(data)=c("EIF4G2", "miR-15b")
colnames(data)=c("0nM",	"50nM",	"100nM")
head(data)
library(pheatmap)

pheatmap(log(data), cutree_cols = 1, cluster_rows = F, cluster_cols = F,
         show_colnames =T,
         cellwidth = 30, cellheight = 30, fontsize = 12)
# annotation_col=myannotation,
#main= "Heatmap log Fold-Change miRNA and mRNA expression")



data=read.delim("boxplot_expression_anti.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "50","70"))
data$Genes <- factor(data$Genes, levels=c("miR-26a", "HMGA2"))

p1 <- ggplot(data, aes(x=dose, y=log(len,2), fill=Genes)) + 
  geom_bar(stat="identity", position=position_dodge())
#geom_errorbar(aes(ymin=log(len-sd,2), ymax=log(len+sd,3)), width=.2,
#              position=position_dodge(.9))
p1 + scale_fill_brewer(palette="Reds") + 
  ggtitle("miRNA and mRNA expression 24h - HEPG2 - Anti-miRNA-26a") +
  xlab("Anti-miRNA Concentration (nM)") + ylab("log2 Fold-change")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
  # annotate("text", x = c(2.5), y = c(37), label = "italic(P) < 0.05 ",
  #           parse = TRUE)+
  annotate("segment", x = c(0,0), xend = c(5,5), y = c(0.5,-0.5), yend = c(0.5,-0.5),
           colour = "red")+
  ylim(log(0.4,2), log(2.8,2))



data=read.delim("boxplot_expression_mimic.txt", header = T, sep = "\t")
head(data)
data$dose <- factor(data$dose, levels=c("0", "50","70"))
data$Genes <- factor(data$Genes, levels=c("miR-15b", "EIF4G2"))

p1 <- ggplot(data, aes(x=dose, y=log(len,2), fill=Genes)) + 
  geom_bar(stat="identity", position=position_dodge())
#geom_errorbar(aes(ymin=log(len-sd,2), ymax=log(len+sd,3)), width=.2,
#              position=position_dodge(.9))
p1 + scale_fill_brewer(palette="Reds") + 
  ggtitle("miRNA and mRNA expression 24h - HEPG2 - mimic-miRNA-15b") +
  xlab("Mimic-miRNA Concentration (nM)") + ylab("log2 Fold-change")+
  
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid = element_line(colour = "azure3"))+
  
  # annotate("text", x = c(2.5), y = c(37), label = "italic(P) < 0.05 ",
  #           parse = TRUE)+
  annotate("segment", x = c(0,0), xend = c(5,5), y = c(0.5,-0.5), yend = c(0.5,-0.5),
           colour = "red")+
  ylim(log(0.3,2), log(5.9,2))







data_arvc<-read.csv("boxplot_metclop_mimic.txt", sep = "\t", header = T)
head(data_arvc)

bp <- ggplot(data_arvc, aes(x=as.character(dose), y=len, fill=as.character(dose))) + 
  geom_boxplot()+
  labs(title="Clopidogrel metabolites by mimic-miNRA-15b",x="mimic-miRNA Concentration (nM)", y = "Met. Clopidogrel [ng/L]",
       fill='mimic [nM]')
  

bp +  theme(plot.title = element_text(color="black", size=14, face="bold"),
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            axis.text = element_text(size=12, face="bold"),
            legend.title = element_text(color="black", size=14, face="bold"),
            legend.text = element_text(size=12, face="bold"),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid = element_line(colour = "azure3"))+
  scale_fill_brewer(palette="Reds")+
  annotate("text", x = c(2), y = c(62), label = "italic(P) < 0.05 ",
           parse = TRUE)+
  annotate("segment", x = c(1), xend = c(3), y = c(60), yend = c(60),
           colour = "black")




data_arvc<-read.csv("boxplot_metclop_anti.txt", sep = "\t", header = T)
head(data_arvc)

bp <- ggplot(data_arvc, aes(x=as.character(dose), y=len, fill=as.character(dose))) + 
  geom_boxplot()+
  labs(title="Clopidogrel metabolites by anti-miNRA-26a",x="mimic-miRNA Concentration (nM)", y = "Met. Clopidogrel [ng/L]",
       fill='anti-miRNA [nM]')


bp +  theme(plot.title = element_text(color="black", size=14, face="bold"),
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            axis.text = element_text(size=12, face="bold"),
            legend.title = element_text(color="black", size=14, face="bold"),
            legend.text = element_text(size=12, face="bold"),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid = element_line(colour = "azure3"))+
  scale_fill_brewer(palette="Reds")+
  annotate("text", x = c(1.5,2), y = c(42,62), label = "italic(P) < 0.05 ",
           parse = TRUE)+
  annotate("segment", x = c(1,1), xend = c(2,3), y = c(40,60), yend = c(40,60),
           colour = "black")


data_arvc<-read.csv("boxplot_agrega_anti.txt", sep = "\t", header = T)
head(data_arvc)

bp <- ggplot(data_arvc, aes(x=as.character(dose), y=len, fill=as.character(dose))) + 
  geom_boxplot()+
  labs(title="Inhibition of platelet aggregation by anti-miNRA-26a",x="anti-miNRA Concentration (nM)", y = "Inhibition of platelet aggregation (%)",
       fill='anti-miRNA [nM]')


bp +  theme(plot.title = element_text(color="black", size=14, face="bold"),
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            axis.text = element_text(size=12, face="bold"),
            legend.title = element_text(color="black", size=14, face="bold"),
            legend.text = element_text(size=12, face="bold"),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid = element_line(colour = "azure3"))+
  scale_fill_brewer(palette="Reds")




data_arvc<-read.csv("boxplot_agrega_mimic.txt", sep = "\t", header = T)
head(data_arvc)

bp <- ggplot(data_arvc, aes(x=as.character(dose), y=len, fill=as.character(dose))) + 
  geom_boxplot()+
  labs(title="Inhibition of platelet aggregation by mimic-miNRA-15b",x="mimic-miNRA Concentration (nM)", y = "Inhibition of platelet aggregation (%)",
       fill='mimic-miRNA [nM]')


bp +  theme(plot.title = element_text(color="black", size=14, face="bold"),
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            axis.text = element_text(size=12, face="bold"),
            legend.title = element_text(color="black", size=14, face="bold"),
            legend.text = element_text(size=12, face="bold"),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid = element_line(colour = "azure3"))+
  scale_fill_brewer(palette="Reds")
