# Intro to phylogenetic modeling - E2M2 2025
# Visualizing your phylogeny
# Script written by Gwen Kettenburg, modified by Sophie Lockwood

# You have now made a phylogeny of Ranomafana lemur species. How can you communicate it? 
# With R, you can flip it, change the outgroup, highlight clades, change the size of text and bootstrap numbers, change the tip colors and symbols, add vector images to the tips, and so much more!
# MEGA is GUI interface that can be used to quickly look at and edit the tree, but R is more powerful and gives you many more options. 


rm(list=ls()) #clear your environment

# 1. Load packages specific to tree editing and visualization

# ggtree runs from bioconductor, so you may need to install it before installing ggtree
install.packages("BiocManager")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggtree")
install.packages("ggimage")
install.packages("ape")
install.packages("ggnewscale")

library(BiocManager)
library(tidyverse)
library(ggplot2)
#BiocManager::install("ggtree")
library(ggtree)
library(ggimage)
library(ape)
library(ggnewscale)


# 2. Load the tree file into R 

lemur.tree <- read.tree("ranomafana_lemurs_cytochromeB_align.raxml.support.tre") 

# Root the tree by identifying the tip label corresponding to the outgroup (here, we used Homo sapiens as the outgroup for our lemur phylogeny)
root.lemur.tree <- root(lemur.tree, which(lemur.tree$tip.label == "Homo_sapiens_cytB"))

# Check that the tree looks right before continuing
plot(root.lemur.tree)

# If you have a very large dataset of sequences from NCBI, it may be good to have a separate csv file of specific parameters to associate with the tree, like if you want to show sampling location, host, novel sequence, year sampled, and more. However, we do not need that for what we're doing here.

# 3. Let's explore what we can do with ggtree

# 3a. When adding features to plots, you can create (and save) versions that you add to, naming the object p is traditionally used. Here, we plot the base and most simple version of our phylogeny.
p <- ggtree(root.lemur.tree) 
p

# 3b. Let's add the tip labels back, and add some points to the tip labels too. If we wanted to change the color, size, or symbol of the tips or nodes to correspond with metadata (sampling location, trait value, etc.), we could do this using geom_tiplab() and geom_nodepoint()
p1 <- p + 
  geom_nodepoint() + 
  geom_tiplab(size=3)
p1


# For example, if you wanted to make the color of the label for the outgroup red so we can see it more easily, we can do so by defining a color scheme and assigning the color scheme using geom_tiplab()

colz = c("Hapalemur_aureus_cytB"="black", 
         "Hapalemur_griseus_meridionalis_cytB"="black", 
         "Prolemur_simus_cytB"="black", 
         "Eulemur_rubriventer_cytB"="black",
         "Eulemur_rufifrons_cytB"="black", 
         "Varecia_variegata_cytB"="black", 
         "Propithecus_edwardsi_cytB"="black", 
         "Cheirogaleus_major_cytB"="black",
         "Microcebus_rufus_cytB"="black", 
         "Lepilemur_microdon_cytB"="black", 
         "Daubentonia_madagascariensis_cytB"="black", 
         "Homo_sapiens_cytB"="red")

p2 <- p1 + 
  geom_nodepoint(color="lightblue", shape=16, size=5) + 
  geom_tippoint(color="indianred1", shape=8, size=1) +
  geom_tiplab(color=colz, size=3)#
p2

# 3c. Looks like the bootstrap values never made it onto the tree, they are stored in the tree file that you imported, you just need to tell
# ggtree to display them, easy peasy 
p3 <- p2 + geom_text2(aes(subset = !isTip, label=label), size = 2)
p3 # check it out


# We can flip the tree around to change the shape too, first we need a version of the tree without the labels, since they will be the wrong direction
p4 <- p + geom_nodepoint(color="lightblue", shape=16, size=5) + geom_tippoint(color="indianred1", shape=8, size=1) + geom_text2(aes(subset = !isTip, label=label))
p4 #check it out

p5 <- open_tree(p4, 180) + geom_tiplab() #ggtree feature opens the tree specified tree view into a fan shape 
# and rotates it 180 degrees, we are re-adding the labels on top of the fan shape
p5 # check it out

# Let's go back to our original tree and add some clade labels! Let's look at the tree again and think, what could be some clades?
# First, we can shorten the branch length a little to see everything better, and also add a scale bar for the substitution rate
p6 <- p3+geom_treescale(1)
p6 # check it out

# There's lepilemuridae for Lepilemur
# Daubentoniidae for Daubentonia
# Lemuridae: Hapalemur, Prolemur, Varecia, and Eulemor go here
# Indriidae for Propithecus
# Cheirogaleidae for microcebus and cheirogaleus

# We need to have 5 clades to put on this phylogeny, but we need the node numbers to tell ggtree what to label

# ggtree labels the nodes with numbers, so let's get those so we know where we are rotating the tree
x <- as_tibble(root.lemur.tree) # look at it this way, which is a table form and fairly easy to read
print(x,n=22) # see all the rows, 22 rows gives the whole data set though
# Here we want to use the parent category for telling ggtree what to do

# Here is another way of looking at the clade numbers
ggtree(root.lemur.tree) + geom_text(aes(label=node), hjust=-.3)


#Two ways you can highlight the clades we have specified

#Method number 1
p7 <- p6 + geom_cladelab(node=10, label="lepilemuridae", align=TRUE,  
                    offset = 0.20, textcolor='deepskyblue') +
  
          geom_cladelab(node=11, label="daubentoniidae", align=TRUE,  
                    offset = 0.20, textcolor='indianred1')+
  
          geom_cladelab(node=7, label="indriidae", align=TRUE,  
                    offset = 0.20, textcolor='mediumorchid1')+
  
          geom_cladelab(node=c(1,2,3,4,5,6), label="lemuridae", align=TRUE,  
                    offset = 0.20, textcolor='slateblue1')+
  
          geom_cladelab(node=c(8,9), label="cheirogaleidae", align=TRUE,  
                    offset = 0.20, textcolor='violetred')
p7 #check it out


#Method number 2
p8 <- p6 + geom_hilight(node=10, fill="deepskyblue", alpha=0.6, extend=1) +
  geom_hilight(node=11, fill="indianred1", alpha=0.6, extend=1) +
  geom_hilight(node=7, fill="mediumorchid1", alpha=0.6, extend=1) +
  geom_hilight(node=17, fill="slateblue1", alpha=0.6, extend=1) +
  geom_hilight(node=22,fill="violetred", alpha=0.6, extend=1)
p8 #check it out

#For further reading, which is a wonderful and easy resource: https://yulab-smu.top/treedata-book/index.html

