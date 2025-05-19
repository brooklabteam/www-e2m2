# Intro to phylogenetic modeling - E2M2 2025
# Ranomafana lemur phylogeny
# Script written by Gwen Kettenburg, updated by Sophie Lockwood

# Okay! you have your phylogeny file, what can you do with it? With R, you can flip it, change the outgroup, highlight clades, change the size of text and bootstrap numbers, change the top colors and symbols, add vector images to the tips, and so much more! MEGA is a GUI interface and can be used to quickly look at the tree before doing more with it, and you can edit in it as well, but R is much more powerful and you have many more options. 

###############################################################################
#First, let's load some packages in R that are specific to tree editing and visualization
###############################################################################

rm(list=ls()) #clear your environment

# ggtree runs from bioconductor, so you may need to install it before getting ggtree
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


#####################################
# Second, load the tree file into R #
#####################################

lemur.tree <- read.tree("lemur_tree_newick.txt") #loading the tree file into R


# Once you have loaded the tree file, you can tell R which tip corresponds to the root (this is the outgroup sequence that you have included as something) that is older than the lemur sequences. Here, Homo sapiens is used as the outgroup.
root.lemur.tree <- root(lemur.tree, which(lemur.tree$tip.label == "Homo_sapiens_cytB")) 

# Check how the tree looks before doing anything else!
plot(root.lemur.tree) 

# If you have a very large dataset of sequences from NCBI, it may be good to have a separate csv file of specific metadata to associate with the tree. For example,if you want to indicate sampling location, host species, whether the sequence is novel, sampling year, etc., these data could be included in the csv file. This is especially important in making viral phylogenies. However, we do not need that for what we are doing here. 

############################################
# Let's explore what we can do with ggtree #
############################################

# When adding features to plots, it is important to save versions of it that you can add to, p is traditionally used

p <- ggtree(root.lemur.tree) #base and most simple version of our phylogeny
p

# Let's add the tip labels back, and add some points to the tip labels too so we can make them colorful later on

p1 <- p + 
  geom_tippoint() + # adding tip points
  geom_tiplab(size=3) # adding original tip labels 
p1 # check it out

#It's easy to add some personalization here, putting colors and shapes inside the parenthesis for the commands to make nodes and tip labels

# You pick the colors and shapes! Let's also make the outgroup text red so we can see it more easily, but first we need to manually define a color scheme

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

# Now, we can make the phylogeny nice looking. We take the skeleton of p2 and add different colors and shapes to it.

p2 <- p1 + 
  geom_nodepoint(color="lightblue", shape=16, size=5) + # add blue node points
  geom_tippoint(color="indianred1", shape=8, size=1) + # add red tip points
  geom_tiplab(color=colz, size=3) # make outgroup label red 
p2 # check it out

# Looks like the bootstrap values never made it onto the tree, they are stored in the tree file that you imported, you just need to tell ggtree to display them, easy peasy 
p3 <- p2 + 
  geom_text2(aes(subset = !isTip, label=label)) # add bootstrap values to nodes
p3 # check it out

# We can flip the tree around to change the shape too, first we need a version of the tree without the labels, since they will be the wrong direction
p4 <- p + 
  geom_nodepoint(color="lightblue", shape=16, size=5) + # add blue node points
  geom_tippoint(color="indianred1", shape=8, size=1) + # add red tip points
  geom_text2(aes(subset = !isTip, label=label)) # add bootstrap values to nodes
p4 #check it out

# We can change the shape of the tree. Here, the open_tree function of ggtree opens the specified tree view into a fan shape and rotates it 180 degrees. 
p5 <- open_tree(p4, 180) + 
  geom_tiplab() #add tip labels again
p5 # check it out

# Let's go back to our original tree and add some clade labels! Let's look at the tree again and think, what could be some clades? First, we can shorten the branch length a little to see everything better, and also add a scale bar for the substitution rate
p6 <- p3 + 
  geom_treescale(1)
p6 # check it out

# There's lepilemuridae for Lepilemur
# Daubentoniidae for Daubentonia
# Lemuridae: Hapalemur, Prolemur, Varecia, and Eulemor go here
# Indriidae for Propithecus
# Cheirogaleidae for microcebus and cheirogaleus

# We need to have 5 clades to put on this phylogeny, but we need the node numbers to tell ggtree what to label. ggtree labels the nodes with numbers, so let's get those so we know where we are rotating the tree. 

x <- as_tibble(root.lemur.tree)
print(x,n=22) 

# Here is another way of looking at the clade numbers
ggtree(root.lemur.tree) + 
  geom_text(aes(label=node), hjust=-.3)


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

################################################################
# For further reading, which is a wonderful and easy resource: #
# https://yulab-smu.top/treedata-book/index.html               #
################################################################

