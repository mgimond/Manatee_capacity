#
#  This script computes the Manatee carrying capacity for 
#  several Florida warm water sites, then generates two
#  of the figures used in the referenced paper.
#
#  Reference:
#  Jane Provancha, Cynthia Taylor, Manny Gimond, and Michael Wild (2015)
#  Carrying Capacity Assessment Methodology Used to Evaluate 
#  Manatee Forage and Warm-water Sites in Florida. To Be Submitted.
# 
# Auhtor Manny Gimond
# Original version: 9/17/2015

options(scipen=999) # Disable scientific notation

library(triangle)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# Number of simulations to run
n <- 99999

# Set seed value for consistent results
# (note that the results published in the referenced paper
# were not generated using this seed; hence, results may 
# vary from those in the publication).
set.seed(2)

# Read site specific data
inFile <- read.csv("capacity.csv", header=T, na.strings = "NA")
attach(inFile)

# Manatee input parameters (non site specific data)
m.length <- rtriangle(n, 1.5, 4.0, 2.4)   # m
m.lbuf   <- rtriangle(n, 0.18, 0.35, 0.3) # m
m.width  <- rtriangle(n, 1, 1.25, 1.16)   # m
m.weight <- rtriangle(n, 500, 1200, 800)  # kg
m.cons   <- rtriangle(n, .12, .14, .13)   # Fraction consumption, C (bm/day)
m.area   <- (m.length + m.lbuf) * m.width # m2

# SAV biomass factors (non site specific data) 
sav.grow <- runif(n, 0.0052, 0.01) # Winter growth rate (fraction per day) 
bio.for  <- runif(n, 0.785, 1.62)  # Forage Biomass (kg/m2)

# Initialize variables
siteK <- matrix(nrow=n,ncol=length(Site))
forK  <- matrix(nrow=n,ncol=length(Site)) 
limK  <- matrix(nrow=n,ncol=length(Site))
colnames(siteK) <- Site
colnames(forK)  <- Site
colnames(limK)  <- Site 

# Start MC simulation
for (i in 1:length(Site)){
  z.lim    <- rtriangle(n, dmin[i], dmax[i], dpeak[i]) 
  loc.len  <- rtriangle(n, lmin[i], lmax[i], lpeak[i])
  loc.wid  <- width[i]
# Use flag from input file to determine site-specific equation for loc.area  
  if (flag[i] == 1){
    loc.area <- (1 - z.lim) * ( loc.len * loc.wid - (2 * m.length * loc.len))
  }else{
    if (flag[i] == 2){
      loc.area <- (1 - z.lim) * ( Area[i] - (m.length * Perimeter[i] * 0.5))
    }else{
        if (flag[i] == 3){
          loc.area <- (1 - z.lim) * ( loc.len * loc.wid - (2 * m.length * loc.len))
      }else{
          loc.area <- (1 - z.lim) * ( Area[i] - (m.length * Perimeter[i]))
      }
    }
  }
# Calculate site-specific SAV area
  loc.sav  <- SAVarea[i] * rtriangle(n, SAVmin[i], SAVmax[i], SAVpeak[i])    
  
# Add results to array
  siteK[,i] <- loc.area / m.area
  forK[,i]  <- loc.sav * bio.for * sav.grow / m.weight / m.cons
  y <- cbind(siteK[,i],forK[,i]) 
  limK[,i] <- apply(y, 1, FUN=function(j) {min(j, na.rm=T)}) 
}

# Compute quantiles for all sites
siteK.qt <- apply(siteK,2, function(x) quantile(x,  probs = seq(0,100,10)/100,na.rm=T))
forK.qt  <- apply(forK,2, function(x) quantile(x,  probs = seq(0,100,10)/100,na.rm=T))
limK.qt  <- apply(limK,2, function(x) quantile(x,  probs = seq(0,100,10)/100,na.rm=T))

# Output quantiles to comma delimited files 
write.csv(siteK.qt, file="siteK.csv")
write.csv(forK.qt, file="forK.csv")
write.csv(limK.qt, file="limK.csv")

detach(inFile)

# reshape tables (wide to long)
siteKlong <- siteK %>%
             as.data.frame %>%
             gather( key ="Site", value="Count") %>%
             mutate( K = as.factor("siteK"))
forKlong  <- forK %>%
             as.data.frame %>%
             gather( key ="Site", value="Count") %>%
             mutate( K = as.factor("forK"))
limKlong  <- limK %>%
             as.data.frame %>%
             gather( key ="Site", value="Count") %>%
             mutate( K = as.factor("limK"))

# Merge distribution files into one large file for plotting purposes
kmerge  <- rbind(siteKlong,forKlong,limKlong)
kmerge$K <- factor(kmerge$K, levels=c("forK","siteK","limK"))

# Order sites by median limK values
lev_med <- kmerge %>%  
  group_by(Site) %>% 
  filter(K == "limK") %>%
  summarise( a = median(Count, na.rm=TRUE)) %>%
  arrange(a) %>%
  select(Site) %>%
  .$Site  # This forces the output to a vector (and not a data.frame)

# Redefine level order (this allows ggplot to sort the
# facets by median limitingK values)
kmerge$Site <- factor(kmerge$Site , levels=lev_med)

# Generate violin plots for all sites (Figure 2 in report)
bks <- c( 100, 1000, 10000, 100000)
ggplot(kmerge , aes( K, Count )) + geom_violin(aes(fill =K)) + 
  facet_grid(Site ~ .) +coord_flip() + 
  theme(strip.text.y = element_text(size = 8, angle = 0))+ 
  scale_fill_manual(values=alpha(c("green", "orange","grey10"), c(0.4,0.4, 0.9))) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  scale_y_log10(breaks= (bks+1),labels=bks) +
  ylab("Manatee capacity (presented on a log scale)") +xlab("") +
  geom_text(data=data.frame(Count = 1000,K = "limK",lab = "Text", Site="Manatee Springs"), 
            label="Limit K is 0 (no forage)", cex=2.0) +
  labs(fill="")

# Export Figure 2 as an image
ggsave("Figure2.jpg",width=6,height=8, units="in")

# Plot simulation distributions for Blue Spring (Figure 5 in report)
Blue_spring <- kmerge %>%
     filter(Site == "Blue Spring" & K != "limK")

ggplot(Blue_spring, aes(fill=K, x=Count )) + geom_density(alpha =.3, aes(y = ..density..)) +
  scale_x_log10(breaks = c(200,500,1000)) + xlab("Manatee capacity (on a log scale)") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  scale_fill_manual(values=alpha(c("green", "orange","grey10"), c(0.6,0.6, 0.9)))

# Export plot as a vector graphic (used to create Figure 5)
ggsave("Blue_spring_example.svg",width=6,height=3, units="in")
