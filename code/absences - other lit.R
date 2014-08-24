library(ggplot2)

# Import the data for each study 
LIT_bornette <- read.csv("bornette.csv")
LIT_edvardsen <- read.csv("edvardsen_okland.csv")
LIT_vestergaard <- read.csv("vestergaard_sand-jensen.csv")
LIT_heegaard <- read.csv("heegaard.csv")
LIT_trei <- read.csv("trei_pall.csv")
LIT_palmer <- read.csv("palmer.csv")
LIT_van_zuidman <- read.csv("van_zuidman.csv")
LIT_dugan <- read.csv("dugan.csv")

head(LIT_bornette)
head(LIT_edvardsen)
head(LIT_vestergaard)
head(LIT_heegaard)
head(LIT_trei)
head(LIT_palmer)
head(LIT_van_zuidman)
head(LIT_dugan)

######################## 
# Bornette et al. 1998 #
# 23 cut-off channels  #
# Rhone River, France  #
########################
sp_freq_plot_bornette <- ggplot(LIT_bornette,aes(x=reorder(Species, -Frequency),y=Frequency,fill=Group))
sp_freq_plot_bornette <- sp_freq_plot_bornette + geom_bar(stat="identity")
sp_freq_plot_bornette <- sp_freq_plot_bornette + theme_classic(base_size=10)
sp_freq_plot_bornette <- sp_freq_plot_bornette + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_bornette <- sp_freq_plot_bornette + xlab("Species")
sp_freq_plot_bornette <- sp_freq_plot_bornette + ylab("# of waterbodies found in")
sp_freq_plot_bornette <- sp_freq_plot_bornette + geom_text(aes(y=Frequency+0.25,label=Label))
sp_freq_plot_bornette <- sp_freq_plot_bornette + ggtitle("Bornette et al. 1998 - 23 cut-off channels Rhone R., France")
sp_freq_plot_bornette

ggsave("sp_freq_plot_bornette.jpg",sp_freq_plot_bornette,height=8,width=11)


########################### 
# Edvardsen & Okland 2006 #
# 64 agricultural ponds   # 
# SE Norway               #
###########################
# NOT INCLUDING "TERRESTRIAL PLANTS"
sp_freq_plot_edvardsen <- ggplot(subset(LIT_edvardsen, LIT_edvardsen$Group!="Terrestrial plant"),
                                 aes(x=reorder(Species, -Frequency),y=Frequency,fill=Group))
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + geom_bar(stat="identity")
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + theme_classic(base_size=12)
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + xlab("Species")
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + ylab("# of waterbodies found in")
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + geom_text(aes(y=Frequency+1,label=Label))
sp_freq_plot_edvardsen <- sp_freq_plot_edvardsen + ggtitle("Edvardsen & Okland 2006 - 64 agric. ponds SE Norway")
sp_freq_plot_edvardsen

ggsave("sp_freq_plot_edvardsen.jpg",sp_freq_plot_edvardsen,height=8,width=11)

##################################
# Vestergaard & Sand-Jensen 2000 #
# 82 lakes                       #
# Denmark                        #                
##################################
sp_freq_plot_vestergaard <- ggplot(LIT_vestergaard, aes(x=reorder(Species, -Frequency),y=Frequency,fill=Group))
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + geom_bar(stat="identity")
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + theme_classic(base_size=10)
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + xlab("Species")
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + ylab("# of waterbodies found in")
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + geom_text(aes(y=Frequency+1,label=Label))
sp_freq_plot_vestergaard <- sp_freq_plot_vestergaard + ggtitle("Vestergaard & Sand-Jensen - 82 lakes Denmark")
sp_freq_plot_vestergaard

ggsave("sp_freq_plot_vestergaard.jpg",sp_freq_plot_vestergaard,height=8,width=11)

#############################
# Trei & Pall 2004          # 
# 40 sites, 29 reaches      #
# Saaremaa Island (Estonia) #
#############################
sp_freq_plot_trei <- ggplot(LIT_trei, aes(x=reorder(Species, -Frequency_Saaremaa),y=Frequency_Saaremaa,fill=Group))
sp_freq_plot_trei <- sp_freq_plot_trei + geom_bar(stat="identity")
sp_freq_plot_trei <- sp_freq_plot_trei + theme_classic(base_size=12)
sp_freq_plot_trei <- sp_freq_plot_trei + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_trei <- sp_freq_plot_trei + xlab("Species")
sp_freq_plot_trei <- sp_freq_plot_trei + ylab("# of waterbodies found in")
sp_freq_plot_trei <- sp_freq_plot_trei + geom_text(aes(y=Frequency_Saaremaa+1,label=Label))
sp_freq_plot_trei <- sp_freq_plot_trei + ggtitle("Trei & Pall - 29 reaches (40 sites) - Saaremaa Island (Estonia)")
sp_freq_plot_trei

ggsave("sp_freq_plot_trei.jpg",sp_freq_plot_trei,height=8,width=11)

########################
# Heegaard et al. 2001 #
# 574 lakes            #
# N. Ireland           #                
########################
sp_freq_plot_heegaard <- ggplot(LIT_heegaard, aes(x=reorder(Species, -Frequency),y=Frequency,fill=Label))
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + geom_bar(stat="identity")
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + theme_classic(base_size=12)
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + xlab("Species")
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + ylab("# of waterbodies found in")
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + geom_text(aes(y=Frequency+1,label=Label))
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + ggtitle("Heegaard et al - 574 lakes N. Ireland \n32 most common spp. (85 total)")
sp_freq_plot_heegaard <- sp_freq_plot_heegaard + theme(legend.position="none")
sp_freq_plot_heegaard

ggsave("sp_freq_plot_heegaard.jpg",sp_freq_plot_heegaard,height=8,width=11)

######################
# Palmer et al. 1992 #
# 1124 water bodies  # 
# Britain            #                
######################
sp_freq_plot_palmer <- ggplot(LIT_palmer, aes(x=reorder(Species, -Frequency),y=Frequency,fill=Label))
sp_freq_plot_palmer <- sp_freq_plot_palmer + geom_bar(stat="identity")
sp_freq_plot_palmer <- sp_freq_plot_palmer + theme_classic(base_size=12)
sp_freq_plot_palmer <- sp_freq_plot_palmer + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_palmer <- sp_freq_plot_palmer + xlab("Species")
sp_freq_plot_palmer <- sp_freq_plot_palmer + ylab("# of waterbodies found in")
sp_freq_plot_palmer <- sp_freq_plot_palmer + geom_text(aes(y=Frequency+1,label=Label))
sp_freq_plot_palmer <- sp_freq_plot_palmer + ggtitle("Palmer et al - 1124 water bodies Britain")
sp_freq_plot_palmer <- sp_freq_plot_palmer + theme(legend.position="none")
sp_freq_plot_palmer

ggsave("sp_freq_plot_palmer.jpg",sp_freq_plot_palmer,height=8,width=11)


###########################
# van Zuidman et al. 2011 #
# 9 diches                #
# Netherlands             #           
###########################
sp_freq_plot_van_zuidman <- ggplot(LIT_van_zuidman, aes(x=reorder(Species, -Frequency),y=Frequency,fill=Label))
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + geom_bar(stat="identity")
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + theme_classic(base_size=12)
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + xlab("Species")
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + ylab("# of waterbodies found in")
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + geom_text(aes(y=Frequency+.25,label=Label))
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + ggtitle("Van Zuidman et al - 9 ditches Netherlands")
sp_freq_plot_van_zuidman <- sp_freq_plot_van_zuidman + theme(legend.position="none")
sp_freq_plot_van_zuidman

ggsave("sp_freq_plot_van_zuidman.jpg",sp_freq_plot_van_zuidman,height=8,width=11)


#####################
# Dugan et al 2006  #
# 1989              #
# 1124 water bodies # 
# Great Britain     #
#####################
sp_freq_plot_duigan_1989 <- ggplot(LIT_dugan, aes(x=reorder(Species, -Frequency_1989),y=Frequency_1989,fill=Label))
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + geom_bar(stat="identity")
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + theme_classic(base_size=12)
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + xlab("Species")
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + ylab("# of waterbodies found in")
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + geom_text(aes(y=Frequency_1989+.25,label=Label))
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + ggtitle("Duigan et al - 1989 - 1124 water bodies Britain")
sp_freq_plot_duigan_1989 <- sp_freq_plot_duigan_1989 + theme(legend.position="none")
sp_freq_plot_duigan_1989

ggsave("sp_freq_plot_duigan_1989.jpg",sp_freq_plot_duigan_1989,height=8,width=11)


#####################
# Dugan et al 2006  #
# 2004              #
# 1124 water bodies # 
# Great Britain     #
#####################
sp_freq_plot_duigan_2004 <- ggplot(LIT_dugan, aes(x=reorder(Species, -Frequency_2004),y=Frequency_2004,fill=Label))
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + geom_bar(stat="identity")
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + theme_classic(base_size=12)
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + xlab("Species")
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + ylab("# of waterbodies found in")
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + geom_text(aes(y=Frequency_2004+.25,label=Label))
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + ggtitle("Duigan et al - 2004 - 3447 water bodies Britain")
sp_freq_plot_duigan_2004 <- sp_freq_plot_duigan_2004 + theme(legend.position="none")
sp_freq_plot_duigan_2004

ggsave("sp_freq_plot_duigan_2004.jpg",sp_freq_plot_duigan_2004,height=8,width=11)


############################
# WA State - D. of Ecology #
# 1974 - 2014              #
# 502 lakes                #
# 1371 surveys (total)     #
# Only 1 survey/lake used  #
############################
sp_freq_plot_WA <- ggplot(dataWA_SPfreq,aes(x=reorder(species, -frequency),y=frequency,fill=label))
sp_freq_plot_WA <- sp_freq_plot_WA + geom_bar(stat="identity")
sp_freq_plot_WA <- sp_freq_plot_WA + theme_classic(base_size=10)
sp_freq_plot_WA <- sp_freq_plot_WA + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_WA <- sp_freq_plot_WA + xlab("Species")
sp_freq_plot_WA <- sp_freq_plot_WA + ylab("# of waterbodies found in")
sp_freq_plot_WA <- sp_freq_plot_WA + geom_text(aes(y=frequency+1,label=label))
sp_freq_plot_WA <- sp_freq_plot_WA + ggtitle("Washington, USA - n=502 lakes")
sp_freq_plot_WA <- sp_freq_plot_WA + theme(legend.position="none")
sp_freq_plot_WA

ggsave("sp_freq_plot_WA.jpg",sp_freq_plot_WA,height=8,width=11)


###################################
# Comparing three regions         #
# CT, USA (n=174 water bodies)    #
# N. Ireland (n=574 water bodies) #
# Norway (n=64 water bodies)      #
# Similar spatial extent          #
# ~15,000 sq. km                  #
###################################
data_compare_regions <- read.csv("compare_regions.csv")
head(data_compare_regions)

plot_France <- ggplot(subset(data_compare_regions, data_compare_regions=="France"),aes(x=Species,y=Value,fill=Variable))
plot_France <- plot_France + geom_bar(stat="identity", position=position_dodge())
plot_France <- plot_France + theme_bw(base_size=16)
plot_France <- plot_France + ylim(0,1)
plot_France <- plot_France + ggtitle("France")
plot_France

plot_CT <- ggplot(subset(data_compare_regions, data_compare_regions=="Connecticut, USA"),aes(x=Species,y=Value,fill=Variable))
plot_CT <- plot_CT + geom_bar(stat="identity", position=position_dodge())
plot_CT <- plot_CT + theme_bw(base_size=16)
plot_CT <- plot_CT + ylim(0,1)
plot_CT <- plot_CT + ggtitle("Connecticut, USA")
plot_CT

plot_Norway <- ggplot(subset(data_compare_regions, data_compare_regions=="Norway"),aes(x=Species,y=Value,fill=Variable))
plot_Norway <- plot_Norway + geom_bar(stat="identity", position=position_dodge())
plot_Norway <- plot_Norway + theme_bw(base_size=16)
plot_Norway <- plot_Norway + ylim(0,1)
plot_Norway <- plot_Norway + ggtitle("Norway")
plot_Norway

plot_Denmark <- ggplot(subset(data_compare_regions, data_compare_regions=="Denmark"),aes(x=Species,y=Value,fill=Variable))
plot_Denmark <- plot_Denmark + geom_bar(stat="identity", position=position_dodge())
plot_Denmark <- plot_Denmark + theme_bw(base_size=16)
plot_Denmark <- plot_Denmark + ylim(0,1)
plot_Denmark <- plot_Denmark + ggtitle("Denmark")
plot_Denmark

plot_Great_Britain <- ggplot(subset(data_compare_regions, data_compare_regions=="Great Britain"),aes(x=Species,y=Value,fill=Variable))
plot_Great_Britain <- plot_Great_Britain + geom_bar(stat="identity", position=position_dodge())
plot_Great_Britain <- plot_Great_Britain + theme_bw(base_size=16)
plot_Great_Britain <- plot_Great_Britain + ylim(0,1)
plot_Great_Britain <- plot_Great_Britain + ggtitle("Great Britain")
plot_Great_Britain

plot_Great_Britain_1989 <- ggplot(subset(data_compare_regions, data_compare_regions=="Great Britain - 1989"),aes(x=Species,y=Value,fill=Variable))
plot_Great_Britain_1989 <- plot_Great_Britain_1989 + geom_bar(stat="identity", position=position_dodge())
plot_Great_Britain_1989 <- plot_Great_Britain_1989 + theme_bw(base_size=16)
plot_Great_Britain_1989 <- plot_Great_Britain_1989 + ylim(0,1)
plot_Great_Britain_1989 <- plot_Great_Britain_1989 + ggtitle("Great Britain - 1989")
plot_Great_Britain_1989

plot_Great_Britain_2004 <- ggplot(subset(data_compare_regions, data_compare_regions=="Great Britain - 2004"),aes(x=Species,y=Value,fill=Variable))
plot_Great_Britain_2004 <- plot_Great_Britain_2004 + geom_bar(stat="identity", position=position_dodge())
plot_Great_Britain_2004 <- plot_Great_Britain_2004 + theme_bw(base_size=16)
plot_Great_Britain_2004 <- plot_Great_Britain_2004 + ylim(0,1)
plot_Great_Britain_2004 <- plot_Great_Britain_2004 + ggtitle("Great Britain - 2004")
plot_Great_Britain_2004

plot_Ireland <- ggplot(subset(data_compare_regions, data_compare_regions=="N. Ireland"),aes(x=Species,y=Value,fill=Variable))
plot_Ireland <- plot_Ireland + geom_bar(stat="identity", position=position_dodge())
plot_Ireland <- plot_Ireland + theme_bw(base_size=16)
plot_Ireland <- plot_Ireland + ylim(0,1)
plot_Ireland <- plot_Ireland + ggtitle("N. Ireland")
plot_Ireland

plot_Netherlands <- ggplot(subset(data_compare_regions, data_compare_regions=="Netherlands"),aes(x=Species,y=Value,fill=Variable))
plot_Netherlands <- plot_Netherlands + geom_bar(stat="identity", position=position_dodge())
plot_Netherlands <- plot_Netherlands + theme_bw(base_size=16)
plot_Netherlands <- plot_Netherlands + ylim(0,1)
plot_Netherlands <- plot_Netherlands + ggtitle("Netherlands")
plot_Netherlands

plot_Estonia <- ggplot(subset(data_compare_regions, data_compare_regions=="Saaremaa Island (Estonia)"),aes(x=Species,y=Value,fill=Variable))
plot_Estonia <- plot_Estonia + geom_bar(stat="identity", position=position_dodge())
plot_Estonia <- plot_Estonia + theme_bw(base_size=16)
plot_Estonia <- plot_Estonia + ylim(0,1)
plot_Estonia <- plot_Estonia + ggtitle("Saaremaa Island (Estonia)")
plot_Estonia







plot_compare_regions <- ggplot(data_compare_regions,aes(x=Species,y=Value,fill=Variable))
plot_compare_regions <- plot_compare_regions + geom_bar(stat="identity", position=position_dodge())
plot_compare_regions <- plot_compare_regions + facet_grid(Location ~ .)
plot_compare_regions <- plot_compare_regions + theme_bw(base_size=16)
plot_compare_regions


ggsave("plot_compare_regions.jpg",plot_compare_regions,height=8,width=8)
