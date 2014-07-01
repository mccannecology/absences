# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

cor(dataENV)
cor(log(dataENV))

pairs(dataENV)
pairs(dataENV,log="xy")

##################################
# Correlation scattefplot matrix #
# Untransformed dataENV          #
# Histogram along diagonal       #
# correlation above diagonal     #   
##################################
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
pairs(dataENV, 
      cex = 1.5, 
      diag.panel = panel.hist, cex.labels = 1, font.labels = 1,
      upper.panel = panel.cor)

# save the file 
jpeg("corrl_dataENV.jpg",height=8,width=11,units="in",res=600)
pairs(dataENV, 
      cex = 1.5, 
      diag.panel = panel.hist, cex.labels = 1, font.labels = 1,
      upper.panel = panel.cor)
dev.off()


##################################
# Correlation scattefplot matrix #
# log-transformed dataENV        #
##################################
pairs(dataENV, log="xy")

# save the file 
jpeg("corrl_log_dataENV.jpg",height=8,width=11,units="in",res=600)
pairs(dataENV, log="xy")
dev.off()
