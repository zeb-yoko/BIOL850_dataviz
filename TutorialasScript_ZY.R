# Biology 850 Workshop on using ggplot2
# Jon Sweetman, Sept19,2018 jon.sweetman@ndsu.edu

# Libraries - if you haven't installed them before, run the code install.packages("package_name")
library(tidyverse) #includes ggplot2 and more..
library(gridExtra) #package for arranging multiple plots

# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
file.path("C:/Users/zippy/OneDrive/NDSU/BIOL850_dataviz")
LPI <- read.csv("LPIdata_CC.csv")  # find where you saved LPIdata_CC.csv

# str displays the structure of our data
str(LPI)

#Clean up & Manipulate data ready for graphing:

# Reshape data into long form
# By adding 9:53, we select columns from 9 to 53, the ones for the different years of monitoring
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)

#parse_number drops any non-numeric characters before or after the first number 
LPI2$year <- parse_number(LPI2$year)

# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is also a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

#unique returns a vector, data frame or an array with duplicate entries removed
unique(LPI2$Common.Name)

# filter is part of the dplyr package we looked at in the data manipulation workshop
bison <- filter(LPI2, Common.Name == "American bison / Wood bison / Plains bison")
head(bison)
Purp <- filter(LPI2, Common.Name =="Purple swamphen")
nrow(Purp)
# There are a lot of NAs in this dataframe, so we will get rid of the empty rows using na.omit()
bison <- na.omit(bison)

# Creating a histogram using ggplot2
bison_hist <- ggplot(bison, aes(x=abundance))  +
  geom_histogram() 
# putting your entire ggplot code in () creates the graph and shows it in the plot viewer.
# Without brackets, you have to call the object, so that it gets displayed, e.g.
bison_hist
# Here is how it looks with the brackets
(bison_hist <- ggplot(bison, aes(x=abundance))  +
    geom_histogram())

# trying some options to change the look of the plot
(bison_hist <- ggplot(bison, aes(x=abundance))  +
    # Changing the binwidth and colours
    geom_histogram(binwidth=250, colour="#8B5A00", fill="#CD8500") +    
    # Adding a line for mean abundance
    geom_vline(aes(xintercept=mean(abundance)),
               # Changing the look of the line
               colour="red", linetype="dashed", size=1) +                
    # Changing the theme to get rid of the grey background
    
    theme_bw() +                                                      
    # Changing the text of the y axis label
    ylab("Count\n") +                                                   
    # \n adds a blank line
    xlab("\nBison abundance")  +                             
    # Changing font size of axis labels
    theme(axis.text.x=element_text(size=12),                                   
          axis.text.y=element_text(size=12),
          # Changing font size of axis titles
          axis.title.x=element_text(size=14, face="plain"),  
          # face="plain" changes font type, could also be italic, etc                
          axis.title.y=element_text(size=14, face="plain"),   
          # Removing the grey grid lines
          panel.grid.major.x=element_blank(),                                   
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          # Putting a 1 cm margin around the plot
          plot.margin = unit(c(1,1,1,1), units = , "cm")))   

# We can see from the histogram that the data are very skewed – 
# a typical distribution of count abundance data

#Creating Scatterplots

# note that the code is similar to the histogram, except a different geom
(bison_scatter <- ggplot(bison, aes (x=year, y=abundance, colour=Country.list)) +
    geom_point())

# improving the look of our scatterplot
(bison_scatter <- ggplot(bison, aes (x=year, y=abundance, colour=Country.list)) +
    geom_point(size=2) +                             # Changing point size
    geom_smooth(method=lm, aes(fill=Country.list)) + # Adding a linear model fit and colour-coding by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +    # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B")) +   # Adding custom colours
    ylab("American Bison abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),       # making the years at a bit of an angle
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")) +                    # Adding a 1cm margin around the plot
    theme(legend.text = element_text(size=12, face="italic"),                  # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position=c(0.9, 0.9)))                  # Setting the position for the legend - 0 is left/bottom, 1 is top/right

# Creating a Boxplot using ggplot2
(bison_boxplot <- ggplot (bison, aes(Country.list, abundance)) + geom_boxplot())

# Beautifying

(bison_boxplot <- ggplot (bison, aes(Country.list, abundance)) + geom_boxplot(aes(fill=Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B")) +             # Adding custom colours
    ylab("American Bison abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                           # Removing the background grid lines                
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position="none"))                                      # Removing the legend - not needed with only two factors

#Barplot to examine species richness
# Calculating species richness using pipes ``%>%` from the `dplyr` package
richness <- LPI2 %>% filter (Country.list == c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>%
  mutate (., richness=(length(unique(Common.Name))))

(richness_barplot <- ggplot(richness, aes(x=Country.list, y=richness)) +
    geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),  # x axis labels angled, so that text doesn't overlap
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

#Arranging plots in a panel using grid.arrange()
grid.arrange(bison_hist, bison_scatter, bison_boxplot, ncol=1)
??grid.arrange

# This doesn't look right - the graphs are too stretched, the legend and text are all messed up, the white margins are too big
# Fixing the problems - adding ylab() again overrides the previous settings

panel <- grid.arrange(bison_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
                        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
                      bison_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
                        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
                      bison_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
                        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
                        theme(legend.text = element_text(size=12, face="italic"),               
                              legend.title = element_blank(),                                   
                              legend.position=c(0.85, 0.85)), # changing the legend position so that it fits within the panel
                      ncol=1) # ncol determines how many columns you have

#make sure you manually create the subdirectory folder first before saving!
ggsave(panel, file="figures/bison_panel2.png", width=5, height=12) # the file is saved in your working directory, find it with getwd()
ggsave(panel, file="figures/bison_panel2.pdf", width=5, height=12) # save as a PDF 


