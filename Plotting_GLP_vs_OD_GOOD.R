
###############################################################################
## Code written by Sergio Gonzalez Ruiz
## Used for: GFP vs. OD plotting, applying first derivative to these plots,
## applying Savitzky-Golay Smoothing (for generated plots), finding inflection
## points in both types of plots and calculating ∇ OD values
## Last update: 17/06/2022
###############################################################################


###############################################################################
## 0 - Loading all required packages and libraries
###############################################################################

## Clear the Console and the Environment
rm(list=ls()) 

## Installing and importing needed packages:
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("pracma")
#install.packages("RootsExtremaInflections")

library("readxl")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("gridExtra")
library("cowplot")
library("pracma")
library("RootsExtremaInflections")


###############################################################################
## 1 - Loading the source file(s)
## The user must choose his/her own directory in which he/she has the file of interest
###############################################################################


#my_csv <- read.csv(file = "C:\\Users\\USUARIO\\OneDrive\\Escritorio\\Master_Biologia_Computacional\\Practicas_TFM\\Fluorescence\\Fluorescence_combinations_Sergio_16_03_22.xlsx")

# LAPTOP:
#my_data <- read_excel(path = "C:\\Users\\USUARIO\\OneDrive\\Escritorio\\Master_Biologia_Computacional\\Practicas_TFM\\Fluorescence\\Fluorescence_combinations_Sergio_16_03_22.xlsx", sheet = "Hoja1")
#my_data_raw <- read_excel(path = "C:\\Users\\USUARIO\\OneDrive\\Escritorio\\Master_Biologia_Computacional\\Practicas_TFM\\Fluorescence\\Fluorescence_combinations_Sergio_16_03_22_RAW_DATA.xlsx", sheet = "Hoja1")
#my_data_1 <- read_excel(path = "C:\\Users\\USUARIO\\OneDrive\\Escritorio\\Master_Biologia_Computacional\\Practicas_TFM\\Fluorescence\\Fluorescence_combinations_Sergio_23_03_22_INDUSTRIAL.xlsx", sheet = "Hoja1")
#my_data_2 <- read_excel(path = "C:\\Users\\USUARIO\\OneDrive\\Escritorio\\Master_Biologia_Computacional\\Practicas_TFM\\Fluorescence\\Fluorescence_combinations_Sergio_30_03_22_INDUSTRIAL.xlsx", sheet = "Hoja1")

# PC:
my_data <- read_excel(path = "C:\\Users\\USUARIO\\Desktop\\Master_Biologia_Computacional\\Practicas_TFM_portatil\\Fluorescence\\Fluorescence_combinations_Sergio_16_03_22.xlsx", sheet = "Hoja1")
my_data_raw <- read_excel(path = "C:\\Users\\USUARIO\\Desktop\\Master_Biologia_Computacional\\Practicas_TFM_portatil\\Fluorescence\\Fluorescence_combinations_Sergio_16_03_22_RAW_DATA.xlsx", sheet = "Hoja1")
my_data_1 <- read_excel(path = "C:\\Users\\USUARIO\\Desktop\\Master_Biologia_Computacional\\Practicas_TFM_portatil\\Fluorescence\\Fluorescence_combinations_Sergio_23_03_22_INDUSTRIAL.xlsx", sheet = "Hoja1")
my_data_2 <- read_excel(path = "C:\\Users\\USUARIO\\Desktop\\Master_Biologia_Computacional\\Practicas_TFM_portatil\\Fluorescence\\Fluorescence_combinations_Sergio_30_03_22_INDUSTRIAL.xlsx", sheet = "Hoja1")


###############################################################################
## 2 - Data Preparation and Data Visualization
## GFP vs. OD Plots
## Derivative Plots (Diff Plots)
###############################################################################

######### FIRST EXCEL + GFP vs. OD plots ######### 

## Splitting data --> GFP and OD measures

GFP <- my_data[,1:146]
OD <- my_data[,c(1,147:291)]

## Converting the first column into ROW NAMES:

GFP <- as.data.frame(GFP) # Unnecessary??
row.names(GFP) <- GFP$Content
GFP_1 <- GFP[,(2:146)]

OD <- as.data.frame(OD) # Unnecessary??
row.names(OD) <- OD$Content
OD_1 <- OD[,(2:146)]

## Transposing both dataframes:https://www.marsja.se/how-to-transpose-a-dataframe-or-matrix-in-r-with-the-t-function/#:~:text=To%20interchange%20rows%20with%20columns,exchanging%20the%20rows%20and%20columns.

GFP_2 <- t(GFP_1)
GFP_2 <- as.data.frame(GFP_2)

OD_2 <- t(OD_1)
OD_2 <- as.data.frame(OD_2)

#df <- rbind(GFP_2,OD_2)
df <- cbind(GFP_2,OD_2)
df <- setNames(df, c("MCR5101 + OFL5101 (C-)_GFP","MCR5101 + HKE3402 (C+)_GFP",
                     "MCR5101 + OFL3201_GFP", "OFL5101 + HKE3402_GFP", 
                     "OFL5101 + OFL3201_GFP",  "OFL5101 + FLO11-_GFP", 
                     "MCR5101 + FLO11-_GFP", "MCR5105 + HKE3402_GFP",
                     "MCR5101 + MCS5205_GFP", "MCR5114 + HKE3402_GFP",
                     "MCR5101 + OFL5101 (C-)_OD","MCR5101 + HKE3402 (C+)_OD",
                     "MCR5101 + OFL3201_OD", "OFL5101 + HKE3402_OD", 
                     "OFL5101 + OFL3201_OD",  "OFL5101 + FLO11-_OD", 
                     "MCR5101 + FLO11-_OD", "MCR5105 + HKE3402_OD",
                     "MCR5101 + MCS5205_OD", "MCR5114 + HKE3402_OD"))

## Plotting the final results: https://hbctraining.github.io/Intro-to-R/lessons/07_ggplot2.html
## We will also plot the fitting line and the FIRST DERIVATIVE --> https://stackoverflow.com/questions/6356665/how-do-i-plot-the-first-derivative-of-the-smoothing-function

### GFP vs. OD plots:

plot1 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`))

plot2 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`))

plot3 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`))

plot4 <- ggplot(df) + 
  geom_point(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`))

plot5 <- ggplot(df) + 
  geom_point(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`))

plot6 <- ggplot(df) + 
  geom_point(aes(y = `OFL5101 + FLO11-_GFP`, x = `OFL5101 + FLO11-_OD`))

plot7 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + FLO11-_GFP`, x = `MCR5101 + FLO11-_OD`))

plot8 <- ggplot(df) + 
  geom_point(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`))

plot9 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`))

plot10 <- ggplot(df) + 
  geom_point(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`))

final_plot <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_point(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`, colour = "MCR5101 + OFL3201")) + 
  geom_point(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`, colour = "OFL5101 + HKE3402")) + 
  geom_point(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`, colour = "OFL5101 + OFL3201")) + 
  geom_point(aes(y = `OFL5101 + FLO11-_GFP`, x = `OFL5101 + FLO11-_OD`, colour = "OFL5101 + FLO11-")) + 
  geom_point(aes(y = `MCR5101 + FLO11-_GFP`, x = `MCR5101 + FLO11-_OD`, colour = "MCR5101 + FLO11-")) + 
  geom_point(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`, colour = "MCR5105 + HKE3402")) + 
  geom_point(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`, colour = "MCR5101 + MCS5205")) + 
  geom_point(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`, colour = "MCR5114 + HKE3402")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(1.0, 1.5))

###### DERIVATIVE FUNCTION #####

### GOOD FIT
plot1 <- ggplot(data = df, aes(x = `MCR5101 + OFL5101 (C-)_OD`, y = `MCR5101 + OFL5101 (C-)_GFP`)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,7)) +
  ggtitle("Plot 1") +
  theme(plot.title = element_text(hjust = 0.5))
plot1
cor(df$`MCR5101 + OFL5101 (C-)_GFP`,df$`MCR5101 + OFL5101 (C-)_OD`)

plot_pos <- ggplot(data = df, aes(x = `MCR5101 + HKE3402 (C+)_OD`, y = `MCR5101 + HKE3402 (C+)_GFP`)) + 
  #geom_point() +
  geom_line() +
  #stat_smooth(method = "lm", formula = y ~ poly(x,10)) +
  ggtitle("MCR5101 + HKE3402 (C+)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))
plot_pos


### LETS TAKE ANOTHER APPROACH --> https://stackoverflow.com/questions/29779342/plotting-a-function-and-a-derivative-function/31260832

# Retrieving the polynomial function of 32nd degree --> fit
fit <- lm(df$`MCR5101 + HKE3402 (C+)_GFP` ~ poly(df$`MCR5101 + HKE3402 (C+)_OD`,32,raw=TRUE), df)

# Calculate the function values and store them in df$fitted_values
df$fitted_values <- fitted(fit)

# Plot original values (red), along with the fitted values (blue)
ggplot(df, aes(x = `MCR5101 + HKE3402 (C+)_OD`)) + 
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`), colour="red") + 
  geom_line(aes(y = fitted_values), colour="blue") +
  coord_cartesian(xlim = c(1.0, 1.45))

# Helper function to calculate the new coefficients:
deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

# Getting SLOPE values:
df$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)

# Previous plot, along with the DERIVATIVE function (green)
ggplot(df, aes(x = `MCR5101 + HKE3402 (C+)_OD`)) + 
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`), colour="red") + 
  geom_line(aes(y = fitted_values), colour="blue") + 
  geom_line(aes(y = slope), colour="green")+
  coord_cartesian(xlim = c(1.0, 1.45))

## 2

# Retrieving the polynomial function of 32nd degree --> fit
fit <- lm(df$`MCR5101 + OFL5101 (C-)_GFP` ~ poly(df$`MCR5101 + OFL5101 (C-)_OD`,32,raw=TRUE), df)

# Calculate the function values and store them in df$fitted_values
df$fitted_values <- fitted(fit)

# Plot original values (red), along with the fitted values (blue)
ggplot(df, aes(x = `MCR5101 + OFL5101 (C-)_OD`)) + 
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`), colour="red") + 
  geom_line(aes(y = fitted_values), colour="blue")

# Helper function to calculate the new coefficients:
deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

# Getting SLOPE values:
df$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)

# Previous plot, along with the DERIVATIVE function (green)
ggplot(df, aes(x = `MCR5101 + OFL5101 (C-)_OD`)) + 
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`), colour="red") + 
  geom_line(aes(y = fitted_values), colour="blue") + 
  geom_line(aes(y = slope), colour="green")


###### Diff Plots #####

### Using "diff" function:

diffs <- diff(df$`MCR5101 + OFL5101 (C-)_GFP`) # Calculating differences between consecutive GFP values
diffs <- append(diffs,0) # As the differences are between a PAIR OF NUMBER, we need to add another one to make this array the same size as the others

## Plotting differences between consecutive GFP values (Y axis) against OD measures (X axis):

# Negative control (C-):
diffplot_Cneg <- ggplot(df) + 
  geom_point(aes(y = diffs, x = `MCR5101 + OFL5101 (C-)_OD`)) + 
  geom_line(aes(y = diffs, x = `MCR5101 + OFL5101 (C-)_OD`)) +
  labs(y = "MCR5101 + OFL5101 (C-)_GFP differences", title = "GFP differences vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.4))

# Positive control (C+):

diffs <- diff(df$`MCR5101 + HKE3402 (C+)_GFP`)
diffs <- append(diffs,0)

diffplot_Cpos <- ggplot(df) + 
  #geom_point(aes(y = diffs, x = `MCR5101 + HKE3402 (C+)_OD`)) + 
  geom_line(aes(y = diffs, x = `MCR5101 + HKE3402 (C+)_OD`)) +
  labs(y = "MCR5101 + HKE3402 (C+)_GFP differences", title = "GFP differences vs. OD Measurements (C+)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.4))

######### EXCEL WITH RAW DATA: FLUORESCENCE VS OD (PLOTS WITH CONFIDENCE REGIONS) ###############

## Splitting data --> GFP and OD measures

GFP <- my_data_raw[,1:146]
OD <- my_data_raw[,c(1,147:291)]

## Obtaining average and standard deviations:

mean_GFP <- GFP%>%    # Specify data frame
  group_by(Content)%>%       # Specify group indicator
  summarise_at(vars(colnames(GFP)[2:length(colnames(GFP))]),  # Specify column(s)
               list(name = mean))     # Specify function

sd_GFP <- GFP%>%      # Specify data frame
  group_by(Content)%>%      # Specify group indicator
  summarise_at(vars(colnames(GFP)[2:length(colnames(GFP))]),  # Specify column(s)
               list(name = sd))     # Specify function

mean_OD <- OD%>%    # Specify data frame
  group_by(Content)%>%       # Specify group indicator
  summarise_at(vars(colnames(OD)[2:length(colnames(GFP))]),  # Specify column(s)
               list(name = mean))     # Specify function

sd_OD <- OD%>%    # Specify data frame
  group_by(Content)%>%       # Specify group indicator
  summarise_at(vars(colnames(OD)[2:length(colnames(GFP))]),  # Specify column(s)
               list(name = sd))     # Specify function

## Converting the first column into ROW NAMES:

mean_GFP <- as.data.frame(mean_GFP) # Unnecessary??
row.names(mean_GFP) <- mean_GFP$Content
mean_GFP <- mean_GFP[,(2:146)]

mean_OD <- as.data.frame(mean_OD) # Unnecessary??
row.names(mean_OD) <- mean_OD$Content
mean_OD <- mean_OD[,(2:146)]

sd_GFP <- as.data.frame(sd_GFP) # Unnecessary??
row.names(sd_GFP) <- sd_GFP$Content
sd_GFP <- sd_GFP[,(2:146)]

sd_OD <- as.data.frame(sd_OD) # Unnecessary??
row.names(sd_OD) <- sd_OD$Content
sd_OD <- sd_OD[,(2:146)]

## Arranging rows as in the original data frame:

correct_order <- c("MCR5101 + OFL5101 (C-)","MCR5101 + HKE3402 (C+)",
                   "MCR5101 + OFL3201", "OFL5101 + HKE3402", 
                   "OFL5101 + OFL3201",  "OFL5101 + BY_FLO11-", 
                   "MCR5101 + BY_FLO11-", "MCR5105 + HKE3402",
                   "MCR5101 + MCS5205", "MCR5114 + HKE3402")

mean_GFP <- mean_GFP %>%
  arrange(factor(rownames(mean_GFP), levels = correct_order))

mean_OD <- mean_OD %>%
  arrange(factor(rownames(mean_OD), levels = correct_order))

sd_GFP <- sd_GFP %>%
  arrange(factor(rownames(sd_GFP), levels = correct_order))

sd_OD <- sd_OD %>%
  arrange(factor(rownames(sd_OD), levels = correct_order))


## Transposing both dataframes:https://www.marsja.se/how-to-transpose-a-dataframe-or-matrix-in-r-with-the-t-function/#:~:text=To%20interchange%20rows%20with%20columns,exchanging%20the%20rows%20and%20columns.

mean_GFP <- t(mean_GFP)
mean_GFP <- as.data.frame(mean_GFP)

mean_OD <- t(mean_OD)
mean_OD <- as.data.frame(mean_OD)

sd_GFP <- t(sd_GFP)
sd_GFP <- as.data.frame(sd_GFP)

sd_OD <- t(sd_OD)
sd_OD <- as.data.frame(sd_OD)


df <- cbind(mean_GFP,mean_OD)
df <- setNames(df, c("MCR5101 + OFL5101 (C-)_GFP","MCR5101 + HKE3402 (C+)_GFP",
                     "MCR5101 + OFL3201_GFP", "OFL5101 + HKE3402_GFP", 
                     "OFL5101 + OFL3201_GFP",  "OFL5101 + BY_FLO11-_GFP", 
                     "MCR5101 + BY_FLO11-_GFP", "MCR5105 + HKE3402_GFP",
                     "MCR5101 + MCS5205_GFP", "MCR5114 + HKE3402_GFP",
                     "MCR5101 + OFL5101 (C-)_OD","MCR5101 + HKE3402 (C+)_OD",
                     "MCR5101 + OFL3201_OD", "OFL5101 + HKE3402_OD", 
                     "OFL5101 + OFL3201_OD",  "OFL5101 + BY_FLO11-_OD", 
                     "MCR5101 + BY_FLO11-_OD", "MCR5105 + HKE3402_OD",
                     "MCR5101 + MCS5205_OD", "MCR5114 + HKE3402_OD"))


## Obtaining high and low values of the confidence bands:

GFP_low_df <- mean_GFP - sd_GFP
GFP_low_df <- setNames(GFP_low_df, c("MCR5101 + OFL5101 (C-)_GFP_low","MCR5101 + HKE3402 (C+)_GFP_low",
                                     "MCR5101 + OFL3201_GFP_low", "OFL5101 + HKE3402_GFP_low",
                                     "OFL5101 + OFL3201_GFP_low",  "OFL5101 + BY_FLO11-_GFP_low",
                                     "MCR5101 + BY_FLO11-_GFP_low", "MCR5105 + HKE3402_GFP_low",
                                     "MCR5101 + MCS5205_GFP_low", "MCR5114 + HKE3402_GFP_low"))

GFP_high_df <- mean_GFP + sd_GFP
GFP_high_df <- setNames(GFP_high_df, c("MCR5101 + OFL5101 (C-)_GFP_high","MCR5101 + HKE3402 (C+)_GFP_high",
                                       "MCR5101 + OFL3201_GFP_high", "OFL5101 + HKE3402_GFP_high",
                                       "OFL5101 + OFL3201_GFP_high",  "OFL5101 + BY_FLO11-_GFP_high",
                                       "MCR5101 + BY_FLO11-_GFP_high", "MCR5105 + HKE3402_GFP_high",
                                       "MCR5101 + MCS5205_GFP_high", "MCR5114 + HKE3402_GFP_high"))


OD_low_df <- mean_OD - sd_OD
OD_low_df <- setNames(OD_low_df, c("MCR5101 + OFL5101 (C-)_OD_low","MCR5101 + HKE3402 (C+)_OD_low",
                                   "MCR5101 + OFL3201_OD_low", "OFL5101 + HKE3402_OD_low",
                                   "OFL5101 + OFL3201_OD_low",  "OFL5101 + BY_FLO11-_OD_low",
                                   "MCR5101 + BY_FLO11-_OD_low", "MCR5105 + HKE3402_OD_low",
                                   "MCR5101 + MCS5205_OD_low", "MCR5114 + HKE3402_OD_low"))

OD_high_df <- mean_OD + sd_OD
OD_high_df <- setNames(OD_high_df, c("MCR5101 + OFL5101 (C-)_OD_high","MCR5101 + HKE3402 (C+)_OD_high",
                                     "MCR5101 + OFL3201_OD_high", "OFL5101 + HKE3402_OD_high",
                                     "OFL5101 + OFL3201_OD_high",  "OFL5101 + BY_FLO11-_OD_high",
                                     "MCR5101 + BY_FLO11-_OD_high", "MCR5105 + HKE3402_OD_high",
                                     "MCR5101 + MCS5205_OD_high", "MCR5114 + HKE3402_OD_high"))



## Adding these results to the dataframe:

df <- cbind(df,GFP_low_df,GFP_high_df,OD_low_df,OD_high_df)

### GFP vs. OD plot:

final_plot <- ggplot(df) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,colour = "MCR5101 + OFL5101 (C-)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,
                  ymin = `MCR5101 + OFL5101 (C-)_GFP_low`, ymax = `MCR5101 + OFL5101 (C-)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`,
                  ymin = `MCR5101 + HKE3402 (C+)_GFP_low`, ymax = `MCR5101 + HKE3402 (C+)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`, colour = "MCR5101 + OFL3201"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`,
                  ymin = `MCR5101 + OFL3201_GFP_low`, ymax = `MCR5101 + OFL3201_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`, colour = "OFL5101 + HKE3402"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`,
                  ymin = `OFL5101 + HKE3402_GFP_low`, ymax = `OFL5101 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`, colour = "OFL5101 + OFL3201"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`,
                  ymin = `OFL5101 + OFL3201_GFP_low`, ymax = `OFL5101 + OFL3201_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + BY_FLO11-_GFP`, x = `OFL5101 + BY_FLO11-_OD`, colour = "OFL5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + BY_FLO11-_GFP`, x = `OFL5101 + BY_FLO11-_OD`,
                  ymin = `OFL5101 + BY_FLO11-_GFP_low`, ymax = `OFL5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + BY_FLO11-_GFP`, x = `MCR5101 + BY_FLO11-_OD`, colour = "MCR5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + BY_FLO11-_GFP`, x = `MCR5101 + BY_FLO11-_OD`,
                  ymin = `MCR5101 + BY_FLO11-_GFP_low`, ymax = `MCR5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`, colour = "MCR5105 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`,
                  ymin = `MCR5105 + HKE3402_GFP_low`, ymax = `MCR5105 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`, colour = "MCR5101 + MCS5205"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`,
                  ymin = `MCR5101 + MCS5205_GFP_low`, ymax = `MCR5101 + MCS5205_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`, colour = "MCR5114 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`,
                  ymin = `MCR5114 + HKE3402_GFP_low`, ymax = `MCR5114 + HKE3402_GFP_high`),alpha = 0.4) +
  labs(x = "Optical Density (OD600 nm) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) +
  scale_x_continuous(breaks = seq(0.2, 1.45, by = 0.2))+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  coord_cartesian(xlim = c(0.2, 1.45))

final_plot

final_plot_zoom <- final_plot +
  scale_x_continuous(breaks = seq(1.0, 1.45, by = 0.1))+
  coord_cartesian(xlim = c(1.0, 1.45))

final_plot_zoom

## Plotting these graphs together:

final_plots_merged <- plot_grid(final_plot,final_plot_zoom,
                                labels = c("A","B"),ncol = 2, nrow = 1)

final_plots_merged


## Subdividing this plot for better visualization

OFL_plot <- ggplot(df) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,colour = "4MCR5101 + OFL5101 (C-)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,
                  ymin = `MCR5101 + OFL5101 (C-)_GFP_low`, ymax = `MCR5101 + OFL5101 (C-)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "1MCR5101 + HKE3402 (C+)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`,
                  ymin = `MCR5101 + HKE3402 (C+)_GFP_low`, ymax = `MCR5101 + HKE3402 (C+)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`, colour = "2MCR5101 + OFL3201"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`,
                  ymin = `MCR5101 + OFL3201_GFP_low`, ymax = `MCR5101 + OFL3201_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`, colour = "3OFL5101 + HKE3402"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`,
                  ymin = `OFL5101 + HKE3402_GFP_low`, ymax = `OFL5101 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`, colour = "5OFL5101 + OFL3201"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`,
                  ymin = `OFL5101 + OFL3201_GFP_low`, ymax = `OFL5101 + OFL3201_GFP_high`),alpha = 0.4) +
  labs(x = "Optical Density (OD600 nm) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements (Controls + FLO11 Overexpressing strains)") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) +
  scale_x_continuous(breaks = seq(0.2, 1.45, by = 0.1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100),limits = c(0,550))+
  coord_cartesian(xlim = c(1.0, 1.45))

OFL_plot


FLO11minus_plot <- ggplot(df) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,colour = "3MCR5101 + OFL5101 (C-)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,
                  ymin = `MCR5101 + OFL5101 (C-)_GFP_low`, ymax = `MCR5101 + OFL5101 (C-)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "1MCR5101 + HKE3402 (C+)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`,
                  ymin = `MCR5101 + HKE3402 (C+)_GFP_low`, ymax = `MCR5101 + HKE3402 (C+)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + BY_FLO11-_GFP`, x = `OFL5101 + BY_FLO11-_OD`, colour = "4OFL5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + BY_FLO11-_GFP`, x = `OFL5101 + BY_FLO11-_OD`,
                  ymin = `OFL5101 + BY_FLO11-_GFP_low`, ymax = `OFL5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + BY_FLO11-_GFP`, x = `MCR5101 + BY_FLO11-_OD`, colour = "2MCR5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + BY_FLO11-_GFP`, x = `MCR5101 + BY_FLO11-_OD`,
                  ymin = `MCR5101 + BY_FLO11-_GFP_low`, ymax = `MCR5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  labs(x = "Optical Density (OD600 nm) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements (Controls + FLO11 Repressing strains)") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) +
  scale_x_continuous(breaks = seq(0.2, 1.45, by = 0.1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100),limits = c(0,550))+
  coord_cartesian(xlim = c(1.0, 1.45))

FLO11minus_plot


Synthethic_strains_plot <- ggplot(df) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,colour = "4MCR5101 + OFL5101 (C-)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`,
                  ymin = `MCR5101 + OFL5101 (C-)_GFP_low`, ymax = `MCR5101 + OFL5101 (C-)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "1MCR5101 + HKE3402 (C+)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`,
                  ymin = `MCR5101 + HKE3402 (C+)_GFP_low`, ymax = `MCR5101 + HKE3402 (C+)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`, colour = "3MCR5105 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`,
                  ymin = `MCR5105 + HKE3402_GFP_low`, ymax = `MCR5105 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`, colour = "2MCR5101 + MCS5205"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`,
                  ymin = `MCR5101 + MCS5205_GFP_low`, ymax = `MCR5101 + MCS5205_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`, colour = "5MCR5114 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`,
                  ymin = `MCR5114 + HKE3402_GFP_low`, ymax = `MCR5114 + HKE3402_GFP_high`),alpha = 0.4) +
  labs(x = "Optical Density (OD600 nm) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements (Controls + Synthetic protein strains)") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) +
  scale_x_continuous(breaks = seq(0.2, 1.45, by = 0.1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100),limits = c(0,550))+
  coord_cartesian(xlim = c(1.0, 1.45))

Synthethic_strains_plot


subdivided_plots_merged <- plot_grid(OFL_plot,FLO11minus_plot,Synthethic_strains_plot,
                                     labels = c("A","B","C"),ncol = 2, nrow = 2)

subdivided_plots_merged

## Plotting Fluorescence and OD over time (10 minutes per lecture)

time <- seq(0,1440, by = 10) # Time in seconds
time_hours <- time/60 # Time in hours

plot_GFP_time <- ggplot(df,aes(x=time_hours)) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`,colour = "MCR5101 + OFL5101 (C-)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, ymin = `MCR5101 + OFL5101 (C-)_GFP_low`, ymax = `MCR5101 + OFL5101 (C-)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, colour = "MCR5101 + HKE3402 (C+)"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, ymin = `MCR5101 + HKE3402 (C+)_GFP_low`, ymax = `MCR5101 + HKE3402 (C+)_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + OFL3201_GFP`, colour = "MCR5101 + OFL3201"),size = 1) +
  geom_ribbon(aes(y = `MCR5101 + OFL3201_GFP`, ymin = `MCR5101 + OFL3201_GFP_low`, ymax = `MCR5101 + OFL3201_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + HKE3402_GFP`, colour = "OFL5101 + HKE3402"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + HKE3402_GFP`, ymin = `OFL5101 + HKE3402_GFP_low`, ymax = `OFL5101 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + OFL3201_GFP`, colour = "OFL5101 + OFL3201"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + OFL3201_GFP`, ymin = `OFL5101 + OFL3201_GFP_low`, ymax = `OFL5101 + OFL3201_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + BY_FLO11-_GFP`, colour = "OFL5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `OFL5101 + BY_FLO11-_GFP`, ymin = `OFL5101 + BY_FLO11-_GFP_low`, ymax = `OFL5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + BY_FLO11-_GFP`, colour = "MCR5101 + BY_FLO11-"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + BY_FLO11-_GFP`, ymin = `MCR5101 + BY_FLO11-_GFP_low`, ymax = `MCR5101 + BY_FLO11-_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5105 + HKE3402_GFP`, colour = "MCR5105 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5105 + HKE3402_GFP`, ymin = `MCR5105 + HKE3402_GFP_low`, ymax = `MCR5105 + HKE3402_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + MCS5205_GFP`, colour = "MCR5101 + MCS5205"),size = 1) + 
  geom_ribbon(aes(y = `MCR5101 + MCS5205_GFP`, ymin = `MCR5101 + MCS5205_GFP_low`, ymax = `MCR5101 + MCS5205_GFP_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5114 + HKE3402_GFP`, colour = "MCR5114 + HKE3402"),size = 1) +
  geom_ribbon(aes(y = `MCR5114 + HKE3402_GFP`, ymin = `MCR5114 + HKE3402_GFP_low`, ymax = `MCR5114 + HKE3402_GFP_high`),alpha = 0.4) +
  labs(x = "Time in hours", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. time (hours)") +
  scale_x_continuous(breaks = seq(0, 24, by = 2))+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0, 24))

plot_GFP_time


plot_OD_time <- ggplot(df,aes(x=time_hours)) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_OD`,colour = "MCR5101 + OFL5101 (C-)"),size = 1) +
  #geom_ribbon(aes(y = `MCR5101 + OFL5101 (C-)_OD`, ymin = `MCR5101 + OFL5101 (C-)_OD_low`, ymax = `MCR5101 + OFL5101 (C-)_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)"),size = 1) +
  #geom_ribbon(aes(y = `MCR5101 + HKE3402 (C+)_OD`, ymin = `MCR5101 + HKE3402 (C+)_OD_low`, ymax = `MCR5101 + HKE3402 (C+)_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + OFL3201_OD`, colour = "MCR5101 + OFL3201"),size = 1) +
  #geom_ribbon(aes(y = `MCR5101 + OFL3201_OD`, ymin = `MCR5101 + OFL3201_OD_low`, ymax = `MCR5101 + OFL3201_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + HKE3402_OD`, colour = "OFL5101 + HKE3402"),size = 1) + 
  #geom_ribbon(aes(y = `OFL5101 + HKE3402_OD`, ymin = `OFL5101 + HKE3402_OD_low`, ymax = `OFL5101 + HKE3402_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + OFL3201_OD`, colour = "OFL5101 + OFL3201"),size = 1) + 
  #geom_ribbon(aes(y = `OFL5101 + OFL3201_OD`, ymin = `OFL5101 + OFL3201_OD_low`, ymax = `OFL5101 + OFL3201_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `OFL5101 + BY_FLO11-_OD`, colour = "OFL5101 + BY_FLO11-"),size = 1) + 
  #geom_ribbon(aes(y = `OFL5101 + BY_FLO11-_OD`, ymin = `OFL5101 + BY_FLO11-_OD_low`, ymax = `OFL5101 + BY_FLO11-_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + BY_FLO11-_OD`, colour = "MCR5101 + BY_FLO11-"),size = 1) + 
  #geom_ribbon(aes(y = `MCR5101 + BY_FLO11-_OD`, ymin = `MCR5101 + BY_FLO11-_OD_low`, ymax = `MCR5101 + BY_FLO11-_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5105 + HKE3402_OD`, colour = "MCR5105 + HKE3402"),size = 1) +
  #geom_ribbon(aes(y = `MCR5105 + HKE3402_OD`, ymin = `MCR5105 + HKE3402_OD_low`, ymax = `MCR5105 + HKE3402_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5101 + MCS5205_OD`, colour = "MCR5101 + MCS5205"),size = 1) + 
  #geom_ribbon(aes(y = `MCR5101 + MCS5205_OD`, ymin = `MCR5101 + MCS5205_OD_low`, ymax = `MCR5101 + MCS5205_OD_high`),alpha = 0.4) +
  geom_line(aes(y = `MCR5114 + HKE3402_OD`, colour = "MCR5114 + HKE3402"),size = 1) +
  #geom_ribbon(aes(y = `MCR5114 + HKE3402_OD`, ymin = `MCR5114 + HKE3402_OD_low`, ymax = `MCR5114 + HKE3402_OD_high`),alpha = 0.4) +
  labs(x = "Time in hours", y = "OD600 nm measurements", title = "OD600 nm vs. time (hours)") +
  geom_vline(xintercept = c(time_hours[c(20,73,130)]), linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 24, by = 2))+
  scale_y_continuous(breaks = seq(0.2, 1.6, by = 0.2))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0, 24))

plot_OD_time


GFPandOD_plots_merged <- plot_grid(plot_GFP_time,plot_OD_time,
                                   labels = c("A","B"),ncol = 2, nrow = 1)

GFPandOD_plots_merged

######### SECOND EXCEL + Plots #########

## Splitting data --> GFP and OD measures

GFP <- my_data_1[,1:119]
OD <- my_data_1[,c(1,120:237)]

## Converting the first column into ROW NAMES:

GFP <- as.data.frame(GFP) # Unnecessary??
row.names(GFP) <- GFP$Content
GFP_1 <- GFP[,(2:119)]

OD <- as.data.frame(OD) # Unnecessary??
row.names(OD) <- OD$Content
OD_1 <- OD[,(2:119)]

## Transposing both dataframes:https://www.marsja.se/how-to-transpose-a-dataframe-or-matrix-in-r-with-the-t-function/#:~:text=To%20interchange%20rows%20with%20columns,exchanging%20the%20rows%20and%20columns.

GFP_2 <- t(GFP_1)
GFP_2 <- as.data.frame(GFP_2)

OD_2 <- t(OD_1)
OD_2 <- as.data.frame(OD_2)

#df <- rbind(GFP_2,OD_2)
df <- cbind(GFP_2,OD_2)
df <- setNames(df, c("MCR5101 + OFL5101 (C-)_GFP","MCR5101 + HKE3402 (C+)_GFP",
                     "MCR5101 + M15_GFP", "MCR5101 + M21_GFP", 
                     "MCR5101 + M41_GFP", "MCR5101 + M42_GFP", 
                     "MCR5101 + OFL5101 (C-)_OD","MCR5101 + HKE3402 (C+)_OD",
                     "MCR5101 + M15_OD", "MCR5101 + M21_OD", 
                     "MCR5101 + M41_OD", "MCR5101 + M42_OD"))

## Plotting the final results: https://hbctraining.github.io/Intro-to-R/lessons/07_ggplot2.html

#install.packages("ggplot2")
library(ggplot2)


plot1 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`)) + 
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`))

plot2 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`)) + 
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`))

plot3 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`)) + 
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`))

plot4 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`)) +
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`))

plot5 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`)) +
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`))

plot6 <- ggplot(df) + 
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`)) +
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`))


final_plot <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) + 
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) + 
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) + 
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) + 
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) + 
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.9, 1.6))


final_plot_withoutCpos <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) + 
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) + 
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) + 
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) + 
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) + 
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.9, 1.6))


######### THIRD EXCEL + ALL RESULTS COMBINED ###############

## Splitting data --> GFP and OD measures

GFP_1 <- my_data_2[,1:146]
OD_1 <- my_data_2[,c(1,147:291)]

## Converting the first column into ROW NAMES:

GFP_1 <- as.data.frame(GFP_1) # Unnecessary??
row.names(GFP_1) <- GFP_1$Content
GFP_1 <- GFP_1[,(2:146)]

OD_1 <- as.data.frame(OD_1) # Unnecessary??
row.names(OD_1) <- OD_1$Content
OD_1 <- OD_1[,(2:146)]

## Transposing both dataframes:https://www.marsja.se/how-to-transpose-a-dataframe-or-matrix-in-r-with-the-t-function/#:~:text=To%20interchange%20rows%20with%20columns,exchanging%20the%20rows%20and%20columns.

GFP_2 <- t(GFP_1)
GFP_2 <- as.data.frame(GFP_2)

OD_2 <- t(OD_1)
OD_2 <- as.data.frame(OD_2)

#df <- rbind(GFP_2,OD_2)
df1 <- cbind(GFP_2,OD_2)
df1 <- setNames(df, c("MCR5101 + OFL5101 (C-)_GFP","MCR5101 + HKE3402 (C+)_GFP",
                     "MCR5101 + OFL3201_GFP", "OFL5101 + HKE3402_GFP", 
                     "OFL5101 + OFL3201_GFP",  "OFL5101 + FLO11-_GFP", 
                     "MCR5101 + FLO11-_GFP", "MCR5105 + HKE3402_GFP",
                     "MCR5101 + MCS5205_GFP", "MCR5114 + HKE3402_GFP",
                     "MCR5101 + M15_GFP", "MCR5101 + M21_GFP", 
                     "MCR5101 + M41_GFP", "MCR5101 + M42_GFP",
                     "MCR5101 + OFL5101 (C-)_OD","MCR5101 + HKE3402 (C+)_OD",
                     "MCR5101 + OFL3201_OD", "OFL5101 + HKE3402_OD", 
                     "OFL5101 + OFL3201_OD",  "OFL5101 + FLO11-_OD", 
                     "MCR5101 + FLO11-_OD", "MCR5105 + HKE3402_OD",
                     "MCR5101 + MCS5205_OD", "MCR5114 + HKE3402_OD",
                     "MCR5101 + M15_OD", "MCR5101 + M21_OD", 
                     "MCR5101 + M41_OD", "MCR5101 + M42_OD"))

## Plotting the final results: https://hbctraining.github.io/Intro-to-R/lessons/07_ggplot2.html
## We will also plot the fitting line and the FIRST DERIVATIVE --> https://stackoverflow.com/questions/6356665/how-do-i-plot-the-first-derivative-of-the-smoothing-function


plot_neg <- ggplot(data = df, aes(x = `MCR5101 + OFL5101 (C-)_OD`, y = `MCR5101 + OFL5101 (C-)_GFP`)) + 
  #geom_point() +
  geom_line() +
  #stat_smooth(method = "lm", formula = y ~ poly(x,7)) +
  ggtitle("MCR5101 + OFL5101 (C-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot_neg

plot_pos <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`)) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`)) +
  #geom_vline(xintercept = x_pos[c(89,111)], linetype = "dotdash", color = "purple") +
  ggtitle("MCR5101 + HKE3402 (C+)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot_pos


plot1 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`)) +
  geom_line(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`)) +
  ggtitle("MCR5101 + OFL3201") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot1

plot2 <- ggplot(df) + 
  #geom_point(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`)) +
  geom_line(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`)) +
  ggtitle("OFL5101 + HKE3402") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot2

plot3 <- ggplot(df) + 
  #geom_point(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`)) +
  geom_line(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`)) +
  #geom_vline(xintercept = x3[c(90,117)], linetype = "dotdash", color = "purple") +
  ggtitle("OFL5101 + OFL3201") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot3

plot4 <- ggplot(df) + 
  #geom_point(aes(y = `OFL5101 + FLO11-_GFP`, x = `OFL5101 + FLO11-_OD`)) +
  geom_line(aes(y = `OFL5101 + BY_FLO11-_GFP`, x = `OFL5101 + BY_FLO11-_OD`)) +
  #geom_vline(xintercept = x4[c(87,113)], linetype = "dotdash", color = "purple") +
  ggtitle("OFL5101 + BY_FLO11-") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot4

plot5 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + FLO11-_GFP`, x = `MCR5101 + FLO11-_OD`)) +
  geom_line(aes(y = `MCR5101 + BY_FLO11-_GFP`, x = `MCR5101 + BY_FLO11-_OD`)) +
  ggtitle("MCR5101 + BY_FLO11-") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot5

plot6 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`)) +
  geom_line(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`)) +
  ggtitle("MCR5105 + HKE3402") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot6

plot7 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`)) +
  geom_line(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`)) +
  #geom_vline(xintercept = x7[c(113,122)], linetype = "dotdash", color = "purple") +
  ggtitle("MCR5101 + MCS5205") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot7

plot8 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`)) +
  geom_line(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`)) +
  #geom_vline(xintercept = x8[c(84,110)], linetype = "dotdash", color = "purple") +
  ggtitle("MCR5114 + HKE3402") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=10)) +
  coord_cartesian(xlim = c(1.0, 1.45))

plot8

plot_M15 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`)) +
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`)) +
  ggtitle("MCR5101 + M15") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

plot_M15

plot_M21 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`)) +
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`)) +
  ggtitle("MCR5101 + M21") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

plot_M21

plot_M41 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`)) +
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`)) +
  ggtitle("MCR5101 + M41") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

plot_M41

plot_M42 <- ggplot(df) + 
  #geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`)) +
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`)) +
  #geom_vline(xintercept = x_42[c(118,134)], linetype = "dotdash", color = "purple") +
  ggtitle("MCR5101 + M42") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

plot_M42

final_plot <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_point(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`, colour = "MCR5101 + OFL3201")) + 
  geom_point(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`, colour = "OFL5101 + HKE3402")) + 
  geom_point(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`, colour = "OFL5101 + OFL3201")) + 
  geom_point(aes(y = `OFL5101 + FLO11-_GFP`, x = `OFL5101 + FLO11-_OD`, colour = "OFL5101 + FLO11-")) + 
  geom_point(aes(y = `MCR5101 + FLO11-_GFP`, x = `MCR5101 + FLO11-_OD`, colour = "MCR5101 + FLO11-")) + 
  geom_point(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`, colour = "MCR5105 + HKE3402")) + 
  geom_point(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`, colour = "MCR5101 + MCS5205")) + 
  geom_point(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`, colour = "MCR5114 + HKE3402")) +
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(y = `MCR5101 + OFL3201_GFP`, x = `MCR5101 + OFL3201_OD`, colour = "MCR5101 + OFL3201")) + 
  geom_line(aes(y = `OFL5101 + HKE3402_GFP`, x = `OFL5101 + HKE3402_OD`, colour = "OFL5101 + HKE3402")) + 
  geom_line(aes(y = `OFL5101 + OFL3201_GFP`, x = `OFL5101 + OFL3201_OD`, colour = "OFL5101 + OFL3201")) + 
  geom_line(aes(y = `OFL5101 + FLO11-_GFP`, x = `OFL5101 + FLO11-_OD`, colour = "OFL5101 + FLO11-")) + 
  geom_line(aes(y = `MCR5101 + FLO11-_GFP`, x = `MCR5101 + FLO11-_OD`, colour = "MCR5101 + FLO11-")) + 
  geom_line(aes(y = `MCR5105 + HKE3402_GFP`, x = `MCR5105 + HKE3402_OD`, colour = "MCR5105 + HKE3402")) + 
  geom_line(aes(y = `MCR5101 + MCS5205_GFP`, x = `MCR5101 + MCS5205_OD`, colour = "MCR5101 + MCS5205")) + 
  geom_line(aes(y = `MCR5114 + HKE3402_GFP`, x = `MCR5114 + HKE3402_OD`, colour = "MCR5114 + HKE3402")) +
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(1.0, 1.6))

final_plot

final_plot_Industrial_with_posControl <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(y = `MCR5101 + HKE3402 (C+)_GFP`, x = `MCR5101 + HKE3402 (C+)_OD`, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(1.0, 1.6))

final_plot_Industrial_without_posControl <- ggplot(df) +
  geom_point(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_point(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_point(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_point(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_point(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  geom_line(aes(y = `MCR5101 + OFL5101 (C-)_GFP`, x = `MCR5101 + OFL5101 (C-)_OD`, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(y = `MCR5101 + M15_GFP`, x = `MCR5101 + M15_OD`, colour = "MCR5101 + M15")) +
  geom_line(aes(y = `MCR5101 + M21_GFP`, x = `MCR5101 + M21_OD`, colour = "MCR5101 + M21")) +
  geom_line(aes(y = `MCR5101 + M41_GFP`, x = `MCR5101 + M41_OD`, colour = "MCR5101 + M41")) +
  geom_line(aes(y = `MCR5101 + M42_GFP`, x = `MCR5101 + M42_OD`, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP measurements (Fluorescence in RFU)", title = "GFP vs. OD Measurements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(1.0, 1.6))


###############################################################################
## 3 - First Derivative of GFP vs OD plots (Diff Plots)
## ALL PLOTS COMBINED (Controls, Original combinations & Industrials)
###############################################################################

### Using "diff" function:

diffs_neg <- diff(df$`MCR5101 + OFL5101 (C-)_GFP`) # Calculating differences between consecutive GFP values
x_neg <- df$`MCR5101 + OFL5101 (C-)_OD`[2:145] # As the differences are between a PAIR OF NUMBERS, we will omit the first OD value so that all arrays are of the same size
#diffs <- append(diffs,0) # As the differences are between a PAIR OF NUMBERS, we need to add another one to make this array the same size as the others

## Plotting differences between consecutive GFP values (Y axis) against OD measures (X axis):

# Negative control (C-):

diffplot_Cneg <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_neg, y = diffs_neg)) +
  labs(x = "MCR5101 + OFL5101 (C-)_OD",y = "MCR5101 + OFL5101 (C-)_GFP differences", title = "GFP differences vs. OD Measurements (C-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_Cneg

diffplot_Cneg1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_neg, y = diffs_neg)) +
  labs(x = "MCR5101 + OFL5101 (C-)_OD",y = "MCR5101 + OFL5101 (C-)_GFP differences", title = "GFP differences vs. OD Measurements (C-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cneg1

# Positive control (C+):

diffs_pos <- diff(df$`MCR5101 + HKE3402 (C+)_GFP`)
x_pos <- df$`MCR5101 + HKE3402 (C+)_OD`[2:145]

diffplot_Cpos <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_pos, y = diffs_pos)) +
  labs(x = "MCR5101 + HKE3402 (C+)_OD",y = "MCR5101 + HKE3402 (C+)_GFP differences", title = "GFP differences vs. OD Measurements (C+)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_Cpos

diffplot_Cpos1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_pos, y = diffs_pos)) +
  labs(x = "MCR5101 + HKE3402 (C+)_OD",y = "MCR5101 + HKE3402 (C+)_GFP differences", title = "GFP differences vs. OD Measurements (C+)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cpos1

# MCR5101 + OFL3201:

diffs1 <- diff(df$`MCR5101 + OFL3201_GFP`)
x1 <- df$`MCR5101 + OFL3201_OD`[2:145]

diffplot_1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x1, y = diffs1)) +
  labs(x = "MCR5101 + OFL3201_OD",y = "MCR5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + OFL3201)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_1

diffplot_1.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x1, y = diffs1)) +
  labs(x = "MCR5101 + OFL3201_OD",y = "MCR5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + OFL3201)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.5))

diffplot_1.1

# OFL5101 + HKE3402:

diffs2 <- diff(df$`OFL5101 + HKE3402_GFP`)
x2 <- df$`OFL5101 + HKE3402_OD`[2:145]

diffplot_2 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x2, y = diffs2)) +
  labs(x = "OFL5101 + HKE3402_OD",y = "OFL5101 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_2

diffplot_2.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x2, y = diffs2)) +
  labs(x = "OFL5101 + HKE3402_OD",y = "OFL5101 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.5))

diffplot_2.1

# OFL5101 + OFL3201:

diffs3 <- diff(df$`OFL5101 + OFL3201_GFP`)
x3 <- df$`OFL5101 + OFL3201_OD`[2:145]

diffplot_3 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x3, y = diffs3)) +
  labs(x = "OFL5101 + OFL3201_OD",y = "OFL5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + OFL3201)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_3

diffplot_3.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x3, y = diffs3)) +
  labs(x = "OFL5101 + OFL3201_OD",y = "OFL5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + OFL3201)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_3.1

# OFL5101 + BY_FLO11-:

diffs4 <- diff(df$`OFL5101 + BY_FLO11-_GFP`)
x4 <- df$`OFL5101 + BY_FLO11-_OD`[2:145]

diffplot_4 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x4, y = diffs4)) +
  labs(x = "OFL5101 + BY_FLO11-_OD",y = "OFL5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + BY_FLO11-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_4

diffplot_4.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x4, y = diffs4)) +
  labs(x = "OFL5101 + BY_FLO11-_OD",y = "OFL5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + BY_FLO11-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_4.1

# MCR5101 + FLO11-:

diffs5 <- diff(df$`MCR5101 + BY_FLO11-_GFP`)
x5 <- df$`MCR5101 + BY_FLO11-_OD`[2:145]

diffplot_5 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x5, y = diffs5)) +
  labs(x = "MCR5101 + BY_FLO11-_OD",y = "MCR5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + BY_FLO11-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_5

diffplot_5.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x5, y = diffs5)) +
  labs(x = "MCR5101 + BY_FLO11-_OD",y = "MCR5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + BY_FLO11-)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_5.1

# MCR5105 + HKE3402:

diffs6 <- diff(df$`MCR5105 + HKE3402_GFP`)
x6 <- df$`MCR5105 + HKE3402_OD`[2:145]

diffplot_6 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x6, y = diffs6)) +
  labs(x = "MCR5105 + HKE3402_OD",y = "MCR5105 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5105 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_6

diffplot_6.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x6, y = diffs6)) +
  labs(x = "MCR5105 + HKE3402_OD",y = "MCR5105 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5105 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_6.1

# MCR5101 + MCS5205:

diffs7 <- diff(df$`MCR5101 + MCS5205_GFP`)
x7 <- df$`MCR5101 + MCS5205_OD`[2:145]

diffplot_7 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x7, y = diffs7)) +
  labs(x = "MCR5101 + MCS5205_OD",y = "MCR5101 + MCS5205_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + MCS5205)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_7

diffplot_7.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x7, y = diffs7)) +
  labs(x = "MCR5101 + MCS5205_OD",y = "MCR5101 + MCS5205_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + MCS5205)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_7.1

# MCR5114 + HKE3402:

diffs8 <- diff(df$`MCR5114 + HKE3402_GFP`)
x8 <- df$`MCR5114 + HKE3402_OD`[2:145]

diffplot_8 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x8, y = diffs8)) +
  labs(x = "MCR5114 + HKE3402_OD",y = "MCR5114 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5114 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.45))

diffplot_8

diffplot_8.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x8, y = diffs8)) +
  labs(x = "MCR5114 + HKE3402_OD",y = "MCR5114 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5114 + HKE3402)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_8.1

# MCR5101 + M15:

diffs_15 <- diff(df$`MCR5101 + M15_GFP`)
x_15 <- df$`MCR5101 + M15_OD`[2:145]

diffplot_M15 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_15, y = diffs_15)) +
  labs(x = "MCR5101 + M15_OD",y = "MCR5101 + M15_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M15)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.5))

diffplot_M15

diffplot_M15.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_15, y = diffs_15)) +
  labs(x = "MCR5101 + M15_OD",y = "MCR5101 + M15_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M15)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.5))

diffplot_M15.1

# MCR5101 + M21:

diffs_21 <- diff(df$`MCR5101 + M21_GFP`)
x_21 <- df$`MCR5101 + M21_OD`[2:145]

diffplot_M21 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_21, y = diffs_21)) +
  labs(x = "MCR5101 + M21_OD",y = "MCR5101 + M21_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M21)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.5))

diffplot_M21

diffplot_M21.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_21, y = diffs_21)) +
  labs(x = "MCR5101 + M21_OD",y = "MCR5101 + M21_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M21)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.5))

diffplot_M21.1

# MCR5101 + M41:

diffs_41 <- diff(df$`MCR5101 + M41_GFP`)
x_41 <- df$`MCR5101 + M41_OD`[2:145]

diffplot_M41 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_41, y = diffs_41)) +
  labs(x = "MCR5101 + M41_OD",y = "MCR5101 + M41_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M41)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M41

diffplot_M41.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_41, y = diffs_41)) +
  labs(x = "MCR5101 + M41_OD",y = "MCR5101 + M41_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M41)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.6))

diffplot_M41.1

# MCR5101 + M42:

diffs_42 <- diff(df$`MCR5101 + M42_GFP`)
x_42 <- df$`MCR5101 + M42_OD`[2:145]

diffplot_M42 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_42, y = diffs_42)) +
  labs(x = "MCR5101 + M42_OD",y = "MCR5101 + M42_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M42)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M42

diffplot_M42.1 <- ggplot() + 
  #geom_point(aes(y = diffs, x = x)) + 
  geom_line(aes(x = x_42, y = diffs_42)) +
  labs(x = "MCR5101 + M42_OD",y = "MCR5101 + M42_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M42)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(1.0, 1.6))

diffplot_M42.1



## All created plots merged:

final_plot_Industrial_GFP_differences <- ggplot() +
  geom_line(aes(x = x_neg, y = diffs_neg, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(x = x_15, y = diffs_15, colour = "MCR5101 + M15")) +
  geom_line(aes(x = x_21, y = diffs_21, colour = "MCR5101 + M21")) +
  geom_line(aes(x = x_41, y = diffs_41, colour = "MCR5101 + M41")) +
  geom_line(aes(x = x_42, y = diffs_42, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP differences (Fluorescence in RFU)", title = "GFP differences vs. OD Measurements (Industrial Strains)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.2, 1.6))

final_plot_Industrial_GFP_differences


final_plot_Industrial_GFP_differences_withCpos <- ggplot() +
  geom_line(aes(x = x_neg, y = diffs_neg, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(x = x_pos, y = diffs_pos, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(x = x_15, y = diffs_15, colour = "MCR5101 + M15")) +
  geom_line(aes(x = x_21, y = diffs_21, colour = "MCR5101 + M21")) +
  geom_line(aes(x = x_41, y = diffs_41, colour = "MCR5101 + M41")) +
  geom_line(aes(x = x_42, y = diffs_42, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP differences (Fluorescence in RFU)", title = "GFP differences vs. OD Measurements (Industrial Strains + Positive Control)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.2, 1.6))

final_plot_Industrial_GFP_differences_withCpos


final_plot_ALL_GFP_differences <- ggplot() +
  geom_line(aes(x = x_neg, y = diffs_neg, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(x = x_pos, y = diffs_pos, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(x = x1, y = diffs1, colour = "MCR5101 + OFL3201")) + 
  geom_line(aes(x = x2, y = diffs2, colour = "OFL5101 + HKE3402")) + 
  geom_line(aes(x = x3, y = diffs3, colour = "OFL5101 + OFL3201")) + 
  geom_line(aes(x = x4, y = diffs4, colour = "OFL5101 + FLO11-")) + 
  geom_line(aes(x = x5, y = diffs5, colour = "MCR5101 + FLO11-")) + 
  geom_line(aes(x = x6, y = diffs6, colour = "MCR5105 + HKE3402")) + 
  geom_line(aes(x = x7, y = diffs7, colour = "MCR5101 + MCS5205")) + 
  geom_line(aes(x = x8, y = diffs8, colour = "MCR5114 + HKE3402")) +
  geom_line(aes(x = x_15, y = diffs_15, colour = "MCR5101 + M15")) +
  geom_line(aes(x = x_21, y = diffs_21, colour = "MCR5101 + M21")) +
  geom_line(aes(x = x_41, y = diffs_41, colour = "MCR5101 + M41")) +
  geom_line(aes(x = x_42, y = diffs_42, colour = "MCR5101 + M42")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP differences (Fluorescence in RFU)", title = "GFP differences vs. OD Measurements (All Strains)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.2, 1.6))

final_plot_ALL_GFP_differences


final_plot_GFP_differences_withoutINDUSTRIALS <- ggplot() +
  geom_line(aes(x = x_neg, y = diffs_neg, colour = "MCR5101 + OFL5101 (C-)")) +
  geom_line(aes(x = x_pos, y = diffs_pos, colour = "MCR5101 + HKE3402 (C+)")) +
  geom_line(aes(x = x1, y = diffs1, colour = "MCR5101 + OFL3201")) + 
  geom_line(aes(x = x2, y = diffs2, colour = "OFL5101 + HKE3402")) + 
  geom_line(aes(x = x3, y = diffs3, colour = "OFL5101 + OFL3201")) + 
  geom_line(aes(x = x4, y = diffs4, colour = "OFL5101 + FLO11-")) + 
  geom_line(aes(x = x5, y = diffs5, colour = "MCR5101 + FLO11-")) + 
  geom_line(aes(x = x6, y = diffs6, colour = "MCR5105 + HKE3402")) + 
  geom_line(aes(x = x7, y = diffs7, colour = "MCR5101 + MCS5205")) + 
  geom_line(aes(x = x8, y = diffs8, colour = "MCR5114 + HKE3402")) +
  labs(x = "Optical Density (OD) measurements", y = "GFP differences (Fluorescence in RFU)", title = "GFP differences vs. OD Measurements (without Industrial Strains)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(col=guide_legend("Combinations")) + 
  coord_cartesian(xlim = c(0.2, 1.5))

final_plot_GFP_differences_withoutINDUSTRIALS



## Combining Plots (http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#:~:text=To%20arrange%20multiple%20ggplot2%20graphs,multiple%20ggplots%20on%20one%20page):

plot_grid(diffplot_Cneg,diffplot_Cneg1,diffplot_Cpos,diffplot_Cpos1,
          labels = c("A","A1","B","B1"),ncol = 2, nrow = 2)

plot_grid(diffplot_M15,diffplot_M15.1,diffplot_M21,diffplot_M21.1,
          labels = c("C","C1","D","D1"),ncol = 2, nrow = 2)

plot_grid(diffplot_M41,diffplot_M41.1,diffplot_M42,diffplot_M42.1,
          labels = c("E","E1","F","F1"),ncol = 2, nrow = 2)

plot_grid(diffplot_1,diffplot_1.1,diffplot_2,diffplot_2.1,
          labels = c("G","G1","H","H1"),ncol = 2, nrow = 2)

plot_grid(diffplot_3,diffplot_3.1,diffplot_4,diffplot_4.1,
          labels = c("I","I1","J","J1"),ncol = 2, nrow = 2)

plot_grid(diffplot_5,diffplot_5.1,diffplot_6,diffplot_6.1,
          labels = c("K","K1","L","L1"),ncol = 2, nrow = 2)

plot_grid(diffplot_7,diffplot_7.1,diffplot_8,diffplot_8.1,
          labels = c("M","M1","N","N1"),ncol = 2, nrow = 2)



###############################################################################
## 4 - Smoothed Plots (using Savitzky-Golay Smoothing)
## ALL PLOTS COMBINED (Controls, Original combinations & Industrials)
###############################################################################

### Savitzky-Golay Smoothing --> https://search.r-project.org/CRAN/refmans/pracma/html/savgol.html

# Negative control (C-):

smooth_neg <- savgol(diffs_neg,61)

diffplot_Cneg_smooth <- ggplot(mapping = aes(x=x_neg)) + 
  geom_line(aes(y = smooth_neg)) +
  labs(x = "MCR5101 + OFL5101 (C-)_OD",y = "MCR5101 + OFL5101 (C-)_GFP differences", title = "GFP differences vs. OD Measurements (C-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cneg_smooth

diffplot_Cneg.1_smooth <- ggplot(mapping = aes(x=x_neg)) + 
  geom_line(aes(y = smooth_neg, color="Smooth data")) +
  geom_line(aes(y = diffs_neg, color="Original data")) +
  labs(x = "MCR5101 + OFL5101 (C-)_OD",y = "MCR5101 + OFL5101 (C-)_GFP differences", title = "GFP differences vs. OD Measurements (C-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"), axis.title = element_text(size = 8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cneg.1_smooth

# Positive control (C+):

smooth_pos <- savgol(diffs_pos, 29)

diffplot_Cpos_smooth <- ggplot(mapping = aes(x=x_pos)) + 
  geom_line(mapping = aes(y = smooth_pos)) +
  labs(x = "MCR5101 + HKE3402 (C+)_OD",y = "MCR5101 + HKE3402 (C+)_GFP differences", title = "GFP differences vs. OD Measurements (C+)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cpos_smooth

diffplot_Cpos.1_smooth <- ggplot(mapping = aes(x=x_pos)) + 
  geom_line(mapping = aes(y = smooth_pos, color="Smooth data")) +
  geom_line(mapping = aes(y = diffs_pos, color="Original data")) +
  labs(x = "MCR5101 + HKE3402 (C+)_OD",y = "MCR5101 + HKE3402 (C+)_GFP differences", title = "GFP differences vs. OD Measurements (C+)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_Cpos.1_smooth


# MCR5101 + OFL3201:

smooth_1 <- savgol(diffs1,33)

diffplot_1_smooth <- ggplot(mapping = aes(x=x1)) + 
  geom_line(aes(y = smooth_1)) +
  labs(x = "MCR5101 + OFL3201_OD",y = "MCR5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + OFL3201)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_1_smooth

diffplot_1.1_smooth <- ggplot(mapping = aes(x=x1)) + 
  geom_line(aes(y = smooth_1, color="Smooth data")) +
  geom_line(aes(y = diffs1, color="Original data")) +
  labs(x = "MCR5101 + OFL3201_OD",y = "MCR5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + OFL3201)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_1.1_smooth

# OFL5101 + HKE3402:

smooth_2 <- savgol(diffs2,51)

diffplot_2_smooth <- ggplot(mapping = aes(x=x2)) + 
  geom_line(aes(y = smooth_2)) +
  labs(x = "OFL5101 + HKE3402_OD",y = "OFL5101 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_2_smooth

diffplot_2.1_smooth <- ggplot(mapping = aes(x=x2)) + 
  geom_line(aes(y = smooth_2, color="Smooth data")) +
  geom_line(aes(y = diffs2, color="Original data")) +
  labs(x = "OFL5101 + HKE3402_OD",y = "OFL5101 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_2.1_smooth

# OFL5101 + OFL3201:

smooth_3 <- savgol(diffs3,43)

diffplot_3_smooth <- ggplot(mapping = aes(x=x3)) + 
  geom_line(aes(y = smooth_3)) +
  labs(x = "OFL5101 + OFL3201_OD",y = "OFL5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + OFL3201)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_3_smooth

diffplot_3.1_smooth <- ggplot(mapping = aes(x=x3)) + 
  geom_line(aes(y = smooth_3, color="Smooth data")) +
  geom_line(aes(y = diffs3, color="Original data")) +
  labs(x = "OFL5101 + OFL3201_OD",y = "OFL5101 + OFL3201_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + OFL3201)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_3.1_smooth

# OFL5101 + BY_FLO11-:

smooth_4 <- savgol(diffs4,41)

diffplot_4_smooth <- ggplot(mapping = aes(x=x4)) + 
  geom_line(aes(y = smooth_4)) +
  labs(x = "OFL5101 + BY_FLO11-_OD",y = "OFL5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + BY_FLO11-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_4_smooth

diffplot_4.1_smooth <- ggplot(mapping = aes(x=x4)) + 
  geom_line(aes(y = smooth_4, color="Smooth data")) +
  geom_line(aes(y = diffs4, color="Original data")) +
  labs(x = "OFL5101 + BY_FLO11-_OD",y = "OFL5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (OFL5101 + BY_FLO11-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_4.1_smooth

# MCR5101 + BY_FLO11-:

smooth_5 <- savgol(diffs5,35)

diffplot_5_smooth <- ggplot(mapping = aes(x=x5)) + 
  geom_line(aes(y = smooth_5)) +
  labs(x = "MCR5101 + BY_FLO11-_OD",y = "MCR5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + BY_FLO11-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_5_smooth

diffplot_5.1_smooth <- ggplot(mapping = aes(x=x5)) + 
  geom_line(aes(y = smooth_5, color="Smooth data")) +
  geom_line(aes(y = diffs5, color="Original data")) +
  labs(x = "MCR5101 + BY_FLO11-_OD",y = "MCR5101 + BY_FLO11-_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + BY_FLO11-)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_5.1_smooth

# MCR5105 + HKE3402:

smooth_6 <- savgol(diffs6,65)

diffplot_6_smooth <- ggplot(mapping = aes(x=x6)) + 
  geom_line(aes(y = smooth_6)) +
  labs(x = "MCR5105 + HKE3402_OD",y = "MCR5105 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5105 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_6_smooth

diffplot_6.1_smooth <- ggplot(mapping = aes(x=x6)) + 
  geom_line(aes(y = smooth_6, color="Smooth data")) +
  geom_line(aes(y = diffs6, color="Original data")) +
  labs(x = "MCR5105 + HKE3402_OD",y = "MCR5105 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5105 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_6.1_smooth

# MCR5101 + MCS5205:

smooth_7 <- savgol(diffs7,27)

diffplot_7_smooth <- ggplot(mapping = aes(x=x7)) + 
  geom_line(aes(y = smooth_7)) +
  labs(x = "MCR5101 + MCS5205_OD",y = "MCR5101 + MCS5205_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + MCS5205)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_7_smooth

diffplot_7.1_smooth <- ggplot(mapping = aes(x=x7)) + 
  geom_line(aes(y = smooth_7, color="Smooth data")) +
  geom_line(aes(y = diffs7, color="Original data")) +
  labs(x = "MCR5101 + MCS5205_OD",y = "MCR5101 + MCS5205_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + MCS5205)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_7.1_smooth

# MCR5114 + HKE3402:

smooth_8 <- savgol(diffs8,41)

diffplot_8_smooth <- ggplot(mapping = aes(x = x8)) +
  geom_line(aes(y = smooth_8)) +
  labs(x = "MCR5114 + HKE3402_OD",y = "MCR5114 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5114 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_8_smooth

diffplot_8.1_smooth <- ggplot(mapping = aes(x = x8)) +
  geom_line(aes(y = smooth_8, color="Smooth data")) +
  geom_line(aes(y = diffs8, color="Original data")) +
  labs(x = "MCR5114 + HKE3402_OD",y = "MCR5114 + HKE3402_GFP differences", title = "GFP differences vs. OD Measurements (MCR5114 + HKE3402)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), axis.title = element_text(size=8)) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(1.0, 1.45))

diffplot_8.1_smooth

# MCR5101 + M15:

smooth_M15 <- savgol(diffs_15,51)

diffplot_M15_smooth <- ggplot(mapping = aes(x = x_15)) + 
  geom_line(aes(y = smooth_M15)) +
  labs(x = "MCR5101 + M15_OD",y = "MCR5101 + M15_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M15)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M15_smooth

diffplot_M15.1_smooth <- ggplot(mapping = aes(x = x_15)) + 
  geom_line(aes(y = smooth_M15, color="Smooth data")) +
  geom_line(aes(y = diffs_15, color="Original data")) +
  labs(x = "MCR5101 + M15_OD",y = "MCR5101 + M15_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M15)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M15.1_smooth

# MCR5101 + M21:

smooth_M21 <- savgol(diffs_21,51)

diffplot_M21_smooth <- ggplot(mapping = aes(x = x_21)) + 
  geom_line(aes(y = smooth_M21)) +
  labs(x = "MCR5101 + M21_OD",y = "MCR5101 + M21_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M21)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M21_smooth

diffplot_M21.1_smooth <- ggplot(mapping = aes(x = x_21)) + 
  geom_line(aes(y = smooth_M21, color="Smooth data")) +
  geom_line(aes(y = diffs_21, color="Original data")) +
  labs(x = "MCR5101 + M21_OD",y = "MCR5101 + M21_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M21)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M21.1_smooth

# MCR5101 + M41:

smooth_M41 <- savgol(diffs_41,51)

diffplot_M41_smooth <- ggplot(mapping = aes(x = x_41)) + 
  geom_line(aes(y = smooth_M41)) +
  labs(x = "MCR5101 + M41_OD",y = "MCR5101 + M41_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M41)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M41_smooth

diffplot_M41.1_smooth <- ggplot(mapping = aes(x = x_41)) + 
  geom_line(aes(y = smooth_M41, color="Smooth data")) +
  geom_line(aes(y = diffs_41, color="Original data")) +
  labs(x = "MCR5101 + M41_OD",y = "MCR5101 + M41_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M41)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M41.1_smooth

# MCR5101 + M42:

smooth_M42 <- savgol(diffs_42,51)

diffplot_M42_smooth <- ggplot(mapping = aes(x = x_42)) + 
  geom_line(aes(y = smooth_M42)) +
  labs(x = "MCR5101 + M42_OD",y = "MCR5101 + M42_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M42)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M42_smooth

diffplot_M42.1_smooth <- ggplot(mapping = aes(x = x_42)) + 
  geom_line(aes(y = smooth_M42, color="Smooth data")) +
  geom_line(aes(y = diffs_42, color="Original data")) +
  labs(x = "MCR5101 + M42_OD",y = "MCR5101 + M42_GFP differences", title = "GFP differences vs. OD Measurements (MCR5101 + M42)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5), axis.title = element_text(size=8,face = "bold")) +
  scale_color_manual(values = c("Smooth data" = "green", "Original data" = "blue")) +
  coord_cartesian(xlim = c(0.2, 1.6))

diffplot_M42.1_smooth

## Combining smoothed plots with original ones for ease of comparison

plot_grid(diffplot_Cneg1,diffplot_Cneg_smooth,diffplot_Cpos1,diffplot_Cpos_smooth,
          labels = c("A","A1","B","B1"),ncol = 2, nrow = 2)
plot_grid(diffplot_1.1,diffplot_1.1_smooth,diffplot_2.1,diffplot_2.1_smooth,
          labels = c("C","C1","D","D1"),ncol = 2, nrow = 2)
plot_grid(diffplot_3.1,diffplot_3.1_smooth,diffplot_4.1,diffplot_4.1_smooth,
          labels = c("E","E1","F","F1"),ncol = 2, nrow = 2)
plot_grid(diffplot_5.1,diffplot_5.1_smooth,diffplot_6.1,diffplot_6.1_smooth,
          labels = c("G","G1","H","H1"),ncol = 2, nrow = 2)
plot_grid(diffplot_7.1,diffplot_7.1_smooth,diffplot_8.1,diffplot_8.1_smooth,
          labels = c("I","I1","J","J1"),ncol = 2, nrow = 2)
plot_grid(diffplot_M15.1,diffplot_M15.1_smooth,diffplot_M21.1,diffplot_M21.1_smooth,
          labels = c("K","K1","L","L1"),ncol = 2, nrow = 2)
plot_grid(diffplot_M41.1,diffplot_M41.1_smooth,diffplot_M42.1,diffplot_M42.1_smooth,
          labels = c("M","M1","N","N1"),ncol = 2, nrow = 2)


### ANOTHER WAY: Creating Scatterplot with Fitted Smooth Line Using ggplot2 Package: https://statisticsglobe.com/fit-smooth-curve-to-plot-of-data-in-r
# https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/

data_Cpos <- data.frame(x_pos, diffs_pos)

ggplot(data_Cpos,aes(x = x_pos, y = diffs_pos))+
  geom_point()+
  geom_smooth(se = FALSE,
              method = "loess",
              formula = y ~ poly(x, 4))

#### (BONUS) OBTAINING OD VALUES AT 50% INCREASE IN GFP MEASURES ###
## First of all, we need to select the specific range of GFP values we are interested in (excluding the curve values which begin to decay sharply)

#diffs_pos1 <- tail(diffs_pos,n = 70) # LAST 70 VALUES OF A VECTOR

max_pos <- match(max(smooth_pos),smooth_pos) # Retrieving the index of the maximum value of smooth points
#smooth_pos1 <- head(smooth_pos,n = 111) # Selecting the new range established
smooth_pos1 <- smooth_pos[72:111]

pos_50 <- quantile(smooth_pos1,probs = .5) # 50TH PERCENTILE
#match(pos_50,smooth_pos)


##### Plotting together Original and Smoothed data #####

plot_grid(diffplot_Cneg.1_smooth,diffplot_Cpos.1_smooth,
          ncol = 2, nrow = 1, label_x = 1)
plot_grid(diffplot_1.1_smooth, diffplot_2.1_smooth, diffplot_3.1_smooth,
          diffplot_4.1_smooth, ncol = 2, nrow = 2, label_x = 1)
plot_grid(diffplot_5.1_smooth, diffplot_6.1_smooth, diffplot_7.1_smooth,
          diffplot_8.1_smooth, ncol = 2, nrow = 2, label_x = 1)
plot_grid(diffplot_M15.1_smooth, diffplot_M21.1_smooth, diffplot_M41.1_smooth,
          diffplot_M42.1_smooth, ncol = 2, nrow = 2, label_x = 1)


plot_grid(diffplot_Cneg.1_smooth,diffplot_Cpos.1_smooth,diffplot_1.1_smooth,
          diffplot_2.1_smooth, diffplot_3.1_smooth,diffplot_4.1_smooth,
          labels = c("A","B","C","D","E","F"),ncol = 3, nrow = 2)
plot_grid(diffplot_5.1_smooth, diffplot_6.1_smooth, diffplot_7.1_smooth,diffplot_8.1_smooth,
          labels = c("G","H","I","J","K","L"),ncol = 3, nrow = 2)


###############################################################################
## 5 - Obtaining Inflection Points
## In First Derivative Plots (Diff Plots)
## In Original Plots (GFP vs. OD Plots)
###############################################################################

#### Negative control (C-): ####

max_neg <- match(max(smooth_neg),smooth_neg)

infl_neg <- c(FALSE, diff(diff(smooth_neg)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_neg==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_Cneg_inflpoints <- diffplot_Cneg_smooth +
  geom_point(aes(x = x_neg[infl_neg], y = smooth_neg[infl_neg]), color = "blue") +
  geom_vline(xintercept = c(x_neg[infl_neg][66], x_neg[max_neg]), linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_neg[71], linetype = "dashed", color = "red")

diffplot_Cneg_inflpoints

# Negative control (C-): USING THE ORIGINAL PLOT!!

x_neg1 <- df$`MCR5101 + OFL5101 (C-)_OD`
y_neg1 <- df$`MCR5101 + OFL5101 (C-)_GFP`


max_neg1 <- match(max(y_neg1),y_neg1)

infl_neg1 <- c(FALSE, diff(diff(y_neg1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_neg1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_neg1_indx <- which(infl_neg1==TRUE)

infl_neg_df <- data.frame(col1 = c(y_neg1[infl_neg1_indx]),
                          col2 = c(x_neg1[infl_neg1_indx]))

highlight_df_neg <- data.frame(col3 = c(x_neg1[c(96,120,max_neg1)]),
                               col4 = c(y_neg1[c(96,120,max_neg1)]))


plot_Cneg_inflpoints <- plot_neg +
  geom_point(data = infl_neg_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_neg,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = c(x_neg[95], x_neg[max_neg]), linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = c(x_neg1[72],x_neg1[max_neg1]), linetype = "dashed", color = "red")

plot_Cneg_inflpoints


#### Positive control (C+): #### 

max_pos <- match(max(smooth_pos),smooth_pos)

infl_pos <- c(FALSE, diff(diff(smooth_pos)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_pos==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_Cpos_inflpoints <- diffplot_Cpos_smooth +
  geom_point(aes(x = x_pos[infl_pos], y = smooth_pos[infl_pos]), color = "blue") +
  geom_vline(xintercept = c(x_pos[87],x_pos[max_pos]), linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_pos[68], linetype = "dashed", color = "red")

diffplot_Cpos_inflpoints


# Positive control (C+): USING THE ORIGINAL PLOT!!

x_pos1 <- df$`MCR5101 + HKE3402 (C+)_OD`
y_pos1 <- df$`MCR5101 + HKE3402 (C+)_GFP`


max_pos1 <- match(max(y_pos1),y_pos1)

infl_pos1 <- c(FALSE, diff(diff(y_pos1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_pos1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_pos1_indx <- which(infl_pos1==TRUE)

infl_pos_df <- data.frame(col1 = c(df$`MCR5101 + HKE3402 (C+)_GFP`[infl_pos1_indx]),
                          col2 = c(df$`MCR5101 + HKE3402 (C+)_OD`[infl_pos1_indx]))

highlight_df_pos <- data.frame(col3 = c(x_pos1[c(88,110,max_pos1)]),
                               col4 = c(y_pos1[c(88,110,max_pos1)]))


plot_Cpos_inflpoints <- plot_pos +
  geom_point(data = infl_pos_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_pos,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x_pos[c(87,109)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = c(x_pos1[69],x_pos1[max_pos1]), linetype = "dashed", color = "red")

plot_Cpos_inflpoints

#### 1) MCR5101 + OFL3201: #### 

max_1 <- match(max(smooth_1),smooth_1)

infl_1 <- c(FALSE, diff(diff(smooth_1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_1_inflpoints <- diffplot_1_smooth +
  geom_point(aes(x = x1[infl_1], y = smooth_1[infl_1]), color = "blue") +
  geom_vline(xintercept = c(x1[infl_1][58],x1[max_1]), linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x1[72], linetype = "dashed", color = "red")

diffplot_1_inflpoints

# MCR5101 + OFL3201: USING THE ORIGINAL PLOT!!

y1.1 <- df$`MCR5101 + OFL3201_GFP`
x1.1 <- df$`MCR5101 + OFL3201_OD`


max_1.1 <- match(max(y1.1),y1.1)

infl_1.1 <- c(FALSE, diff(diff(y1.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_1.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_1.1_indx <- which(infl_1.1==TRUE)

infl_1_df <- data.frame(col1 = c(y1.1[infl_1.1_indx]), col2 = c(x1.1[infl_1.1_indx]))

highlight_df_1 <- data.frame(col3 = c(x1.1[c(103,117,max_1.1)]),
                             col4 = c(y1.1[c(103,117,max_1.1)]))


plot_1_inflpoints <- plot1 +
  geom_point(data = infl_1_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_1,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x1[c(102,116)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = c(x1.1[73],x1.1[max_1.1]), linetype = "dashed", color = "red")

plot_1_inflpoints


#### 2) OFL5101 + HKE3402: #### 

max_2 <- match(max(smooth_2),smooth_2)

infl_2 <- c(FALSE, diff(diff(smooth_2)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_2==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_2_inflpoints <- diffplot_2_smooth +
  geom_point(aes(x = x2[infl_2], y = smooth_2[infl_2]), color = "blue") +
  geom_vline(xintercept = x2[c(82,max_2)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x2[68], linetype = "dashed", color = "red")

diffplot_2_inflpoints

x2[c(82,110)]

# OFL5101 + HKE3402: USING THE ORIGINAL PLOT!!

y2.1 <- df$`OFL5101 + HKE3402_GFP`
x2.1 <- df$`OFL5101 + HKE3402_OD`


max_2.1 <- match(max(y2.1),y2.1)

infl_2.1 <- c(FALSE, diff(diff(y2.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_2.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_2.1_indx <- which(infl_2.1==TRUE)

infl_2_df <- data.frame(col1 = c(y2.1[infl_2.1_indx]), col2 = c(x2.1[infl_2.1_indx]))

highlight_df_2 <- data.frame(col3 = c(x2.1[c(83,111,max_2.1)]),
                             col4 = c(y2.1[c(83,111,max_2.1)]))


plot_2_inflpoints <- plot2 +
  geom_point(data = infl_2_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_2,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x2[c(82,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x2.1[c(69,max_2.1)], linetype = "dashed", color = "red")

plot_2_inflpoints


#### 3) OFL5101 + OFL3201: #### 

max_3 <- match(max(smooth_3),smooth_3)

infl_3 <- c(FALSE, diff(diff(smooth_3)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_3==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_3_inflpoints <- diffplot_3_smooth +
  geom_point(aes(x = x3[infl_3], y = smooth_3[infl_3]), color = "blue") +
  geom_vline(xintercept = x3[c(94,max_3)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x3[69], linetype = "dashed", color = "red")

diffplot_3_inflpoints

x3[c(94,115)]

# OFL5101 + OFL3201: USING THE ORIGINAL PLOT!!

y3.1 <- df$`OFL5101 + OFL3201_GFP`
x3.1 <- df$`OFL5101 + OFL3201_OD`

max_3.1 <- match(max(y3.1),y3.1)

infl_3.1 <- c(FALSE, diff(diff(y3.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_3.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_3.1_indx <- which(infl_3.1==TRUE)

infl_3_df <- data.frame(col1 = c(y3.1[infl_3.1_indx]), col2 = c(x3.1[infl_3.1_indx]))

highlight_df_3 <- data.frame(col3 = c(x3.1[c(95,117,max_3.1)]),
                             col4 = c(y3.1[c(95,117,max_3.1)]))


plot_3_inflpoints <- plot3 +
  geom_point(data = infl_3_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_3,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x3[c(94,116)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x3.1[c(71,max_3.1)], linetype = "dashed", color = "red")

plot_3_inflpoints


#### 4) OFL5101 + BY_FLO11-: #### 

max_4 <- match(max(smooth_4),smooth_4)

infl_4 <- c(FALSE, diff(diff(smooth_4)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_4==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_4_inflpoints <- diffplot_4_smooth +
  geom_point(aes(x = x4[infl_4], y = smooth_4[infl_4]), color = "blue") +
  geom_vline(xintercept = x4[c(87,max_4)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x4[70], linetype = "dashed", color = "red")

diffplot_4_inflpoints

x4[c(87,113)]

# OFL5101 + FLO11-: USING THE ORIGINAL PLOT!!

y4.1 <- df$`OFL5101 + BY_FLO11-_GFP`
x4.1 <- df$`OFL5101 + BY_FLO11-_OD`

max_4.1 <- match(max(y4.1),y4.1)

infl_4.1 <- c(FALSE, diff(diff(y4.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_4.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_4.1_indx <- which(infl_4.1==TRUE)

infl_4_df <- data.frame(col1 = c(y4.1[infl_4.1_indx]), col2 = c(x4.1[infl_4.1_indx]))

highlight_df_4 <- data.frame(col3 = c(x4.1[c(88,114,max_4.1)]),
                             col4 = c(y4.1[c(88,114,max_4.1)]))

plot_4_inflpoints <- plot4 +
  geom_point(data = infl_4_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_4,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x4[c(87,113)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x4.1[c(71,max_4.1)], linetype = "dashed", color = "red")

plot_4_inflpoints


#### 5) MCR5101 + BY_FLO11-: #### 

max_5 <- match(max(smooth_5),smooth_5)

infl_5 <- c(FALSE, diff(diff(smooth_5)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_5==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_5_inflpoints <- diffplot_5_smooth +
  geom_point(aes(x = x5[infl_5], y = smooth_5[infl_5]), color = "blue") +
  geom_vline(xintercept = x5[c(97,max_5)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x5[70], linetype = "dashed", color = "red")

diffplot_5_inflpoints

x5[c(97,112)]

# MCR5101 + FLO11-: USING THE ORIGINAL PLOT!!

y5.1 <- df$`MCR5101 + BY_FLO11-_GFP`
x5.1 <- df$`MCR5101 + BY_FLO11-_OD`

max_5.1 <- match(max(y5.1),y5.1)

infl_5.1 <- c(FALSE, diff(diff(y5.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_5.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_5.1_indx <- which(infl_5.1==TRUE)

infl_5_df <- data.frame(col1 = c(y5.1[infl_5.1_indx]), col2 = c(x5.1[infl_5.1_indx]))

highlight_df_5 <- data.frame(col3 = c(x5.1[c(98,113,max_5.1)]),
                             col4 = c(y5.1[c(98,113,max_5.1)]))

plot_5_inflpoints <- plot5 +
  geom_point(data = infl_5_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_5,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x5[c(97,112)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x5.1[c(71,max_5.1)], linetype = "dashed", color = "red")

plot_5_inflpoints


#### 6) MCR5105 + HKE3402: #### 

max_6 <- match(max(smooth_6),smooth_6)

infl_6 <- c(FALSE, diff(diff(smooth_6)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_6==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_6_inflpoints <- diffplot_6_smooth +
  geom_point(aes(x = x6[infl_6], y = smooth_6[infl_6]), color = "blue") +
  geom_vline(xintercept = x6[c(82,max_6)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x6[69], linetype = "dashed", color = "red")

diffplot_6_inflpoints

x6[c(82,110)]

# MCR5105 + HKE3402: USING THE ORIGINAL PLOT!!

y6.1 <- df$`MCR5105 + HKE3402_GFP`
x6.1 <- df$`MCR5105 + HKE3402_OD`

max_6.1 <- match(max(y6.1),y6.1)

infl_6.1 <- c(FALSE, diff(diff(y6.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_6.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_6.1_indx <- which(infl_6.1==TRUE)

infl_6_df <- data.frame(col1 = c(y6.1[infl_6.1_indx]), col2 = c(x6.1[infl_6.1_indx]))

highlight_df_6 <- data.frame(col3 = c(x6.1[c(83,111,max_6.1)]),
                             col4 = c(y6.1[c(83,111,max_6.1)]))

plot_6_inflpoints <- plot6 +
  geom_point(data = infl_6_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_6,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x6[c(82,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x6.1[c(70,max_6.1)], linetype = "dashed", color = "red")

plot_6_inflpoints


#### 7) MCR5101 + MCS5205: #### 

max_7 <- match(max(smooth_7),smooth_7)

infl_7 <- c(FALSE, diff(diff(smooth_7)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_7==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_7_inflpoints <- diffplot_7_smooth +
  geom_point(aes(x = x7[infl_7], y = smooth_7[infl_7]), color = "blue") +
  geom_vline(xintercept = x7[c(111,max_7)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x7[75], linetype = "dashed", color = "red")

diffplot_7_inflpoints

x7[c(111,123)]

# MCR5101 + MCS5205: USING THE ORIGINAL PLOT!!

y7.1 <- df$`MCR5101 + MCS5205_GFP`
x7.1 <- df$`MCR5101 + MCS5205_OD`

max_7.1 <- match(max(y7.1),y7.1)

infl_7.1 <- c(FALSE, diff(diff(y7.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_7.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_7.1_indx <- which(infl_7.1==TRUE)

infl_7_df <- data.frame(col1 = c(y7.1[infl_7.1_indx]), col2 = c(x7.1[infl_7.1_indx]))

highlight_df_7 <- data.frame(col3 = c(x7.1[c(112,124,max_7.1)]),
                             col4 = c(y7.1[c(112,124,max_7.1)]))

plot_7_inflpoints <- plot7 +
  geom_point(data = infl_7_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_7,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x7[c(111,123)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x7.1[c(76,max_7.1)], linetype = "dashed", color = "red")

plot_7_inflpoints


#### 8) MCR5114 + HKE3402: #### 

max_8 <- match(max(smooth_8),smooth_8)

infl_8 <- c(FALSE, diff(diff(smooth_8)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_8==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_8_inflpoints <- diffplot_8_smooth +
  geom_point(aes(x = x8[infl_8], y = smooth_8[infl_8]), color = "blue") +
  geom_vline(xintercept = x8[c(84,max_8)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x8[74], linetype = "dashed", color = "red")

diffplot_8_inflpoints

x8[c(84,110)]

# MCR5114 + HKE3402: USING THE ORIGINAL PLOT!!

y8.1 <- df$`MCR5114 + HKE3402_GFP`
x8.1 <- df$`MCR5114 + HKE3402_OD`

max_8.1 <- match(max(y8.1),y8.1)

infl_8.1 <- c(FALSE, diff(diff(y8.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl_8.1==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_8.1_indx <- which(infl_8.1==TRUE)

infl_8_df <- data.frame(col1 = c(y8.1[infl_8.1_indx]), col2 = c(x8.1[infl_8.1_indx]))

highlight_df_8 <- data.frame(col3 = c(x8.1[c(85,111,max_8.1)]),
                             col4 = c(y8.1[c(85,111,max_8.1)]))

plot_8_inflpoints <- plot8 +
  geom_point(data = infl_8_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_8,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x8[c(84,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x8.1[c(75,max_8.1)], linetype = "dashed", color = "red")

plot_8_inflpoints


#### MCR5101 + M15: #### 

max_M15 <- match(max(smooth_M15),smooth_M15)

infl <- c(FALSE, diff(diff(smooth_M15)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_M15_inflpoints <- diffplot_M15_smooth +
  geom_point(aes(x = x_15[infl], y = smooth_M15[infl]), color = "blue") +
  #geom_vline(xintercept = x_15[infl][51], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_15[max_M15], linetype = "dashed", color = "red")

diffplot_M15_inflpoints

# MCR5101 + M15: USING THE ORIGINAL PLOT!!

y_15.1 <- df$`MCR5101 + M15_GFP`
x_15.1 <- df$`MCR5101 + M15_OD`

max_M15.1 <- match(max(y_15.1),y_15.1)

infl <- c(FALSE, diff(diff(y_15.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_indx <- which(infl==TRUE)

infl_M15_df <- data.frame(col1 = c(y_15.1[infl_indx]), col2 = c(x_15.1[infl_indx]))

highlight_df_M15 <- data.frame(col3 = x_15.1[c(max_M15 + 1, max_M15.1)],
                               col4 = y_15.1[c(max_M15 + 1, max_M15.1)])

plot_M15_inflpoints <- plot_M15 +
  geom_point(data = infl_M15_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_M15,aes(x = col3, y = col4), color = "green") +
  #geom_vline(xintercept = x_15[c(84,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_15.1[max_M15.1], linetype = "dashed", color = "red")

plot_M15_inflpoints


#### MCR5101 + M21: #### 

max_M21 <- match(max(smooth_M21),smooth_M21)

infl <- c(FALSE, diff(diff(smooth_M21)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_M21_inflpoints <- diffplot_M21_smooth +
  geom_point(aes(x = x_21[infl], y = smooth_M21[infl]), color = "blue") +
  #geom_vline(xintercept = x_21[infl][51], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_21[max_M21], linetype = "dashed", color = "red")

diffplot_M21_inflpoints

# MCR5101 + M21: USING THE ORIGINAL PLOT!!

y_21.1 <- df$`MCR5101 + M21_GFP`
x_21.1 <- df$`MCR5101 + M21_OD`

max_M21.1 <- match(max(y_21.1),y_21.1)

infl <- c(FALSE, diff(diff(y_21.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_indx <- which(infl==TRUE)

infl_M21_df <- data.frame(col1 = c(y_21.1[infl_indx]), col2 = c(x_21.1[infl_indx]))

highlight_df_M21 <- data.frame(col3 = x_21.1[c(max_M21 + 1, max_M21.1)],
                               col4 = y_21.1[c(max_M21 + 1, max_M21.1)])

plot_M21_inflpoints <- plot_M21 +
  geom_point(data = infl_M21_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_M21,aes(x = col3, y = col4), color = "green") +
  #geom_vline(xintercept = x_21[c(84,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_21.1[max_M21.1], linetype = "dashed", color = "red")

plot_M21_inflpoints


#### MCR5101 + M41: #### 

max_M41 <- match(max(smooth_M41),smooth_M41)

infl <- c(FALSE, diff(diff(smooth_M41)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_M41_inflpoints <- diffplot_M41_smooth +
  geom_point(aes(x = x_41[infl], y = smooth_M41[infl]), color = "blue") +
  #geom_vline(xintercept = x_41[infl][51], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_41[max_M41], linetype = "dashed", color = "red")

diffplot_M41_inflpoints

# MCR5101 + M41: USING THE ORIGINAL PLOT!!

y_41.1 <- df$`MCR5101 + M41_GFP`
x_41.1 <- df$`MCR5101 + M41_OD`

max_M41.1 <- match(max(y_41.1),y_41.1)

infl <- c(FALSE, diff(diff(y_41.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_indx <- which(infl==TRUE)

infl_M41_df <- data.frame(col1 = c(y_41.1[infl_indx]), col2 = c(x_41.1[infl_indx]))

highlight_df_M41 <- data.frame(col3 = x_41.1[c(max_M41 + 1, max_M41.1)],
                               col4 = y_41.1[c(max_M41 + 1, max_M41.1)])

plot_M41_inflpoints <- plot_M41 +
  geom_point(data = infl_M41_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_M41,aes(x = col3, y = col4), color = "green") +
  #geom_vline(xintercept = x_41[c(84,110)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_41.1[max_M41.1], linetype = "dashed", color = "red")

plot_M41_inflpoints


#### MCR5101 + M42: #### 

max_M42 <- match(max(smooth_M42),smooth_M42)

infl <- c(FALSE, diff(diff(smooth_M42)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION

diffplot_M42_inflpoints <- diffplot_M42_smooth +
  geom_point(aes(x = x_42[infl], y = smooth_M42[infl]), color = "blue") +
  geom_vline(xintercept = x_42[c(118,max_M42)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_42[84], linetype = "dashed", color = "red")

diffplot_M42_inflpoints

x_42[c(118,134)]

# MCR5101 + M42: USING THE ORIGINAL PLOT!!

y_42.1 <- df$`MCR5101 + M42_GFP`
x_42.1 <- df$`MCR5101 + M42_OD`

max_M42.1 <- match(max(y_42.1),y_42.1)

infl <- c(FALSE, diff(diff(y_42.1)>0)!=0) # PUNTOS DE INFLEXION, O PUNTOS DONDE EL CAMBIO EN "Y" CAMBIA DE SIGNO!
which(infl==TRUE) # INDICES (INDEXES) DE DICHOS PUNTOS DE INFLEXION
infl_indx <- which(infl==TRUE)

infl_M42_df <- data.frame(col1 = c(y_42.1[infl_indx]), col2 = c(x_42.1[infl_indx]))

highlight_df_M42 <- data.frame(col3 = x_42.1[c(119,135,max_M42.1)],
                               col4 = y_42.1[c(119,135,max_M42.1)])

plot_M42_inflpoints <- plot_M42 +
  geom_point(data = infl_M42_df,aes(x = col2, y = col1), color = "blue") +
  geom_point(data = highlight_df_M42,aes(x = col3, y = col4), color = "green") +
  geom_vline(xintercept = x_42[c(118,134)], linetype = "dotdash", color = "purple") +
  geom_vline(xintercept = x_42.1[c(85,max_M42.1)], linetype = "dashed", color = "red")

plot_M42_inflpoints


#### COMBINED PLOTS #### 

inflection_plot <- plot_grid(diffplot_Cneg_inflpoints,plot_Cneg_inflpoints,
                             diffplot_Cpos_inflpoints,plot_Cpos_inflpoints,
                             diffplot_1_inflpoints,plot_1_inflpoints,
                             diffplot_2_inflpoints,plot_2_inflpoints,
                             diffplot_3_inflpoints,plot_3_inflpoints,
                             diffplot_4_inflpoints,plot_4_inflpoints,
                             diffplot_5_inflpoints,plot_5_inflpoints,
                             diffplot_6_inflpoints,plot_6_inflpoints,
                             diffplot_7_inflpoints,plot_7_inflpoints,
                             diffplot_8_inflpoints,plot_8_inflpoints,
                             labels = c("A","A1","B","B1","C","C1","D","D1",
                                        "E","E1","F","F1","G","G1","H","H1",
                                        "I","I1","J","J1"),ncol = 4, nrow = 5)

inflection_plot


inflection_plot1 <- plot_grid(diffplot_Cneg_inflpoints,plot_Cneg_inflpoints,
                             diffplot_Cpos_inflpoints,plot_Cpos_inflpoints,
                             diffplot_1_inflpoints,plot_1_inflpoints,
                             labels = c("A","A1","B","B1","C","C1"),
                             ncol = 2, nrow = 3)

inflection_plot1

inflection_plot2 <- plot_grid(diffplot_2_inflpoints,plot_2_inflpoints,
                             diffplot_3_inflpoints,plot_3_inflpoints,
                             labels = c("D","D1","E","E1"),
                             ncol = 2, nrow = 2)

inflection_plot2


inflection_plot3 <- plot_grid(diffplot_4_inflpoints,plot_4_inflpoints,
                              diffplot_5_inflpoints,plot_5_inflpoints,
                              diffplot_6_inflpoints,plot_6_inflpoints,
                              labels = c("F","F1","G","G1","H","H1"),
                              ncol = 2, nrow = 3)

inflection_plot3


inflection_plot4 <- plot_grid(diffplot_7_inflpoints,plot_7_inflpoints,
                              diffplot_8_inflpoints,plot_8_inflpoints,
                              labels = c("I","I1","J","J1"),ncol = 2, nrow = 2)

inflection_plot4


###############################################################################
## 6 - Calculating ∇ OD values
## ∇ OD1 (A)
## ∇ OD2 (B)
## ∇ OD3 (C)
###############################################################################

OD_values_neg <- highlight_df_neg$col3

A_neg <- OD_values_neg[1] - 1
A_neg
B_neg <- OD_values_neg[2] - OD_values_neg[1]
B_neg
C_neg <- OD_values_neg[3] - OD_values_neg[2]
C_neg


OD_values_pos <- highlight_df_pos$col3

A_pos <- OD_values_pos[1] - 1
A_pos
B_pos <- OD_values_pos[2] - OD_values_pos[1]
B_pos
C_pos <- OD_values_pos[3] - OD_values_pos[2]
C_pos


OD_values_1 <- highlight_df_1$col3

A_1 <- OD_values_1[1] - 1
A_1
B_1 <- OD_values_1[2] - OD_values_1[1]
B_1
C_1 <- OD_values_1[3] - OD_values_1[2]
C_1


OD_values_2 <- highlight_df_2$col3

A_2 <- OD_values_2[1] - 1
A_2
B_2 <- OD_values_2[2] - OD_values_2[1]
B_2
C_2 <- OD_values_2[3] - OD_values_2[2]
C_2


OD_values_3 <- highlight_df_3$col3

A_3 <- OD_values_3[1] - 1
A_3
B_3 <- OD_values_3[2] - OD_values_3[1]
B_3
C_3 <- OD_values_3[3] - OD_values_3[2]
C_3


OD_values_4 <- highlight_df_4$col3

A_4 <- OD_values_4[1] - 1
A_4
B_4 <- OD_values_4[2] - OD_values_4[1]
B_4
C_4 <- OD_values_4[3] - OD_values_4[2]
C_4


OD_values_5 <- highlight_df_5$col3

A_5 <- OD_values_5[1] - 1
A_5
B_5 <- OD_values_5[2] - OD_values_5[1]
B_5
C_5 <- OD_values_5[3] - OD_values_5[2]
C_5


OD_values_6 <- highlight_df_6$col3

A_6 <- OD_values_6[1] - 1
A_6
B_6 <- OD_values_6[2] - OD_values_6[1]
B_6
C_6 <- OD_values_6[3] - OD_values_6[2]
C_6


OD_values_7 <- highlight_df_7$col3

A_7 <- OD_values_7[1] - 1
A_7
B_7 <- OD_values_7[2] - OD_values_7[1]
B_7
C_7 <- OD_values_7[3] - OD_values_7[2]
C_7


OD_values_8 <- highlight_df_8$col3

A_8 <- OD_values_8[1] - 1
A_8
B_8 <- OD_values_8[2] - OD_values_8[1]
B_8
C_8 <- OD_values_8[3] - OD_values_8[2]
C_8


##### Classifying the different combinations based on ∇ OD values #####

Diff_OD_values_df <- data.frame(A = c(A_neg,A_pos,A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8),
                                B = c(B_neg,B_pos,B_1,B_2,B_3,B_4,B_5,B_6,B_7,B_8),
                                C = c(C_neg,C_pos,C_1,C_2,C_3,C_4,C_5,C_6,C_7,C_8),
                                row.names = c("neg","pos","1","2","3","4","5","6","7","8"))

Diff_OD_values_df$delayed <- Diff_OD_values_df$A[Diff_OD_values_df$A > A_pos]

table(ifelse(Diff_OD_values_df$A<=A_pos, "Less or equal to C+", "Greater than C+"))

which(Diff_OD_values_df$A > A_pos) -> delayed_indexes
which(Diff_OD_values_df$A < A_pos) -> advanced_indexes
which(Diff_OD_values_df$B > B_pos) -> milder_indexes
which(Diff_OD_values_df$B < B_pos) -> steeper_indexes

combinations <- c("MCR5101 + OFL5101 (C-)","MCR5101 + HKE3402 (C+)",
                  "MCR5101 + OFL3201 (1)","OFL5101 + HKE3402 (2)",
                  "OFL5101 + OFL3201 (3)","OFL5101 + BY_FLO11- (4)",
                  "MCR5101 + BY_FLO11- (5)","MCR5105 + HKE3402 (6)",
                  "MCR5101 + MCS5205 (7)","MCR5114 + HKE3402 (8)")

delayed_combinations <- combinations[delayed_indexes]
delayed_combinations

advanced_combinations <- combinations[advanced_indexes]
advanced_combinations

milder_combinations <- combinations[milder_indexes]
milder_combinations

steeper_combinations <- combinations[steeper_indexes]
steeper_combinations


#### Plotting phenotypes vs. Steepness (Δ OD values) --> B

phenotypes_vs_Steepness_df <- data.frame(Combinations = combinations,
                                         Steepness = c(B_neg,B_pos,B_1,B_2,B_3,B_4,B_5,B_6,B_7,B_8))

phenotypes_vs_Steepness_df <- phenotypes_vs_Steepness_df[1:10,]

phenotypes_vs_Steepness_df

phenotypes_vs_Steepness_plot <- ggplot(phenotypes_vs_Steepness_df,aes(x= reorder(Combinations,Steepness),Steepness)) + 
  geom_col() +
  labs(title = "Steepness (ΔOD2) vs. Phenotypes") +
  xlab("Phenotypes (combinations)") +
  ylab("ΔOD2 (Steepness)") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 12), axis.text.x = element_text(size = 8, angle = 60, vjust = 1, hjust=1))

phenotypes_vs_Steepness_plot

#### Plotting phenotypes vs. OD1 (Δ OD values) --> A

phenotypes_vs_OD1_df <- data.frame(Combinations = combinations,
                                   OD1 = c(A_neg,A_pos,A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8))

phenotypes_vs_OD1_df <- phenotypes_vs_OD1_df[1:10,]

phenotypes_vs_OD1_df

phenotypes_vs_OD1_plot <- ggplot(phenotypes_vs_OD1_df,aes(x= reorder(Combinations,OD1),OD1)) + 
  geom_col() +
  labs(title = "ΔOD1 vs. Phenotypes") +
  xlab("Phenotypes (combinations)") +
  ylab("ΔOD1") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1),limits = c(0,0.35))+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 12), axis.text.x = element_text(size = 8, angle = 60, vjust = 1, hjust=1))

phenotypes_vs_OD1_plot

##Plotting both graphs together:

plot_grid(phenotypes_vs_OD1_plot,phenotypes_vs_Steepness_plot,
          labels = c("A","B"),ncol = 2, nrow = 1)

### Final plot:

phenotypes_vs_Steepness_plot2 <- ggplot(phenotypes_vs_Steepness_df,
                                        aes(x= reorder(phenotypes_vs_OD1_df$Combinations,
                                                       phenotypes_vs_OD1_df$OD1),Steepness)) + 
  geom_col() +
  labs(title = "Steepness (ΔOD2) vs. Phenotypes") +
  xlab("Phenotypes (combinations)") +
  ylab("ΔOD2 (Steepness)") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1),limits = c(0,0.35))+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 12), axis.text.x = element_text(size = 8, angle = 60, vjust = 1, hjust=1))

phenotypes_vs_Steepness_plot2

deltaOD_finalplot <- plot_grid(phenotypes_vs_OD1_plot,phenotypes_vs_Steepness_plot2,
                               labels = c("A","B"),ncol = 2, nrow = 1)

deltaOD_finalplot


## Another approach (both together)


colnames(phenotypes_vs_OD1_df)[2] <- "delta_OD"
colnames(phenotypes_vs_Steepness_df)[2] <- "delta_OD"

good_order <- c("MCR5114 + HKE3402 (8)","MCR5105 + HKE3402 (6)","OFL5101 + HKE3402 (2)",
                "MCR5101 + HKE3402 (C+)","OFL5101 + BY_FLO11- (4)","MCR5101 + OFL5101 (C-)",
                "OFL5101 + OFL3201 (3)","MCR5101 + BY_FLO11- (5)","MCR5101 + OFL3201 (1)",
                "MCR5101 + MCS5205 (7)")

df_deltaOD <- bind_rows("ΔOD1" = phenotypes_vs_OD1_df, "ΔOD2" = phenotypes_vs_Steepness_df, .id = "Groups")

df_deltaOD %>% 
  ggplot(aes(x=factor(Combinations, level = good_order), y = delta_OD, fill = Groups)) + 
  geom_col(position = position_dodge()) +
  labs(title = "ΔOD1 and ΔOD2 vs. Phenotypes (combinations)") +
  xlab("Phenotypes (combinations)") +
  ylab("ΔOD") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1),limits = c(0,0.35))+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        axis.title = element_text(size = 12), axis.text.x = element_text(size = 8, angle = 60, vjust = 1, hjust=1))

