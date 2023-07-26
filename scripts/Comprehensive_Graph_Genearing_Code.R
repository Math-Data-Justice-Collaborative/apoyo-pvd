library(tidyverse)
library(readxl)
library(png)
library(patchwork)

#First we get the data.
#The data comes from reportcard.ride.ri.gov
#Website: https://reportcard.ride.ri.gov/ 
# Select “CustomView/Download Data: Select Datasets” and hit “Go”
# Select year and district level
#Click on “download button” from corner of “assessments” block

#First get the state data. Select the tab labeled "State Performance".
#Look only at the rows with GroupName = "All" and GroupName = "ELL". 
#For each exam topic (ELA, Math, Science) and for each score (1, 2, 3, 4),  
#and for the total count, subtract the number of ELL from the number for ALL.
#This gives you the number for non-ELL.

#For each exam subject, and for both non-ELL and ELL, have Excel calculate
#the number of 3s and 4s, divided by the total count. Convert to a percentage.
#This gives you the percentage of those who met or exceeded state standards.
#For ELL and NonELL, put the percentage for meets/exceeds for the ELA, Math, and Science subjects as indicated:
ELL_ELA <- #insert percentage
NonELL_ELA <- #insert percentage
ELL_Math <- #insert percentage
NonELL_Math <- #insert percentage
ELL_Science <- #insert percentage
NonELL_Science <- #insert percentage

#We put the state results into a vector:

state <- c(ELL_ELA, NonELL_ELA, ELL_Math, NonELL_Math, ELL_Science, NonELL_Science)

#While we are at it, let's set up a few other things we'll need.
#First, we want the labels for ELL and NonELL students in the state data.
StudentType <- c("Statewide ELL", "Statewide NonELL")

#We also will want the icon for schools with bilingual programs.
es <- readPNG("ES.png")

#Finally, we make a vector containing the school codes for schools with bilingual education
ell <- c(26601, 69601, 28103, 28134, 28153, 28157, 28144)

#26601 is International Charter School
#69601 is Nuestro Mundo Charter School
#28103 is Leviton Dual Language School which has dual language programs
#28134 is Frank D. Spaziano El School which has dual language programs
#28153 is William D'Abate El School which has developmental bilingual programs
#28157 is Lillian Feinstein El School which has transitional bilingual programs
#28144 is Gilbert Stuart Middle School, has "dual language immersion"

#Now we get the school-by-school results

#Pull out the sheet labeled "School Performance" as a separate spreadsheet, title it "RI_scores".
#Change the column heads "1", "2", etc. to "Level_1", "Level_2", etc. This will avoid problems later.
#Import it.

RI_scores <- read_excel("RI_scores.xlsx")

#Make a column with the percentage of meets and exceeds in each categorty (which means students scored 3 or 4), then delete the columns with individual levels
RI_scores <- RI_scores %>% mutate(Percent_M_E = (Level3 + Level4)/TotalCount) %>% select(-c("Level1", "Level2", "Level3", "Level4"))

#We also don't need "Group Type" since we have "Group Name"
RI_scores <- RI_scores %>% select(-c("GroupType"))

#Now we only group the data for the PVD and charter districts available to students in Providence (had we not already done the PVD district, we would include 28 in the districts vector)
districts <- c(28, 41, 48, 68, 53, 52, 69, 51, 43, 55, 59, 58, 83, 61, 54, 81, 7, 63, 42, 62, 64)
PVD_scores <- RI_scores %>% filter(DistCode %in% districts)

#NAs mean there were no students that met or exceeded standards, so replace those with 0
PVD_scores[is.na(charter_scores_2223)] <- 0

#We also only want (for now) all students and ELL students - then we need to change to non-ELL and ELL
PVD_scores <- PVD_scores %>% filter(GroupName == "All" | GroupName == "ELL")

#The comparison we really want to do is non-ELL vs ELL, as opposed to All vs. ELL.
#We accomplish this in two steps. First we crate a function that , given a school code, creates a df with ELL, non-ELL scores and totals.
#Second, we bind those dfs together.

#The advantage of the separate dfs for each school is that we can control the row numbers to perform the needed operations

#Not all schools tested Science, so the function first has to determine if the school had science test scores or not. Then does its thang
new_row <- function(code){
  df = PVD_scores %>% filter(SchCode == code)
  if ("Science" %in% df[3,]){      #some schools didn’t assess science
    non_T_ELA = df[1,7]-df[4,7]   #extracting the df makes this works
    non_T_Math = df[2,7] - df[5,7]
    non_T_Sci = df[3,7] - df[6,7]
    non_ELA = df[1,8] - df[4,8]
    non_Math = df[2,8] - df[5,8]
    non_Sci = df[3,8] - df[6,8]
    non_row_ELA <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "ELA", TotalCount = non_T_ELA, M_E = non_ELA)
    non_row_Math <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Math", TotalCount = non_T_Math, M_E = non_Math)
    non_row_Sci <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Science", TotalCount = non_T_Sci, M_E = non_Sci)
    df <- df %>% rbind(non_row_ELA, non_row_Math, non_row_Sci)
    df <- df %>% filter(GroupName != "All")
  }
  else {
    non_T_ELA = df[1,7]-df[3,7]
    non_T_Math = df[2,7] - df[4,7]
    non_ELA = df[1,8] - df[3,8]
    non_Math = df[2,8] - df[4,8]
    non_row_ELA <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "ELA", TotalCount = non_T_ELA, M_E = non_ELA)
    non_row_Math <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Math", TotalCount = non_T_Math, M_E = non_Math)
    df <- df %>% rbind(non_row_ELA, non_row_Math)
    df <- df %>% filter(GroupName != "All")
  }
  return(df)
}


#Next, to start building our final data frame, we initialize an empty df with the desired column names
colnames <- colnames(PVD_scores)
PVD_non_vs_ELL <- data.frame(matrix(nrow = 0, ncol = length(colnames)))
colnames(PVD_non_vs_ELL) = colnames

# Note, if the PVD_non_vs_ELL includes cases where no students took an exam
#that is, rows where TotalCount is NA or 0, there will be problems.
#To take care of this, one can create a new df where those rows are omitted:
PVD_non_vs_ELL <- PVD_non_vs_ELL[complete.cases(PVD_non_vs_ELL),]


#Because the school codes are strings, but we want to iterate through them, we create a numerical vector of school codes
schools <- PVD_scores$SchCode
schools <- as.numeric(schools)
schools <- unique(v)  #There is one entry for each test subject and group

#Make a new df using a loop - we iterate over the school codes, for each school code we apply the new_rows function to generate a temporary df with non-ELL and ELL, then add that df to the non-vs-ell df initialized earlier.
i <- 1
while (i <= length(schools)){
  df <- new_row(v[i])
  PVD_non_vs_ELL <- rbind(PVD_non_vs_ELL, df)
  i <- i+1
}

#The data is now cleaned, let's start building the graphs.
#We build the graph by first building the base graph - the bar graphs with the results.
#Then we add state data.
#We conclude by making the title and adding the icon for bilingual programs when appropriate.
#In each of these, the call will be with df = PVD_non_vs_ELL.

#Generate base graph for a given school coming from a dataframe
base_graph <- function(df, code){
  temp <- df %>% filter(SchCode == code)
  graph <- temp %>% ggplot(aes(fill=GroupName, x = TestSubject, y = Percent_M_E)) + geom_bar(position = "dodge", stat = "identity") + labs(x = "Test Subject", y = "Meets or Exceeds (%)", title = temp$SchName[1]) + theme(legend.title = element_blank())
  return(graph)
}

#Code to add state data
add_state <- function(graph){
  graph <- graph + geom_segment(aes(x = 0.55, y = state[1], xend = 1, yend = state[1], linetype = "dashed")) + geom_segment(aes(x = 1, y = state[2], xend = 1.45, yend = state[2], linetype = "solid")) + geom_segment(aes(x = 1.55, y = state[3], xend  = 2, yend=state[3]),  linetype = 2) + geom_segment(aes(x=2, y = state[4], xend = 2.45, yend=state[4]), linetype = 1) + geom_segment(aes(x = 2.55, y = state[5], xend = 3, yend = state[5]),  linetype = 2) + geom_segment(aes(x=3, y=state[6], xend = 3.45, yend=state[6]), linetype = 1) 
  graph <- graph + scale_linetype_manual(values = c("dashed", "solid"), labels = StudentType)
  return(graph)
}

#Function to generate graph with state data and icon for bilingual progams
make_graph <- function(df, code){
  graph <- base_graph(df, code)
  graph <- add_state(graph)
  if(code %in% ell)
    graph <- graph + inset_element(grid::rasterGrob(es), -0.1, 1.05, 0, 1.15)
  else
    graph <- graph
  return(graph)
}

#We conclude by creating the png file with the graph.
#I tried doing this with a loop, but it kept generating empty graphs!
#So I had to do this by iterating manually through i between 1 and length(schools)

#First we make the string that is the filename
filename <- function(df, v, i){
  temp <- df %>% filter(SchCode == v[i])
  filename <- paste("Vecina_Graphs/", temp$SchName[1], ".png", sep = "")
  return(filename)
}

#Finally, we start with i <- 1 and start generating the files one-at-a-time
#I would love it if someone could get a loop to do this properly!

i <- 1
png(filename(PVD_non_vs_ELL, schools, i))
make_graph(PVD_non_vs_ELL, schools[i])
dev.off()
i <- i+1

