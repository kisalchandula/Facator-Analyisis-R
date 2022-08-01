library(seminr)

#load data
survey <- read.csv('E:/Final project - Research survey.csv')

#select columns
survey <- survey[,1:20]
#rename variables
names(survey) <- c('Q1','Q2','Q3','Q4','Q5','Q6',
       'Q7','Q8','Q9','Q10','Q11','Q12',
       'Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20')

#measurement model
simple_mm <- constructs(
  composite("Private_Data1", multi_items("Q", 7:11)),
  composite("Private_Data2", multi_items("Q", 12:15)),
  composite("AI_Existence", multi_items("Q", 3:5))
)

#structural model
simple_sm <- relationships(
  paths(from = c("Private_Data1", "Private_Data2"), to = "AI_Existence" )
)

#estimate model
simple_model <- estimate_pls(data = survey,
                             measurement_model = simple_mm,
                             structural_model = simple_sm,
                             missing = mean_replacement,
                             missing_value = NA)
summary(simple_model)
plot(simple_model)

#bootrapping with sub samples
boot_simple <- bootstrap_model(seminr_model = simple_model, nboot = 500, cores = 5, seed = 123)
summary(boot_simple)

th = seminr_theme_create(mm.node.fill = "yellow",
                         sm.node.fill = "lightblue",
                         mm.node.label.fontsize = 16,
                         mm.edge.label.fontsize = 14,
                         mm.edge.positive.color = "green",
                         mm.edge.negative.color = "red",
                         construct.compositeA.shape = "ellipse",
                         manifest.compositeA.shape = "ellipse",
                         sm.node.label.fontsize = 16,
                         sm.edge.label.fontsize = 15)
plot(boot_simple, title = "Structural Equation Model", theme = th) #final model

