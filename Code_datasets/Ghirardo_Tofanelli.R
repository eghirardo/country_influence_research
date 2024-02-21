## SECTION 3: DATASETS

# set the working directory to avoid having to use full paths every time (remove if the code is run from another computer)
setwd('/Users/edoardoghirardo/Desktop/Education_project')

# import the data on density of scientific articles
scientific_publication_df <- read.csv('scientific-publications-per-million.csv', header=TRUE)

# rename columns to make the dataframe neater
colnames(scientific_publication_df)[2] <- 'country_code'
colnames(scientific_publication_df)[4] <- 'scientific_publication'

# only select the 2018 data
cleaned_scientific_publication <- scientific_publication_df[scientific_publication_df['Year'] == 2018,c('country_code','scientific_publication')]
final_df <- cleaned_scientific_publication

# get rid of the original dataframes to keep the workspace clean
rm(scientific_publication_df)


# import the rest of the data, doing similar cleaning

life_expectancy <- read.csv('life-expectancy.csv', header=TRUE)
colnames(life_expectancy)[2] <- 'country_code'
colnames(life_expectancy)[4] <- 'life_expectancy'
cleaned_life_expectancy <- life_expectancy[life_expectancy['Year'] == 2018,c('country_code','life_expectancy')]
# add to the final df, keeping only countries that have all the data
final_df <- merge(final_df, cleaned_life_expectancy, by = 'country_code')

rm(life_expectancy)
rm(cleaned_life_expectancy)

reported_happiness <- read.csv('happiness-cantril-ladder.csv', header=TRUE)
colnames(reported_happiness)[2] <- 'country_code'
colnames(reported_happiness)[4] <- 'reported_happiness'
cleaned_reported_happiness <- reported_happiness[reported_happiness['Year'] == 2018,c('country_code','reported_happiness')]
final_df <- merge(final_df, cleaned_reported_happiness, by = 'country_code')
rm(reported_happiness)
rm(cleaned_reported_happiness)


urban_population <- read.csv('urban-and-rural-population.csv', header=TRUE)
colnames(urban_population)[2] <- 'country_code'
colnames(urban_population)[4] <- 'urban_tot'
colnames(urban_population)[5] <- 'rural_tot'
# for the urban population, we will consider the percentage of the population living in urban areas
urban_population$urban_proportion <- urban_population$urban_tot / (urban_population$urban_tot + urban_population$rural_tot)
cleaned_urban_population <- urban_population[urban_population['Year'] == 2018,c('country_code','urban_proportion')]
final_df <- merge(final_df, cleaned_urban_population, by = 'country_code')
rm(urban_population)
rm(cleaned_urban_population)

internet_usage <- read.csv('share-of-individuals-using-the-internet.csv', header=TRUE)
colnames(internet_usage)[2] <- 'country_code'
colnames(internet_usage)[4] <- 'internet_usage'
cleaned_internet_usage <- internet_usage[internet_usage['Year'] == 2018,c('country_code','internet_usage')]
final_df <- merge(final_df, cleaned_internet_usage, by = 'country_code')
rm(internet_usage)
rm(cleaned_internet_usage)


education_expenditure <- read.csv('total-government-expenditure-on-education-gdp.csv', header=TRUE)
colnames(education_expenditure)[2] <- 'country_code'
colnames(education_expenditure)[4] <- 'education_expenditure'
cleaned_education_expenditure <- education_expenditure[education_expenditure['Year'] == 2018,c('country_code','education_expenditure')]
final_df <- merge(final_df, cleaned_education_expenditure, by = 'country_code')
rm(education_expenditure)
rm(cleaned_education_expenditure)

math_data <- read.csv('pisa-test-score-mean-performance-on-the-mathematics-scale.csv', header=TRUE)
colnames(math_data)[2] <- 'country_code'
colnames(math_data)[4] <- 'math_score'
cleaned_math_data <- math_data[math_data['Year'] == 2018,c('country_code','math_score')]
final_df <- merge(final_df, cleaned_math_data, by = 'country_code')
rm(math_data)
rm(cleaned_math_data)

# now, attach the final dataframe to make it easier to work with
attach(final_df)



# SECTION 4: VISUALIZING THE DATA

plot(life_expectancy, scientific_publication)
plot(reported_happiness, scientific_publication)
plot(urban_proportion, scientific_publication)
plot(internet_usage, scientific_publication)
plot(education_expenditure, scientific_publication)
plot(math_score, scientific_publication)

# from the plot, the internet_usage and math_score seem to resemble qualitatively an exponential, so we will add them to the linear regression
exp_math = exp(math_score)
exp_internet = exp(internet_usage)
life_square = life_expectancy^2



# SECTION 5: MULTIVARIATE LINEAR REGRESSION

regression <- lm(scientific_publication ~ math_score+exp_math+life_expectancy+life_square+internet_usage+exp_internet+education_expenditure+reported_happiness+urban_proportion, data=final_df)
summary(regression)


# we notice that some covariates have p-values higher than 0.05. we try to remove them one at a time (step-down method)
regression <- lm(scientific_publication ~ math_score+exp_math+life_expectancy+life_square+internet_usage+education_expenditure+reported_happiness+urban_proportion, data=final_df)
summary(regression)

regression <- lm(scientific_publication ~ math_score+exp_math+life_expectancy+life_square+education_expenditure+reported_happiness+urban_proportion, data=final_df)
summary(regression)

regression <- lm(scientific_publication ~ math_score+exp_math+life_expectancy+life_square+education_expenditure+reported_happiness, data=final_df)
summary(regression)

regression <- lm(scientific_publication ~ math_score+exp_math+life_expectancy+life_square+reported_happiness, data=final_df)
summary(regression)

regression <- lm(scientific_publication ~ math_score+exp_math+life_square+reported_happiness, data=final_df)
summary(regression)

# now all of the covariates have a p-value lower than 0.05. additionally, we have an adjusted R^2 which is very close, so we prefer the simpler model
# plot(regression)

# plot the predicted values against the actual values
prediction <- predict(regression, final_df)
plot(prediction, scientific_publication)
abline(a=0,b=1)

detach(final_df)



# SECTION 6: TESTING THE HYPOTHESIS

# import a csv file of developed countries, divide the countries between developed and developing
developed_economies = read.csv('developed_countries.csv', header=TRUE)
colnames(developed_economies)[2] <- 'country_code'
developed_codes <- developed_economies$country_code

# divide the countries among developed and developing
scientific_publication_developed <- cleaned_scientific_publication[cleaned_scientific_publication$country_code %in% developed_codes, ]
scientific_publication_developing <- cleaned_scientific_publication[!(cleaned_scientific_publication$country_code %in% developed_codes), ]

developed_data <- scientific_publication_developed$scientific_publication
developing_data <- scientific_publication_developing$scientific_publication

# we will now do a qualitative check of normality
hist(developed_data)
qqnorm(developed_data)

hist(developing_data)
qqnorm(developing_data)

# we check to see if the standard deviations are the same
sd(developed_data)
sd(developing_data)

# we notice that the standard deviations are very different, so we will not assume them to be equal in the test
t.test(developed_data, developing_data, var.equal=FALSE, alternative = 'greater')

# we conclude that developed countries have higher scientific output

# SECTION 7: LIMITATIONS OF THE STUDY

#we first check that the errors of our linear regression are normally distributed

ks.test(residuals(regression), "pnorm", mean= mean(residuals(regression)), sd = sd(residuals(regression)))
qqnorm(residuals(regression))
qqline(residuals(regression))

# we check qualitatively that the variance is constant
plot(fitted(regression), residuals((regression)))

#the output of the test tells us that there is not enough statistical evidence against the normality of our residuals. 
#the first limitation of our test will then be to assume the normality of such errors

#we then check if the the two data that we used for the hypothesis testing are actually normally distributed

ks.test(developed_data, "pnorm", mean= mean(developed_data), sd = sd(developed_data))
ks.test(developing_data, "pnorm", mean= mean(developing_data), sd = sd(developing_data))

qqnorm(developed_data)
qqline(developed_data)
qqnorm(developing_data)
qqline(developing_data)

#we conclude that for sure developing_data is not normally distributed, while we don't have enough evidence against the normality of developed_data
#so the second limitation of our test is the normality of this two data.
