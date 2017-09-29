# 1. Importing data
# We can choose to omit the 'header = FALSE' or not. Removing it will set a default value, i.e. FALSE.
adult_db <- read.table(file="adult.csv", header = FALSE, sep=",", 
                       na.strings=c("?","NA","-"), 
                       strip.white=TRUE, stringsAsFactors=FALSE)

# These are the attribute names for the data imported above.
names(adult_db) = c("age",
                    "workclass",
                    "fnlwgt",
                    "education",
                    "education_num",
                    "marital_status",
                    "occupation",
                    "relationship",
                    "race",
                    "sex",
                    "capital_gain",
                    "capital_loss",
                    "hours_per_week",
                    "native_country",
                    "class")

# Returns a new window with a table for the attributes defined above and their values.
fix(adult_db)

# Adults with salaries less than/equal to 50k or greater than 50k.
adult_db$class[adult_db$class==">50K"] <- 1
adult_db$class[adult_db$class=="<=50K"] <- 0

# 2. Looking for missing values in the dataset and (sum) tells me how many rows there are with missing values.
is.na(adult_db)
sum(is.na(adult_db))

# 2. Looking for missing values in all the columns in the dataset.
apply(adult_db, MARGIN=2, function(x) 
                            sum(is.na(x)))

# 2. Removing rows with missing values.
adult_db_nomiss <- na.omit(adult_db)
apply(adult_db_nomiss, MARGIN=2, function(x)
                                   sum(is.na(x)))

# 3. Selecting a small data sample of 1000 rows.
set.seed(1013)
idx <- sample(1:nrow(adult_db_nomiss), 1000)
adult_db2 <- adult_db_nomiss[idx,]
row.names(adult_db2) <- NULL

# 3a. Counting the rows to see if there are 1000 rows (instead of manually counting them).
nrow(adult_db2)

# Plots a histogram for the small data sample.
hist(adult_db2$age[adult_db2$class==0], breaks=50, main="Age Distribution", 
         xlab="Age", ylab="Frequency", col="red")
hist(adult_db2$age[adult_db2$class==1], breaks=50, main="Age Distribution", 
         xlab="Age", ylab="Frequency", col="blue", add=T)
legend("topright", legend = c(">50k","<=50k"), col = c("Blue","Red"), pch=c(16), cex=1)


# 3b. Creates a bar chart for the race attribute with the nativities on the x-axis.
table(adult_db2$race)
barplot(table(adult_db2$race), col=1:5, xlab="Race", 
        main="Race of adults")
legend(x=0.5, y=800, legend = c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black", "Other", "White"), 
       col = 1:5, pch=c(16), cex=0.75)

# 3c. Plots	a	boxplot	for	the “age”	attribute	and	lists the values that do not fit in and could	be outliers.
boxplot(adult_db2$age, col="red", ylab="Age", main="Age of adults", outpch = 21, outbg = "black")
boxplot.stats(adult_db2$age)$out
sort(boxplot.stats(adult_db2$age)$out)

# 4. Standardizing the numeric attributes only.
adult_db2_numeric <- adult_db2[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(adult_db2[,c("class")])

scaled.adult_db2_numeric <- scale(adult_db2_numeric)
colMeans(scaled.adult_db2_numeric)
apply(scaled.adult_db2_numeric, 2, sd)

# 5a. Plotting the first two principal components.
pr.out <- prcomp(adult_db2_numeric, scale=TRUE, center=TRUE)
names(pr.out)
head(pr.out$x)

principal_comp <- pr.out$x
head(principal_comp)

plot(principal_comp[,1:2], col=(as.numeric(as.factor(class_val)) + 1), pch=20, xlim=c(-8,5), ylim=c(-9, 9),
     main="First two Principal Components")
legend(x=-7, y=3, legend = c("<=50k", ">50k"), col=c(2, 3), pch=20)

# 5b. Calculating the proportion of variance and cumulative variance.
pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)

par(mfrow=c(1,2), oma=c(0,0,2,0))
plot(pve , xlab=" Principal Components ", ylab="Variance ", ylim=c(0,1) ,type='b', col = "red")
plot(cumsum(pve ), xlab=" Principal Components ", ylab ="Cumulative Variance", ylim=c(0,1), type='b', col = "red")
mtext("Proportion of variance explained by Principal Components", outer=TRUE)
par(mfrow=c(1,1))

# 5c.
# To capture at least 50% of the total variance in the dataset, I would need to
# use approx. three principal components according to the plot. 
# To capture at least 90% of the total variance in the dataset, I would need to
# use between 5 and 6 principal components, as 90% lies somewhere in-between.