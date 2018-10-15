setwd("C:/Users/Lucy/Desktop/BBOXX")
data1=read.csv("collected_customer_classified_x0_20171219_v1.csv")
data=data1[,c("Customer_ID","Mean_Payment","Variance_Payment","Daily_Rate","Average_Time_B2n_Payments","Total_Number_Payments",
              "Consecutive_Late_Days", "Total_Late_To_Date", "Average_Late_Days")]
str(data)
data$Consecutive_Late_Days=as.numeric(data$Consecutive_Late_Days)
data$Daily_Rate=as.numeric(data$Daily_Rate)
data$Total_Number_Payments=as.numeric(data$Total_Number_Payments)
data$Total_Late_To_Date=as.numeric(data$Total_Late_To_Date)

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
outlierKD(data, data$Mean_Payment)
outlierKD(data, Variance_Payment)
outlierKD(data, Daily_Rate)
outlierKD(data, Average_Time_B2n_Payments)
outlierKD(data, Total_Number_Payments)


data=na.omit(data)
data_label=data
data$Customer_ID=NULL
data_std=scale(data, center = TRUE, scale = TRUE)

wss=(nrow(data_std)-1)*sum(apply(data_std,2,var))
for (i in 2:15)
  wss[i]=sum(kmeans(data_std,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "Number of Clusters",
     ylab = "Within group ss", main="Determining the Optimal Number of clusters")

fit <- kmeans(data_std, 7)
fit
fit$size
fit$withinss

library(cluster) 
clusplot(data, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
library(fpc)
plotcluster(data, fit$cluster)

new=cbind(data_label,fit$cluster)
str(data_label)
plot(new$Mean_Payment,new$Total_Number_Payments, col=fit$cluster)
plot(new$Average_Time_B2n_Payments,new$Mean_Payment, col=fit$cluster)
plot(new$Average_Late_Days,new$Mean_Payment,col=fit$cluster)

write.csv(new,"cluster_dec_labels.csv")
new2=cbind(data1,fit$cluster)
