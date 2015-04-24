library(caret)
library(dplyr)
library(lubridate)
setwd('C:\\Users\\sharan\\Documents\\GitHub\\CashCar\\data')
consumer= read.csv('consumer-survey.csv')
consumer[]

paid=read.csv('CARS_PAID_Final.csv')
paid_bkp=paid


paid=paid_bkp
attach(paid)
paid=select(paid, -dealer_name,-city,-ZIP,-address_line1,-address_line2,-new_vehicle_model,-trade_in_vehicle_drive_train,-disposal_status,
                -address_line3,-address_line4,-area_code,-vendor_id,-disposal_facility_nmvtis_id,-disposal_facility_contact_info,-trade_in_NMVTIS_flag,-phone,
                -invoice_id,-invoice_date,-invoice_num,phone,-area_code,-trade_in_VIN,-trade_in_model,-trade_in_make,-trade_in_title_state,-trade_in_odometer_reading,-trade_in_registration_end,-trade_in_registration_state,-trade_in_registration_start,-trade_in_insurance_start,-new_vehicle_VIN_trunc,-new_vehicle_make,-new_vehicle_drive_train)
paid$phone<-NULL
paid$sale_date <-as.Date(sale_date,format='%d-%b-%y')
paid$sale_month <- as.factor(month(paid$sale_date))
paid$sale_date <-NULL
str(paid)

attach(paid)
reg1 <- lm((new_vehicle_MSRP) ~ . ,select(paid,-state))
s=summary(reg1)
s
# comma_split<- function(data){
#   return(unlist(strsplit(data,','))[1])
# }
# trade_in_vehicle_drive_train=unlist(lapply((paid$trade_in_vehicle_drive_train),as.character))
# trade_in_vehicle_drive_train=lapply((trade_in_vehicle_drive_train),comma_split)
# trade_in_vehicle_drive_train[1]
cluster_cols <- select(paid,invoice_amount,trade_in_mileage,new_vehicle_car_mileage,
                  as.numeric(new_vehicle_year),as.numeric(trade_in_year),
                  new_vehicle_MSRP)
str(cluster_cols)


# ===============
# Inspect results
# ===============
cluster_cols=scale(cluster_cols)
restCluster=kmeans(cluster_cols,centers = 5)
# Size of each cluster
restCluster$size

# Cluster centers (means)
restCluster$centers

# Cluster assignments
restCluster$cluster

# Plot the clusters 
#   pch parameter controls the shape the dots
#   cex controls the size of the dots
plot(bdata.train$revenue,bdata.train$month, col = (restCluster$cluster), main = "K means clustering",
     pch = 20, cex = 2)

# =============================
# Get data for a single cluster
# =============================

cluster1.indicators <- (restCluster$cluster == 1)
print(cluster1.indicators)
cluster1.rows <- which(restCluster$cluster == 1)
print(cluster1.rows)

irisdata.cluster1 <- irisdata[cluster1.indicators, ]
print(irisdata.cluster1)
summary(irisdata.cluster1)

# ===============================================
# Determine the number of clusters (based on SSE)
# ===============================================

# Run kmeans for 1 to 20 clusters and save SSE
k <- c()
ClustSSE <- c()
for (n in 10:15) {
  restCluster <- kmeans(cluster_cols, n, nstart=10)
  sse <- restCluster$tot.withinss
  k <- append(k, n)
  ClustSSE <- append(ClustSSE, sse)
}
print(data.frame(k, ClustSSE))

# Plot SEE (inspect for "elbows")
plot(ClustSSE, type="b", xlab = "Number of clusters", ylab = "SSE")




pairs(data.train[,seq(4,10)])

