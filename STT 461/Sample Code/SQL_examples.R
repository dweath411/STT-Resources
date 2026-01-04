library(sqldf)

order_details <- read.csv("order_details.csv")
orders <- read.csv("orders.csv")
territories <- read.csv("territories.csv")
regions <- read.csv("regions.csv")
employee_territories <- read.csv("employee_territories.csv")
employees <- read.csv("employees.csv")
customers <- read.csv("customers.csv")
shippers <- read.csv("shippers.csv")
suppliers <- read.csv("suppliers.csv")
products <- read.csv("products.csv")
categories <- read.csv("categories.csv")

# Simplest example: make a copy of a dataset
order2 <- sqldf("SELECT * 
                FROM orders")
#(Select * means pulling from all fields from the orders)

# Next, select columns
order_some <- sqldf("SELECT orderID, customerID 
                    FROM orders")

# sort
order_sort <- sqldf("SELECT * 
                    FROM orders 
                    ORDER BY orderDate")
#can do from descending by changing using DESC after "orderDate", default is ascending
# filtering
low_ship <- sqldf("SELECT * from orders 
                  WHERE freight < 10")

# calculated fields
order_revenue <- sqldf("SELECT *, 
                       unitPrice*quantity*(1-discount) 
                       AS revenue 
                       FROM order_details ")
#First, revenue has the entire order_details data set, but it's calculated by the function above
#naming it as revenue.

# summaries
order_summary <- sqldf("SELECT count(orderID), customerID 
                       FROM orders 
                       GROUP BY customerID")

#join example
full_orders <- sqldf("SELECT * 
                     FROM orders 
                     INNER JOIN orders_details 
                     WHERE orders.orderID = orders_details.orderID")