##### CONNECTING TO A DATABASE #######
# Ideally, you should keep the passwords in a separate file, create environmental
# variables, or encrypt them. This is publicly available data, so I'm not too
# worried about it. This is more of an example.
con <- dbConnect(RMySQL::MySQL()
                 , user = 'JeshuaOfJozadak'
                 , password =  '(9HhG2M&$3'
                 , host = 'covid19dashboard.cut6x5kwuxm3.us-east-1.rds.amazonaws.com' 
                 , dbname = 'dashboardData'
)