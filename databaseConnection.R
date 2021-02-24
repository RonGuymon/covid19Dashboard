##### CONNECTING TO A DATABASE #######
# Ideally, you should keep the passwords in a separate file, create environmental
# variables, or encrypt them. This is publicly available data, so I'm not too
# worried about it. This is more of an example.
con <- dbConnect(RMySQL::MySQL()
                 , user = 'gies2021'
                 , password =  '$hdU8_z[uqi'
                 , host = 'imba.ccgqvhj5h01w.us-east-1.rds.amazonaws.com' 
                 , dbname = 'dashboardData'
)