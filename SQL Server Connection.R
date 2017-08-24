# Creates appropriate connection to SQL Server instance based on operating system - Assumes either Linux or Windows
# JDBC Driver download: https://docs.microsoft.com/en-us/sql/connect/jdbc/download-microsoft-jdbc-driver-for-sql-server

require(RJDBC)

SQLConnect <<- function(DBName, UserName, Password, ConnectionName = "sqlconnection", ServerIPAddress = "127.0.0.1"){
  # Close previous connections (useful for garbage collection)
  suppressWarnings(a <- try(rm(sqlconnection, envir = .GlobalEnv), silent = TRUE))
  
  # Test OS type to find driver location - uses hardcoded locations
  if (.Platform$OS.type == "unix"){
    driver <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "/opt/jdbc/sqljdbc_6.0/enu/jre7/sqljdbc41.jar") #Purposeful driver location
  } else {
    driver <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "C:/SQLJDBC/sqljdbc_3.0/enu/sqljdbc4.jar") #Default driver location
  }
  
  # Create connection
  eval(parse(text = sprintf("assign('sqlconnection', dbConnect(driver, 'jdbc:sqlserver://%s;databaseName=%s', '%s', '%s'), envir = .GlobalEnv)", ServerIPAddress, DBName, UserName, Password)))
  
}

