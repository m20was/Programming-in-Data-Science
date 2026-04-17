#!/usr/bin/env python
# coding: utf-8

# # use html tags to define the header  (1)
# 
# <h1> Welcome to the class of Programming for Data Structure </h1>
# <h2> Today is the test </h2>
# <h2> Welcome class </h2>

# # Explain the Object Oriented Programming concepts        (3)
# 
# 
# <h1>inheritance- it can be defined as the process where one class acquires the properties of another.</h1>
# <h1>polymorphism- it is the ability of an object to different actions based on the context.</h1>
# <h1>encapsulation- it can be achieved by declaring all the variables in a class as private and writing private methods in the class to set and get the values of the class.</h1>
# <h1>abstraction- data abstraction is the property by virtue of which only the essential details are displayed to the user.</h1>

# In[17]:


# print factors
# input = 100, output = [1, 2, 4, 5, 10, 20, 25, 50, 100]

# Add comments wherever needed      (3)

def printFactors(n) :    #to define the number of which we want the factors
    i = 1
    while i <= n :
        if (n % i==0) :      #to check the divisibility 
            print (i,end=" ")
        i = i + 1            # it will proceed with consecutive numbers to continue in the loop
         
# Driver method
print ("The factors of 100 are: ")
printFactors(100)


# # Add 45 days to the list and print it as due date (3)
# # use functions datetime, strptime, timedelta 
# import datetime as dt
# names_list =[{
#     "name": "Joe",
#     "issue_date":"30-12-2022"},
#     {"name": "John",
#     "issue_date":"15-01-2022"}]

# In[14]:


from datetime import datetime
from datetime import timedelta

names_list =[{
    "name": "Joe",
    "issue_date":"30-12-2022"},
    {"name": "John",
    "issue_date":"15-01-2022"}]

for i in names_list:
    new = datetime.strptime(i["issue_date"], "%d-%m-%Y") + timedelta(days=45)
    i["Due_Date"] = str(new.date())
    print (i)


# In[ ]:




