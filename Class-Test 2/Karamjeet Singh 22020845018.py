#!/usr/bin/env python
# coding: utf-8

# # use html tags to define the header  (1)
# <h1> Welcome to the world of Programming<?h1>
# <p>PDS Test</p>

# In[ ]:





# # Explain the Object Oriented Programming concepts        (3)
# #The concepts of object oriented programming are-
# 
# #Polymorphism-It ois defined as ability of a message to be displayed in more than one form.
# 
# #Abstraction-Data Abstraction is the property by virtue of which only the essential details are displayed to the user. 
# #The trivial or non-essential units are not displayed to the user
# 
# #Encapsulation-It is defined as the wrapping up of data under a single unit.
# #It is the mechanism that binds together the code and the data it manipulates. 
# #Another way to think about encapsulation is that it is a protective shield that prevents the 
# #data from being accessed by the code outside this shield. 
# 
# #Inheritance-. It is the mechanism by which by which one class is allowed to 
# #inherit the features (fields and methods) of another class. 
# 

# In[25]:


# print factors
# input = 100, output = [1, 2, 4, 5, 10, 20, 25, 50, 100]

# Add comments wherever needed      (3)
def printfactors(n):
    i=1
    while i <= n : 
        if (n%i==0) :
            print(i,end=" ")
        i=i+1
            #driver method
print("The factors of 100 are :")
printfactors(100)
      


# In[ ]:


# Add 45 days to the list and print it as due date (3)
# use functions datetime, strptime, timedelta 
import datetime as dt
names_list =[{
    "name": "Joe",
    "issue_date":"30-12-2022"},
    {"name": "John",
    "issue_date":"15-01-2022"}]


# In[35]:



from datetime import datetime
from datetime import timedelta
names_list =[{
    "name": "Joe",
    "issue_date":"30-12-2022"},
    {"name": "John",
    "issue_date":"15-01-2022"}]

for i in names_list:
    new=datetime.strptime(i["issue_date"],"%d-%m-%Y") + timedelta(days=45)
    i["Due_date"] = str(new.date())
    print(i)


# In[ ]:





# In[ ]:




