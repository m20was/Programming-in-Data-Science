#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# use html tags to define the header  (1)


# # <h1> Hello class I am Aritra Karan -22020845008 </h1>
#     

# # Explain the Object Oriented Programming concepts        (3)

# the concepts of object oriented programming are -
# inheritence-Inheritance is the capability of one class to derive or inherit the properties from another class.
# encapsulation -Polymorphism simply means having many forms. For example, we need to determine if the given species of birds fly or not, using polymorphism we can do this using a single function.
# polymorphism-Encapsulation is one of the fundamental concepts in object-oriented programming (OOP). It describes the idea of wrapping data and the methods that work on data within one unit. 
# abstraction -It hides the unnecessary code details from the user.
# 

# # print factors
# # input = 100, output = [1, 2, 4, 5, 10, 20, 25, 50, 100]
# 
# # Add comments wherever needed      (3)

# In[27]:


#definng a function for factor calculation
def printFactors(n):
    i = 1
    while i <= n :
            if (n % i==0) :
                  print(i,end=" ")
            i = i + 1
                    
 #calling that function and giving value
print ("the factors of 100 are: ")
printFactors(100)
        


# # Add 45 days to the list and print it as due date (3)
# # use functions datetime, strptime, timedelta 
# import datetime as dt
# names_list =[{
#     "name": "Joe",
#     "issue_date":"30-12-2022"},
#     {"name": "John",
#     "issue_date":"15-01-2022"}]

# In[30]:


from datetime import datetime
from datetime import timedelta
names_list =[
    {"name" : "joe" ,
     "issue_date" : "30-12-2022"},
    {"name" : "John" ,
     "issue_date" : "15-01-2022"}]
for i in names_list:
    new = datetime.strptime(i["issue_date"], "%d-%m-%Y") + timedelta(days=45)
    i["due_date"] = str(new.date())
    print(i)


# In[ ]:





# In[ ]:




