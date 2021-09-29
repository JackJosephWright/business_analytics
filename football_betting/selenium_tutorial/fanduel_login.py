
from pff_get_table import *
from selenium import webdriver

from selenium.webdriver.common.keys import Keys
import time
from selenium.webdriver.support.ui import Select
import numpy as np
import pandas as pd
driver = webdriver.Firefox()

#navigate to page

driver.get('https://sportsbook.fanduel.com/navigation/nfl')


#this tag is for the teams ONLY
element = driver.find_elements_by_class_name('ii')
for i in range(len(element)):
    print(element[i].text)

#opening the arrow tabs on the game page
element = driver.find_elements_by_class_name('ku')
len(element)

for i in range(len(element)):
    element[i].click()

#opening sub dropdowns in tabs

element = driver.find_elements_by_xpath("//span[text()='Show more']")
for i in range(len(element)):
    element[i].click()

#get titles of props

element = driver.find_elements_by_class_name('kr')
len(element)
prop_list=[]
for i in range(len(element)):
    prop_list.append(element[i].text)
prop_list

#get names of players in props

element = driver.find_element_by_class_name('aj')
print(element.text)

all_char = list(element.text.split("\n"))
print(all_char)


## split this list by the elements in 'prop_list' so we can then turn those lists into dataframes


for i in prop_list:
    print(i)
    j="NULL"
    temp_list=[i]
    while i !=j:

