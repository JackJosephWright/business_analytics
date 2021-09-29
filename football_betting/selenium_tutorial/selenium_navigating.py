from selenium import webdriver
from selenium.webdriver.common.keys import Keys



driver = webdriver.Firefox()

#navigate to the page

driver.get('http://www.google.com')

#interacting with the page

#find an html element to interact with

element = driver.find_element_by_name('q')

#send elemeont some text

element.send_keys('some text')

#simulate pressing arrow keys by using Keys class

element.send_keys(' and some', Keys.ARROW_DOWN)

#clear element

element.clear()


#filling in forms

#toggling through state of dropdown

#use setSelected to get something like an OPTION tag selected.

driver.close()
