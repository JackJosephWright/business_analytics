'''
selenium mostly used to write test cases. 

can write unittest cases with python unittest module
'''

import unittest
from selenium import webdriver
from selenium.webdriver.common.keys import Keys

#test case class is inherited from unittest.TestCase.

#inheriting from TestCase class is the way to tell unittest module that this is a test case
class PythonOrgSearch(unittest.TestCase):

    #setup part of initialization, this mehtod will get called before every test function to write in test case class


    def setUp(self):
        self.driver = webdriver.Firefox()
    #this is the test case method. should always start with 'test'

    
    def test_search_in_python_org(self):
        #method to create local reference to the driver object created in setUp
        driver = self.driver
        #navigate to the url, waits until page is loaded
        driver.get("http://www.python.org")
        #asserting that the title has python in it
        self.assertIn("Python", driver.title)
        #finds element by its NAME attribute, input element has name "q"
        elem = driver.find_element_by_name("q")
        #sending 'pycon' to the selected element
        elem.send_keys("pycon")
        #hits the return key
        elem.send_keys(Keys.RETURN)
        assert "No results found." not in driver.page_source

    #teardown gets called after every test method. place to do all the cleanup actions
    #quit will shut down the whole browser, close will close the tab
    def tearDown(self):
        self.driver.close()

if __name__ == "__main__":
    unittest.main()