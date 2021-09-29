from selenium import webdriver
from selenium.webdriver.common.keys import Keys

#instance of webdriver created
driver = webdriver.Firefox()
#driver.get navigates to page, will wait till page has fully loaded before running script
driver.get('http://www.python.org')

#assert that title has the word python in it
assert "Python" in driver.title

#find element by method

#look at 'locating elements in the tutorial for more
elem = driver.find_element_by_name('q')
#next we are sending keys, similiar to entering keys with keyboard.

#special keys can be sent using Keys class.

#first we clear the keys to be safe
elem.clear()
#then we send 'pycon' to the input field
elem.send_keys('pycon')
elem.send_keys(Keys.RETURN)
assert "no results found." not in driver.page_source
driver.close()

## example explained

'''
selenium webdriver provides all webdriver implementations


Instance of fid
'''