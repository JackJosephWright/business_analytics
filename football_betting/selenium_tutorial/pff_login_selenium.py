
from pff_get_table import get_prop_table
from selenium import webdriver

from selenium.webdriver.common.keys import Keys
import time
from selenium.webdriver.support.ui import Select
import numpy as np
import pandas as pd
driver = webdriver.Firefox()

#navigate to page

driver.get('https://www.pff.com/betting/player-props')

#navigate to sign in button
element = driver.find_element_by_class_name('g-btn')

element.send_keys(Keys.RETURN)

#enter username

email = driver.find_element_by_id('login_email')
email.send_keys('Wrighp2003@yahoo.com')

#enter password

pswd = driver.find_element_by_id('login_password')
pswd.send_keys('Trinity56')


#time sleep until you click the captcha

time.sleep(5) 

#click sign in button
button = driver.find_element_by_id('sign-in')
button.send_keys(Keys.RETURN)

#navigate to player prop betting tool

#driver.get('https://www.pff.com/betting/player-props')


game_select = driver.find_element_by_class_name('kyber-filter-dropdown__toggle')

game_select.send_keys(Keys.RETURN)

game_all = driver.find_element_by_xpath('//*[text()="All"]')
game_all.send_keys(Keys.RETURN)




test=get_prop_table()
test.tail()