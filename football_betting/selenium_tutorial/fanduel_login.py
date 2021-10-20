
from pff_get_table import *
from selenium import webdriver

from selenium.webdriver.common.keys import Keys
import time
from selenium.webdriver.support.ui import Select
import numpy as np
import pandas as pd
from selenium.webdriver.support.ui import WebDriverWait
driver = webdriver.Firefox()
import time
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup

#navigate to page
driver.set_window_size(1200,1200)
driver.get('https://sportsbook.fanduel.com/navigation/nfl')

###NEED TO ITERATE THROUGH EACH GAME#



#open all tabs on page
def scrape_game(link):
    #open new driver
    driver = webdriver.Firefox()
    #navigate to game page
    driver.set_window_size(1200,1200)
    driver.get(link)
    #open arrow tabs
    element = driver.find_elements_by_xpath("//*[@width = '9']")
    clicks = len(element)
    print('number of clicks to do {}'.format(clicks))
    print('clicking arrows')
    for i in range(clicks):
        print('clicking arrow_{}'.format(i))
        time.sleep(1)
        element = driver.find_elements_by_xpath("//*[@width = '9']")
        try:
            element[i].click()
        except IndexError:
            print('reached end of list')
            

        time.sleep(1)


    #click show more on every tab
    element = driver.find_elements_by_xpath("//span[text()='Show more']")
    clicks = len(element)
    print('number of clicks to do {}'.format(clicks))
    print('clicking show more tabs')
    for i in range(clicks):
        print('show more_{}'.format(i))
        time.sleep(1)
        element = driver.find_elements_by_xpath("//span[text()='Show more']")
        try:
            element[i].click()
        except IndexError:
            print('reached end of list')
            
        time.sleep(1)
        

        
    content = driver.page_source

    soup = BeautifulSoup(content)

    i = soup.find_all('span')
    output=[]
    for p in i:
        output.append(p.text)


    textfile = open("game_name_1.txt", "w")
    for element in output:
        textfile.write(element + "\n")
    textfile.close()
    #driver.close()
link=link
scrape_game(link)