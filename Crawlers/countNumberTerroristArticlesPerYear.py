"""
This Crawls the NewYorkTimes Archive looking for the work terrism
"""
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
import csv
import time

def main(start,stop):

    # url for all articles
    #URL = ['http://query.nytimes.com/search/sitesearch/#/*/from','02to','01/']
    #saveName = 'articleCount.csv'

    # url for terrism related articles
    URL = ['http://query.nytimes.com/search/sitesearch/#/terrorism/from','02to','01/allresults/1/allauthors/oldest/']
    saveName = 'test.csv'

    # make sure you have the lastest cromedriver in the same file direcroty as this python program
    driver = webdriver.Chrome()

    soup = ''

    with open(saveName,'w',newline='') as rFile:
        writer = csv.writer(rFile, delimiter=',')

        # save headings
        headings = ['Year','month','NumArticles']
        writer.writerow(headings)


        startDate = start -1
        stopDate = start

        while stopDate <= stop:
            startDate += 1
            stopDate += 1

            print('looking at the year ',str(startDate))

            for j in range(1,13):

                print('looking at month ',j)

                lookedAtEverything = False

                if j < 9:
                    startTotal = str(startDate) + str(0) + str(j)
                    endTotal = str(startDate) + str(0) + str(j+1)
                elif j ==  9:
                    startTotal = str(startDate) + str(0) + str(j)
                    endTotal = str(startDate) + str(j + 1)
                elif j == 12:
                    startTotal = str(startDate) + str('12')
                    endTotal = str(stopDate) + str('01')
                else:
                    startTotal = str(startDate) +  str(j)
                    endTotal = str(startDate) + str(j + 1)


                pageURL = URL[0] + str(startTotal) + URL[1] +  str(endTotal) + URL[2]

                # gets page source
                driver.get(pageURL)

                time.sleep(5)

                html = driver.page_source
                # converts it to a BS4 object
                soup = BeautifulSoup(html)

                # get number of articles
                try:
                    num = soup.find('div',{'id','totalResultsCount'}).text
                    num = num.strip()
                    numberArticles = num.split(' ')
                    numberArticles = numberArticles[3]
                    numberArticles =int(''.join(x for x in numberArticles if x.isdigit()))
                except Exception:
                    numberArticles = 'Unknown'



                # write to file
                list = [str(startDate),str(j),numberArticles]
                writer.writerow(list)


if __name__ == "__main__":
    main(1969,2015)
