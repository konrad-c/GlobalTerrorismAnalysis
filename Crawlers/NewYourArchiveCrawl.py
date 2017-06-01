"""
This Crawls the NewYorkTimes Archive looking for the work terrism
"""
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
import csv
import time

def main(start,stop):

    URL = ['http://query.nytimes.com/search/sitesearch/#/terrorism/from','0102to','0101/allresults/','/allauthors/oldest/']

    # make sure you have the lastest cromedriver in the same file direcroty as this python program
    driver = webdriver.Chrome()

    soup = ''

    with open('articleDatabase.csv','w',newline='') as rFile:
        writer = csv.writer(rFile, delimiter=',')

        # save headings
        headings = ['Link','Name','Month','Day','Year']
        writer.writerow(headings)


        startDate = start -1
        stopDate = start

        while stopDate <= stop:
            startDate += 1
            stopDate += 1

            print('looking at the year ',str(startDate))

            lookedAtEverything = False

            i = 0

            # go through every page (10 articles per page and 4,727,349 articles in tota)
            while not lookedAtEverything:

                # just pass if there is any errors
                try:
                    i += 1
                    pageURL = URL[0] + str(startDate) + URL[1] + str(stopDate)+ URL[2] + str(i) + URL[3]

                    # gets page source
                    driver.get(pageURL)

                    time.sleep(3)

                    html = driver.page_source
                    # converts it to a BS4 object
                    soup = BeautifulSoup(html)


                    # find each individual articles
                    articles = soup.find('ol',{'class','searchResultsList flush'})

                    articles = articles.findAll('li')
                    # go through and save each article name, date and page
                    for article in articles:

                        allInfo = article.find('div',{'class','storyMeta'})

                        # get link
                        link = article.find('a',href = True)['href']

                        #get name
                        name = allInfo.find('span',{'class','printHeadline'})
                        name = name.text[17:-1]

                        # get date
                        date = allInfo.find('span',{'class','dateline'})
                        date = date.text
                        date = date.split(' ')

                        # save to rFile
                        line = [str(link),str(name),str(date[0]),str(date[1][:2]),str(date[2])]
                        writer.writerow(line)
                except Exception as e:
                    print(e)
                    print('did not read link: ', pageURL)
                    pass


                try:
                    # check if we have looked at everything
                    check = soup.find('div',{'class','searchPagination wrap'})
                    check = check.text
                    if not 'next' in check.lower():
                        lookedAtEverything = True
                except Exception:
                    pass




if __name__ == "__main__":
    main(1969,2015)