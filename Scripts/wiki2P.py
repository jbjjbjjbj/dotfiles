import requests, bs4

#res = requests.get("https://en.wikipedia.org/wiki/Special:Random")
res = requests.get("https://en.wikipedia.org/wiki/Study")


soup = bs4.BeautifulSoup(res.text)


element = soup.select("p > a")

print(element[0])

#while( soup.select(".firstHeading")[0] != "Philosophy"):
#	
#	print(soup.select(".firstHeading")[0].text)
