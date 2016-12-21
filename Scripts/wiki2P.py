import requests, bs4, re

# res = requests.get("https://en.wikipedia.org/wiki/Special:Random")
res = requests.get("https://en.wikipedia.org/wiki/Linux")


soup = bs4.BeautifulSoup(res.text, "html.parser")


element = soup.select("#mw-content-text a[title]")


pattern = re.compile("^\/.*")


for i in element:
	if "Edit section" not in i["title"] and pattern.match(i["href"]):
		if "div" not in str(i.parent) and "th" not in str(i.parent) and "td" not in str(i.parent):
			try:
				i["class"]
			except KeyError:
				print(i)
				break


