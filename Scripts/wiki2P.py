import requests, bs4, re




def calculate(site):

        print("Downloading wikipedia site: " + site)


        res = requests.get("https://en.wikipedia.org" + site)

        print("Download completed analysing")

        soup = bs4.BeautifulSoup(res.text, "html.parser")


        element = soup.find("div", {"class": "mw-content-ltr"}).find("p")


        #print(element)

        elements = element.find_all("a")




        pattern = re.compile("\/wiki\/(?!File|Help).*")

        results = ""


        for i in elements:
                if pattern.match(i["href"]) :
                        results = i["href"]
                        break

        return results



siter = "/wiki/Linux"

while siter is not "philosophy":
        input(siter)
        siter = calculate(siter)
