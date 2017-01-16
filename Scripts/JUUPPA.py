import requests, bs4, re, os, sys

def getPPA(link):
    m = re.search('\/.(.*)\/\+archive\/\w*\/(.*)', link)
    if m != None:
        return m.group(1) + "/" + m.group(2)
    else:
        return "Sorry could not find PPA"

res = requests.get("https://launchpad.net/ubuntu/+ppas?name_filter=" + sys.argv[1])


soup = bs4.BeautifulSoup(res.text, "html.parser")

element = soup.find("table", {"class" : "listing"})
if element != None:
    elements = element.find("tbody").find_all("tr")

    count = len(elements)
    results = []

    print("Found", len(elements),"results:\n")
    for item in reversed(elements):
        count -= 1
        name = item.find("td").find("a").string
        PPA = getPPA(item.find("td").find("a")["href"])
        sources = item.find_all("td")[-2].string
        binaries = item.find_all("td")[-1].string

        results.append((name, PPA, sources, binaries))

        print("Option number", count)
        print("Name :", name)
        print("PPA :", PPA)
        print("Sources :", sources)
        print("Binaries :", binaries)
        print("")
        

    print("Please select PPA", 0, "to", len(results))
    index = int(input("Number: "))

    print("Adding PPA", results[index-1][1], "with command 'sudo add-apt-repository ppa:" + results[index-1][1] + "'" + str(index-1))
    os.system("sudo add-apt-repository ppa:" + results[index-1][1])

    print("Updating packages")
    os.system("sudo apt update")

    print("Installing", "grub-customizer")
    os.system("sudo apt install " + "grub-customizer")
else:
    print("No results where found for", sys.argv[1])