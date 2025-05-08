import requests
import re
from bs4 import BeautifulSoup

def check_teams_in_matches():
    # Teams to search for with specific score formats
    target_teams = {
        "Blackburn Rovers": r"\bBlackburn Rovers\b",
        "Burgos": r"\bBurgos\b(?! Promesas\b)",  # Match "Burgos" but not if followed by "Promesas"
        "Almeria": r"\bAlmeria\b",
        "Torino": r"\bTorino\b"
    }
    
    # Fetch the XML feed
    url = "https://www.soccerstats247.com/DailyMatchFeed.aspx?langId=1"
    response = requests.get(url)
    if response.status_code != 200:
        print("Failed to fetch the feed")
        return
    
    # Parse the XML
    soup = BeautifulSoup(response.content, 'xml')
    latest_item = soup.find('item')
    if not latest_item:
        print("No match data found in the feed")
        return
    
    pub_date = latest_item.find('pubDate').text if latest_item.find('pubDate') else "unknown date"
    description = latest_item.find('description').text if latest_item.find('description') else ""
    
    found_teams = []
    for team_name, team_pattern in target_teams.items():
        if re.search(team_pattern, description, re.IGNORECASE):
            found_teams.append(team_name)
    
    if found_teams:
        print(f"On {pub_date}, the following teams were found in the matches:")
        for team in found_teams:
            print(f"- {team}")
    else:
        print(f"No matching teams found in the matches for {pub_date}")

if __name__ == "__main__":
    check_teams_in_matches()
