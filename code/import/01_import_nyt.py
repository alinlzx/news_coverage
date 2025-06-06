import requests
import json
import os

def fetch_nyt_archive(api_key, year, month, save_to_file=True):
    """
    Fetch NYT archive articles for a specified year and month.

    Parameters:
        api_key (str): Your NYT API key.
        year (int): The year (e.g. 2022).
        month (int): The month (1-12).
        save_to_file (bool): If True, saves results to JSON file.

    Returns:
        list: A list of article metadata dictionaries.
    """
    url = f'https://api.nytimes.com/svc/archive/v1/{year}/{month}.json'
    params = {'api-key': api_key}

    print(f"Fetching NYT archive for {year}-{month:02d}...")
    response = requests.get(url, params=params)

    if response.status_code != 200:
        raise Exception(f"Failed to fetch data: {response.status_code} - {response.text}")

    data = response.json()
    articles = data['response']['docs']

    if save_to_file:
        filename = f'data/mst/nyt_archive_{year}_{month:02d}.json'
        with open(filename, 'w') as f:
            json.dump(articles, f, indent=2)
        print(f"Saved {len(articles)} articles to {filename}")

    return articles


# === Example Usage ===
if __name__ == "__main__":
    # Replace with your own NYT API key
    NYT_API_KEY = 'WVjuDGBibETjxDaW2iLJKSIFRFiFN3pf'

    # Specify year and month
    YEAR = 2023
    MONTH = 5  # May

    # setting directory
    os.chdir("../..")
    current_directory = os.getcwd()
    print("Current directory:", current_directory)

    articles = fetch_nyt_archive(NYT_API_KEY, YEAR, MONTH)