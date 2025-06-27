import requests
import json
import os
import time


def fetch_monthly_archive(api_key, year, month):
    """
    Fetch NYT archive articles for a given year and month, and save as JSON.
    """
    url = f"https://api.nytimes.com/svc/archive/v1/{year}/{month}.json"
    params = {"api-key": api_key}

    print(f"Fetching {year}-{month:02d}...")
    response = requests.get(url, params=params)

    if response.status_code != 200:
        raise Exception(f"Failed to fetch {year}-{month:02d}: {response.status_code} - {response.text}")

    articles = response.json()['response']['docs']

    # os.makedirs(save_dir, exist_ok=True)
    print(f"There're {len(articles)} articles in {year}/{month}")
    return articles


def fetch_multiple_months(api_key, year_month_pairs):
    """
    Fetch NYT archives for multiple (year, month) pairs.
    """
    all_articles = []
    for year, month in year_month_pairs:
        try:
            articles = fetch_monthly_archive(api_key, year, month)
            all_articles.extend(articles)
            time.sleep(1)  # avoid hitting rate limits
        except Exception as e:
            print(f"Error fetching {year}-{month:02d}: {e}")

    file_path = f'02_nyt_jan_mar_2024.json'
    with open(file_path, "w") as f:
        json.dump(all_articles, f, indent=2)

    print(f"Saved {len(all_articles)} articles to {file_path}")

    return all_articles


if __name__ == "__main__":
    # setting directory
    os.chdir("../..")
    os.chdir("data/raw")

    NYT_API_KEY = "WVjuDGBibETjxDaW2iLJKSIFRFiFN3pf"

    # Specify the year and month pairs you want to download
    date_range = [(2024, 1), (2024, 2), (2024, 3)]

    articles = fetch_multiple_months(NYT_API_KEY, date_range)
    # print(f"Total articles fetched: {len(articles)}")