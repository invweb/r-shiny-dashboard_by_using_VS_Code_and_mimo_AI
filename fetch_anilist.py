#!/usr/bin/env python3
import subprocess, json, csv, time, sys, os

API_URL = "https://graphql.anilist.co"
QUERY = """
query ($page: Int, $perPage: Int) {
  Page(page: $page, perPage: $perPage) {
    pageInfo { hasNextPage }
    media(type: ANIME, sort: POPULARITY_DESC) {
      id
      title { romaji english native }
      type format episodes status
      startDate { year month day }
      endDate { year month day }
      averageScore popularity favourites season genres
      studios(isMain: true) { nodes { name } }
      coverImage { large medium }
      description(asHtml: false)
      duration source
    }
  }
}
"""

WORK_DIR = "/Users/vasiliikarpenko/VS_Code_Projects/R"
csv_path = f"{WORK_DIR}/anime_data.csv"
tmp_json = "/tmp/anilist_page.json"

existing = 0
if os.path.exists(csv_path):
    with open(csv_path, "r", encoding="utf-8") as f:
        existing = sum(1 for _ in f) - 1

csv_file = open(csv_path, "a", newline="", encoding="utf-8")
writer = csv.writer(csv_file, quoting=csv.QUOTE_ALL)
if existing == 0:
    writer.writerow(["id","title","title_japanese","type","source","episodes","status","aired_from","aired_to","score","scored_by","rank","popularity","members","favorites","rating","duration","year","season","genres","studios","image_url","synopsis"])

def gql(query, variables):
    payload = json.dumps({"query": query, "variables": variables})
    result = subprocess.run(
        ["curl", "-s", "-m", "15", "--compressed", "-X", "POST", API_URL,
         "-H", "Content-Type: application/json", "-H", "User-Agent: Mozilla/5.0",
         "-d", payload, "-o", tmp_json],
        capture_output=True, timeout=20
    )
    if result.returncode != 0 or not os.path.exists(tmp_json):
        return None
    with open(tmp_json, "r", encoding="utf-8") as f:
        return json.load(f)

page = max(1, existing // 50 + 1)
per_page = 50
total = existing

while True:
    sys.stdout.write(f"\rFetching page {page} (total: {total})...")
    sys.stdout.flush()

    resp = None
    for attempt in range(5):
        resp = gql(QUERY, {"page": page, "perPage": per_page})
        try:
            if resp and resp["data"]["Page"]["media"]:
                break
        except (TypeError, KeyError):
            resp = None
        time.sleep(2 * (attempt + 1))

    try:
        if not resp or not resp["data"]["Page"]["media"]:
            print(f"\nNo data at page {page}. Stopping.")
            break
    except (TypeError, KeyError):
        print(f"\nNo data at page {page}. Stopping.")
        break

    media = resp["data"]["Page"]["media"]
    for i, m in enumerate(media):
        title = m.get("title", {}).get("romaji", "") or ""
        title_jp = m.get("title", {}).get("native", "") or ""
        anime_type = m.get("type", "") or ""
        fmt = m.get("format", "") or ""
        episodes = m.get("episodes") or ""
        status = m.get("status", "") or ""
        sd = m.get("startDate") or {}
        ed = m.get("endDate") or {}
        aired_from = f"{sd.get('year','')}-{sd.get('month',''):02d}-{sd.get('day',''):02d}" if sd.get("year") else ""
        aired_to = f"{ed.get('year','')}-{ed.get('month',''):02d}-{ed.get('day',''):02d}" if ed.get("year") else ""
        score = m.get("averageScore") or ""
        if score != "": score = score / 10.0
        popularity = m.get("popularity") or ""
        favorites = m.get("favourites") or ""
        rank = total + i + 1
        year = sd.get("year") or ""
        season = m.get("season") or ""
        genres = ", ".join(m.get("genres") or [])
        studios_list = [n["name"] for n in (m.get("studios") or {}).get("nodes", [])]
        studios = ", ".join(studios_list)
        img = (m.get("coverImage") or {}).get("large") or (m.get("coverImage") or {}).get("medium") or ""
        synopsis = (m.get("description") or "")[:500]
        duration = m.get("duration") or ""
        source = m.get("source") or ""
        rating = ""

        row = [m["id"], title, title_jp, fmt, source, episodes, status, aired_from, aired_to,
               score, "", rank, popularity, "", favorites, rating, duration, year, season,
               genres, studios, img, synopsis]
        writer.writerow(row)

    csv_file.flush()
    total += len(media)
    print(f"\n>>> Total anime added so far: {total}")

    has_next = resp["data"]["Page"]["pageInfo"].get("hasNextPage", False)
    if not has_next:
        print("No more pages. Done.")
        break

    page += 1
    time.sleep(1.0)

csv_file.close()
print(f"\n=== DONE === Total: {total}")
