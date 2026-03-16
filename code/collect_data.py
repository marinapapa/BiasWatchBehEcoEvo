from pybliometrics.scopus import ScopusSearch, AbstractRetrieval
import csv
import os
import time
import unicodedata
import pybliometrics
import json
import traceback

#Need to have a pybliometrics config file (~/.config/pybliometrics.cfg) set up with your Scopus API key.

# Initialize Pybliometrics (ensures config is loaded)
pybliometrics.init()

def open_csv_to_list(filename):
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        return [item for row in reader for item in row]

def get_papers_info(journal_list):
    all_ids = []
    errors = 0
    for journal in journal_list:
        try:
            print(f"Searching: {journal}")
            #if using journal name instead of ISSN (less accurate)
            #query = f"EXACTSRCTITLE(\"{journal}\") AND SRCTYPE(j) AND PUBYEAR > 1999 AND PUBYEAR < 2021"
            query = f"ISSN({journal}) AND PUBYEAR > 1999 AND PUBYEAR < 2021"
            s = ScopusSearch(query, refresh=False)
            ids = s.get_eids()
            all_ids.extend(ids)
            print(f"Found {len(ids)} papers in {journal}")
        except Exception as e:
            print(f"Error retrieving {journal}: {type(e).__name__} - {str(e)}")
            #traceback.print_exc()  # Optional: prints the full traceback
            #print(f"Error fetching {journal}: {e}")
            errors += 1
            continue
    print(f"Collected {len(all_ids)} IDs with {errors} errors")
    return all_ids

def save_ids_to_csv(eid_list, output_path):
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w') as f:
        for eid in eid_list:
            f.write(f"{eid}\n")

def read_eids_from_csv(filepath):
    with open(filepath, 'r') as f:
        return [line.strip() for line in f if line.strip()]


def get_metadata(eid_list, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    start = time.time()
    success = 0
    failures = 0

    for eid in eid_list:
        try:
            ab = AbstractRetrieval(eid, view="FULL", refresh=False)
            metadata = {
                "eid": eid,
                "title": ab.title,
                "doi": ab.doi,
                "publication_name": ab.publicationName,
                "cover_date": ab.coverDate,
                "authors": [
                    f"{author.given_name} {author.surname}"
                    for author in ab.authors or []
                ],
                "affiliations": [
                    aff.name for aff in ab.affiliation or []
                ],
                "abstract": ab.abstract,
                "keywords": ab.authkeywords,
                "citation_count": ab.citedby_count,
                "reference_count": len(ab.references or [])

            }

            filename = os.path.join(output_dir, f"{eid}.json")
            with open(filename, "w", encoding="utf-8") as f:
                json.dump(metadata, f, ensure_ascii=False, indent=2)

            success += 1
        except Exception as e:
            print(f"Error retrieving {eid}: {type(e).__name__} - {str(e)}")
            #traceback.print_exc()  # Optional: prints the full traceback
            failures += 1

    end = time.time()
    print(f"Downloaded {success} records with {failures} failures in {end - start:.2f} seconds.")


# Main execution

# Example list of journal ISSNs
issn_list = [
    "00048038",  # Ornithology
    "00063606",  # Biotropica
    "27307182",  # BMC Ecology and Evolution
    "21508925",  # Ecosphere
    "02697653",  # Evolutionary Ecology
    "00191019",  # Ibis
    "01640291",  # International Journal of Primatology
    "14209101",  # Journal of Evolutionary Biology
    "03051838",  # Mammal Review
    "02752565",  # American Journal of Primatology
    "23993642",  # Communications Biology
    "00713260",  # Evolutionary Biology
    "00222372",  # Journal of Mammalogy
    "09528369",  # Journal of Zoology
    "20513933"   # Movement Ecology
]

ids = get_papers_info(issn_list)
save_ids_to_csv(ids, "../Data/Extracted/ID_list.csv")
eid_file = "../Data/Extracted/ID_list.csv"
eids = read_eids_from_csv(eid_file)


get_metadata(eids, "../Data/Downloads/All_papers_jsons")
