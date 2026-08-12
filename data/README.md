# LendingClub accepted-loan data contract

The analysis expects the accepted-loan history distributed through the public [LendingClub dataset mirror on Kaggle](https://www.kaggle.com/datasets/wordsforthewise/lending-club/data). The raw file is intentionally excluded from Git because it is approximately 1.56 GiB and contains high-dimensional borrower records.

This mirror is the executable source for the project. As an independent provenance check, a [Hong Kong Institute for Monetary and Financial Research paper hosted by the Bank for International Settlements](https://www.bis.org/events/confresearchnetwork1909/lam.pdf#page=3) describes LendingClub's funded-listing sequence as 2,260,701 observations from June 2007 through December 2018—the same record count and historical coverage represented by this accepted-loan artifact. LendingClub's [2014 Form S-1 in SEC EDGAR](https://www.sec.gov/Archives/edgar/data/1409970/000119312514323136/d766811ds1.htm) provides primary historical documentation of the platform.

## Expected local artifact

| Property | Recorded value |
|---|---|
| Relative path | `data/accepted_2007_to_2018Q4.csv` |
| Size | 1,675,133,810 bytes |
| Records | 2,260,701 data rows |
| Columns | 151 |
| SHA-256 | `3eae03c28fd9d2e8a076ebeb73507e8d4d0f44d90500decdb0936e0933d1f36a` |

Download the archive from the Kaggle data card, extract the accepted-loan CSV, and place it at the path above. From the repository root, verify that it matches the file used for the project:

```bash
make verify-data
```

The checksum identifies the exact local artifact behind the submitted analysis. A newer mirror or re-export may contain the same observations with different byte-level formatting; in that case, confirm the 151-column schema and 2,260,701 records before running the pipeline. Do not treat matching dimensions alone as byte-level verification.

## Data handling

The source includes identifiers, employment text, location fields, credit-history summaries, and post-origination servicing variables. The R pipeline constructs a smaller origination-only modelling table and deliberately removes direct identifiers, free-text fields, and outcome leakage. Do not commit the raw CSV, derived borrower-level extracts, or local archives.

Use of the dataset remains subject to the terms and documentation provided by its distributor. The repository's MIT license applies to the original code and documentation, not to the third-party LendingClub records. The repository does not grant redistribution rights for raw or derived borrower-level data.
