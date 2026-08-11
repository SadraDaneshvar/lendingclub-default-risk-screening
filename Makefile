R ?= Rscript
DATA_FILE := data/accepted_2007_to_2018Q4.csv
DATA_SHA256 := 3eae03c28fd9d2e8a076ebeb73507e8d4d0f44d90500decdb0936e0933d1f36a

.PHONY: setup verify-data run clean

setup:
	$(R) requirements.R

verify-data:
	test -f $(DATA_FILE)
	printf "%s  %s\n" "$(DATA_SHA256)" "$(DATA_FILE)" | shasum -a 256 -c -

run:
	$(R) src/risk_at_first_sight.R

clean:
	rm -rf results
	rm -f Rplots.pdf
