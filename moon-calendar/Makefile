# Data comes from the Astronomical Applications API v4.0.1
# https://aa.usno.navy.mil/data/api
#
# We cache them as part of the build process because:
#
# 1. The API lacks CORS and thus cannot be fetched in the browser.
# 2. The data changes rarely, whereas the API broke not that long ago.

YEARS = $(shell seq 2024 2100)

all: $(patsubst %,data/%.json,$(YEARS))

data/%.json:
	mkdir -p $(@D)
	curl -LSfs -o $@.tmp "https://aa.usno.navy.mil/api/moon/phases/year?ID=WUFCALEN&year=$*"
	mv $@.tmp $@
