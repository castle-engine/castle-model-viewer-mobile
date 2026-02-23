# Use "make" to regenerate PNGs.

SOURCES:=$(wildcard *.svg)
OUTPUT:=$(patsubst %.svg,%.png,$(SOURCES))

.PHONY: default
default: $(OUTPUT)

.PHONY: clean
clean:
	rm -Rf $(OUTPUT)

warning.png: warning.svg
	inkscape --export-width=40 --export-height=40 $< --export-filename=$@
# do not change colors for this icon

%.png: %.svg
	inkscape --export-width=40 --export-height=40 $< --export-filename=$@
# change to white, following
# https://stackoverflow.com/questions/57427046/imagemagick-replace-rgb-channels-with-white-retain-alpha
	convert $@ -fill white -colorize 100 $@
