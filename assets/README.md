# Poster assets

`uw-logo-horizontal-full-color-print.svg` is a flattened copy of the University of Wisconsin wordmark PDF used by the conference poster. The supplied Illustrator PDF contained optional-content groups, which Typst warns may render incorrectly. It was exported as plain SVG with Inkscape, removing the PDF optional-content groups while preserving vector content:

```sh
inkscape path/to/source-logo.pdf \
  --pdf-poppler \
  --export-plain-svg \
  --export-filename=uw-logo-horizontal-full-color-print.svg
```

`repo-qr.svg` links to the repository landing page used on the poster.
