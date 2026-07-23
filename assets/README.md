# Poster assets

`uw-logo-horizontal-full-color-print.pdf` is a flattened copy of the University of Wisconsin wordmark PDF used by the conference poster. The supplied Illustrator PDF contained optional-content groups, which Typst warns may render incorrectly. It was normalized with Ghostscript while preserving vector output:

```sh
gs -q -dSAFER -dBATCH -dNOPAUSE \
  -sDEVICE=pdfwrite \
  -dCompatibilityLevel=1.7 \
  -dDetectDuplicateImages=true \
  -dCompressFonts=true \
  -sOutputFile=uw-logo-horizontal-full-color-print.pdf \
  path/to/source-logo.pdf
```

`repo-qr.svg` links to the repository landing page used on the poster.
