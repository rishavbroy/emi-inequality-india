# Poster assets

`uw-logo-horizontal-full-color-print.pdf` is the official UW–Madison horizontal wordmark retained as the archival print source.

`uw-logo-horizontal-full-color-print.svg` is the vector derivative used by the Typst poster. It was exported from the official PDF with `pdftocairo -svg` so Typst does not encounter the PDF optional-content groups that trigger an export warning.

`repo-qr.svg` links to the repository landing page used on the poster.

The poster build keeps this official PDF unchanged and uses Ghostscript to generate `outputs/derived/poster/uw-logo-horizontal-full-color-print-flat.pdf`. The PDF 1.3 derivative preserves the print artwork while removing the optional-content group that Typst warns about. The derivative is generated output, not a substitute source asset.
