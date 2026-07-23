// Based on the documented Quarto poster template and pncnmnp/typst-poster.
// The function stays in this template partial so the document body and project
// resources are resolved in the generated Typst document.
#let poster(
  size: "'36x24' or '48x36''",
  title: "Paper Title",
  authors: "Author Names (separated by commas)",
  departments: "Department Name",
  univ_logo: "Logo Path",
  footer_text: "Footer Text",
  footer_url: "Footer URL",
  footer_email_ids: "Email IDs (separated by commas)",
  footer_color: "Hex Color Code",
  keywords: (),
  num_columns: "3",
  univ_logo_scale: "100",
  univ_logo_column_size: "10",
  title_column_size: "20",
  title_font_size: "48",
  authors_font_size: "36",
  footer_url_font_size: "30",
  footer_text_font_size: "40",
  body,
) = {
  set text(size: 16pt)

  let sizes = size.split("x")
  let width = int(sizes.at(0)) * 1in
  let height = int(sizes.at(1)) * 1in
  univ_logo_scale = int(univ_logo_scale) * 1%
  title_font_size = int(title_font_size) * 1pt
  authors_font_size = int(authors_font_size) * 1pt
  num_columns = int(num_columns)
  univ_logo_column_size = int(univ_logo_column_size) * 1in
  title_column_size = int(title_column_size) * 1in
  footer_url_font_size = int(footer_url_font_size) * 1pt
  footer_text_font_size = int(footer_text_font_size) * 1pt

  set page(
    width: width,
    height: height,
    margin: (top: 1in, left: 2in, right: 2in, bottom: 2in),
    footer: [
      #set align(center)
      #block(
        fill: rgb(footer_color),
        width: 100%,
        inset: 20pt,
        radius: 10pt,
        [
          #text(font: "Courier", size: footer_url_font_size, footer_url)
          #h(1fr)
          #text(size: footer_text_font_size, smallcaps(footer_text))
          #h(1fr)
          #text(font: "Courier", size: footer_url_font_size, footer_email_ids)
        ],
      )
    ],
  )

  set math.equation(numbering: "(1)")
  show math.equation: set block(spacing: 0.65em)
  set enum(indent: 10pt, body-indent: 9pt)
  set list(indent: 10pt, body-indent: 9pt)

  set heading(numbering: "I.A.1.")
  show heading: it => context {
    let levels = counter(heading).get()
    let deepest = if levels != () { levels.last() } else { 1 }

    set text(24pt, weight: 400)
    if it.level == 1 [
      #set align(center)
      #set text(32pt)
      #show: smallcaps
      #v(50pt, weak: true)
      #if it.numbering != none {
        numbering("I.", deepest)
        h(7pt, weak: true)
      }
      #it.body
      #v(35.75pt, weak: true)
      #line(length: 100%)
    ] else if it.level == 2 [
      #set text(style: "italic")
      #v(32pt, weak: true)
      #if it.numbering != none {
        numbering("i.", deepest)
        h(7pt, weak: true)
      }
      #it.body
      #v(10pt, weak: true)
    ] else [
      #if it.level == 3 {
        numbering("1)", deepest)
        [ ]
      }
      _#(it.body):_
    ]
  }

  align(
    center,
    grid(
      rows: 2,
      columns: (univ_logo_column_size, title_column_size),
      column-gutter: 0pt,
      row-gutter: 50pt,
      image(univ_logo, width: univ_logo_scale),
      text(title_font_size, title + "\n\n")
        + text(authors_font_size, emph(authors) + "   (" + departments + ") "),
    ),
  )

  show: columns.with(num_columns, gutter: 64pt)
  set par(justify: true, first-line-indent: 0em, spacing: 0.65em)

  if keywords != () [
    #set text(24pt, weight: 400)
    #show "Keywords": smallcaps
    *Keywords* --- #keywords.join(", ")
  ]

  body
}
