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
  accent_color: none,
  section_fill: "f3f0ea",
  keywords: (),
  num_columns: "3",
  univ_logo_scale: "100",
  univ_logo_column_size: "10",
  title_column_size: "20",
  title_font_size: "48",
  authors_font_size: "36",
  footer_url_font_size: "30",
  footer_text_font_size: "40",
  body_font_size: "16",
  heading_font_size: "32",
  subheading_font_size: "22",
  body,
) = {

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
  body_font_size = int(body_font_size) * 1pt
  heading_font_size = int(heading_font_size) * 1pt
  subheading_font_size = int(subheading_font_size) * 1pt
  let accent = if accent_color == none { rgb(footer_color) } else { rgb(accent_color) }
  let section_background = rgb(section_fill)

  set text(size: body_font_size)

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

  set heading(numbering: none)
  show heading: it => {
    if it.level == 1 {
      block(
        width: 100%,
        fill: section_background,
        inset: (x: 7pt, y: 2pt),
        radius: 3pt,
        above: 20pt,
        below: 8pt,
        text(heading_font_size, fill: accent, weight: "bold", it.body),
      )
    } else if it.level == 2 {
      block(
        above: 12pt,
        below: 4pt,
        text(
          subheading_font_size,
          fill: accent,
          weight: "bold",
          style: "italic",
          it.body,
        ),
      )
    } else {
      text(18pt, fill: accent, weight: "bold", it.body)
    }
  }

  align(
    center,
    grid(
      rows: 2,
      columns: (univ_logo_column_size, title_column_size),
      column-gutter: 0pt,
      row-gutter: 50pt,
      image(univ_logo, width: univ_logo_scale),
      text(title_font_size, fill: accent, weight: "bold", title + "\n\n")
        + text(authors_font_size, authors + "   (" + departments + ") "),
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
