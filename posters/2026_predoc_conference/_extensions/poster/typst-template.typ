// Based on Quarto's documented poster format and pncnmnp/typst-poster.
// Content and routine theme settings enter through poster.qmd metadata; this
// partial owns the reusable page, header, heading, footer, and column layout.
#let poster(
  size: "36x24",
  title: "Paper Title",
  authors: "Author Names",
  departments: "Department Name",
  univ_logo: "Logo Path",
  footer_text: "Footer Text",
  footer_url: "Footer URL",
  footer_email_ids: "Email IDs",
  footer_color: "7a0019",
  accent_color: none,
  section_fill: "f3f0ea",
  keywords: (),
  num_columns: "3",
  univ_logo_scale: "100",
  univ_logo_column_size: "7",
  title_column_size: "25",
  title_font_size: "85",
  authors_font_size: "56",
  department_font_size: "30",
  footer_url_font_size: "20",
  footer_text_font_size: "24",
  body_font_size: "24",
  heading_font_size: "36",
  subheading_font_size: "28",
  logo_y_offset: "-10",
  title_y_offset: "10",
  author_y_offset: "-8",
  body,
) = {
  let dims = size.split("x")
  let width = int(dims.at(0)) * 1in
  let height = int(dims.at(1)) * 1in
  let ncols = int(num_columns)
  let logo_width = int(univ_logo_column_size) * 1in
  let title_width = int(title_column_size) * 1in
  let logo_scale = int(univ_logo_scale) * 1%
  let logo_path = univ_logo.replace("\\", "/")
  let title_size = int(title_font_size) * 1pt
  let author_size = int(authors_font_size) * 1pt
  let department_size = int(department_font_size) * 1pt
  let footer_side_size = int(footer_url_font_size) * 1pt
  let footer_center_size = int(footer_text_font_size) * 1pt
  let body_size = int(body_font_size) * 1pt
  let heading_size = int(heading_font_size) * 1pt
  let subheading_size = int(subheading_font_size) * 1pt
  let logo_shift = int(logo_y_offset) * 1pt
  let title_shift = int(title_y_offset) * 1pt
  let author_shift = int(author_y_offset) * 1pt
  let accent = if accent_color == none { rgb(footer_color) } else { rgb(accent_color) }
  let section_background = rgb(section_fill)

  set page(
    width: width,
    height: height,
    margin: (top: 0.55in, left: 0.65in, right: 0.65in, bottom: 1.05in),
    footer: block(
      fill: rgb(footer_color),
      width: 100%,
      inset: 10pt,
      radius: 5pt,
      grid(
        columns: (1fr, 1.3fr, 1fr),
        align(left, text(size: footer_side_size, weight: "bold", fill: white, footer_url)),
        align(center, text(size: footer_center_size, weight: "bold", fill: white, footer_text)),
        align(right, text(size: footer_side_size, weight: "bold", fill: white, footer_email_ids)),
      ),
    ),
  )

  set text(font: "Libertinus Serif", size: body_size, fill: rgb("202020"))
  set par(justify: false, leading: 0.62em, spacing: 0.55em)
  show figure.caption: set text(size: 18pt)
  set list(indent: 18pt, body-indent: 10pt, spacing: 9pt)
  set enum(indent: 18pt, body-indent: 10pt, spacing: 9pt)
  set heading(numbering: none)
  show heading: it => {
    if it.level == 1 {
      v(16pt, weak: true)
      block(
        fill: section_background,
        inset: (x: 10pt, y: 5pt),
        radius: 4pt,
        width: 100%,
        text(size: heading_size, weight: "bold", fill: accent, it.body),
      )
      v(1pt)
    } else {
      text(size: subheading_size, weight: "bold", fill: accent, it.body)
      v(3pt)
    }
  }

  grid(
    columns: (logo_width, title_width),
    gutter: 0.35in,
    align(center + horizon, move(dy: logo_shift, image(logo_path, width: logo_scale))),
    align(
      center,
      move(
        dy: title_shift,
        [
          #text(size: title_size, weight: "bold", fill: accent, title)
          #v(8pt)
          #move(dy: author_shift, [
            #text(size: author_size, authors)
            #h(14pt)
            #text(size: department_size, departments)
          ])
        ],
      ),
    ),
  )
  v(18pt)

  show: columns.with(ncols, gutter: 0.45in)
  body
}
