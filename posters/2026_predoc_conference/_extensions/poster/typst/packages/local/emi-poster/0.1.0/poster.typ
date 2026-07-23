#let poster(
  size: "36x24", title: "Paper Title", authors: "Author", departments: "Department",
  univ_logo: none, footer_text: "", footer_url: "", footer_email_ids: "", footer_color: "c5050c",
  num_columns: "3", univ_logo_column_size: "7",
  title_column_size: "25", title_font_size: "85", authors_font_size: "56",
  footer_url_font_size: "20", footer_text_font_size: "24", body
) = {
  let dims = size.split("x")
  let width = int(dims.at(0)) * 1in
  let height = int(dims.at(1)) * 1in
  let ncols = int(num_columns)
  set page(
    width: width, height: height,
    margin: (top: 0.55in, left: 0.65in, right: 0.65in, bottom: 1.05in),
    footer: block(
      fill: rgb(footer_color), width: 100%, inset: 10pt, radius: 5pt,
      grid(columns: (1fr, 1.3fr, 1fr),
        align(left, text(size: int(footer_url_font_size) * 1pt, fill: white, footer_url)),
        align(center, text(size: int(footer_text_font_size) * 1pt, weight: "bold", fill: white, footer_text)),
        align(right, text(size: int(footer_url_font_size) * 1pt, fill: white, footer_email_ids))
      )
    )
  )
  set text(font: "Libertinus Serif", size: 24pt, fill: rgb("202020"))
  set par(justify: false, leading: 0.62em, spacing: 0.55em)
  show figure.caption: set text(size: 18pt)
  set list(indent: 18pt, body-indent: 10pt, spacing: 9pt)
  set enum(indent: 18pt, body-indent: 10pt, spacing: 9pt)
  show heading: it => {
    if it.level == 1 {
      v(9pt, weak: true)
      block(fill: rgb("f3f0ea"), inset: (x: 10pt, y: 7pt), radius: 4pt, width: 100%,
        text(size: 36pt, weight: "bold", fill: rgb("7a0019"), it.body))
      v(5pt)
    } else {
      text(size: 28pt, weight: "bold", fill: rgb("7a0019"), it.body)
      v(3pt)
    }
  }
  let logo = univ_logo
  grid(columns: (int(univ_logo_column_size) * 1in, 1fr), gutter: 0.35in,
    align(center + horizon, logo),
    align(center, [#text(size: int(title_font_size) * 1pt, weight: "bold", fill: rgb("7a0019"), title)
      #v(10pt)
      #text(size: int(authors_font_size) * 1pt, authors)
      #v(5pt)
      #text(size: 30pt, departments)]))
  v(18pt)
  show: columns.with(ncols, gutter: 0.45in)
  body
}
