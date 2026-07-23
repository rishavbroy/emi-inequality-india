#show: doc => poster(
  $if(title)$ title: [$title$], $endif$
  $if(poster-authors)$ authors: [$poster-authors$], $endif$
  $if(departments)$ departments: [$departments$], $endif$
  $if(size)$ size: "$size$", $endif$
  $if(institution-logo)$ univ_logo: image("$institution-logo$"$if(univ-logo-scale)$, width: $univ-logo-scale$ * 1%$endif$), $endif$
  $if(footer-text)$ footer_text: [$footer-text$], $endif$
  $if(footer-url)$ footer_url: [$footer-url$], $endif$
  $if(footer-emails)$ footer_email_ids: [$footer-emails$], $endif$
  $if(footer-color)$ footer_color: "$footer-color$", $endif$
  $if(num-columns)$ num_columns: $num-columns$, $endif$
  $if(univ-logo-column-size)$ univ_logo_column_size: $univ-logo-column-size$, $endif$
  $if(title-font-size)$ title_font_size: $title-font-size$, $endif$
  $if(authors-font-size)$ authors_font_size: $authors-font-size$, $endif$
  $if(footer-url-font-size)$ footer_url_font_size: $footer-url-font-size$, $endif$
  $if(footer-text-font-size)$ footer_text_font_size: $footer-text-font-size$, $endif$
  doc,
)
