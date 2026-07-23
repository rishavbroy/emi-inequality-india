// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



// Imported from quarto-ext/typst-templates/poster and resolved from the
// extension's gathered local package during Quarto's Typst staging step.
#import "@local/typst-poster:0.1.1": poster
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: doc => poster(
   title: [Can English-Medium Instruction Raise Local Living Standards in India?], 
   authors: [Rishav Roy], 
   departments: [Department of Economics, University of Wisconsin-Madison], 
   size: "36x24", 
   univ_logo: "../../assets/uw-logo-horizontal-full-color-print.pdf", 
   footer_text: [PREDOC.org Research Conference | Chicago Booth | July 30-31, 2026], 
   footer_url: [rishavbroy.github.io], 
   footer_email_ids: [roybrishav\@gmail.com], 
   footer_color: "7a0019", 
   num_columns: 3, 
   univ_logo_scale: 100, 
   univ_logo_column_size: 7, 
   title_column_size: 25, 
   title_font_size: 85, 
   authors_font_size: 56, 
   footer_url_font_size: 20, 
   footer_text_font_size: 22, 
  doc,
)

= Why this matters
<why-this-matters>
+ #emph[Jobless growth.] India's rapid GDP growth has coincided with weak wage growth, falling labor-force participation, and high unemployment among educated young people @organization2024a.

+ #emph[English as human capital.] English fluency is associated with large wage premia, access to higher education, and employment in India's expanding service economy @azam2013a@chakraborty2016a.

+ #emph[EMI joins schooling and English.] English-medium instruction (EMI) teaches all subjects in English. Families often treat it as a route to mobility, but fees, school quality, and unequal access may limit its benefits @lahoti2019a@bhattacharya2013.

+ #emph[The question.] When more children in a district attend EMI schools, does average household consumption grow faster over the next decade?

= Research design
<research-design>
#block(fill: rgb("f3f0ea"), inset: 12pt, radius: 5pt, width: 100%)[
  #grid(columns: (1fr, auto, 1fr, auto, 1fr), gutter: 10pt,
    align(center)[*Census 2001*\ Mother tongues\ + district identities],
    align(center)[#text(size: 30pt, fill: rgb("7a0019"))[→]],
    align(center)[*NSS 2007-08*\ EMI exposure\ + baseline controls],
    align(center)[#text(size: 30pt, fill: rgb("7a0019"))[→]],
    align(center)[*NSS 2017-18*\ Consumption\ + inequality outcomes]
  )
  #v(10pt)
  #align(center)[*Common unit:* Census 2001 districts]
]
#strong[Treatment.] District EMI exposure is the percentage of school-going children enrolled in English-medium instruction in 2007-08.

#strong[Outcome.] Percentage growth in district mean household consumption between 2007-08 and 2017-18.

#strong[Instrument.] Average linguistic distance between Hindi and the three most common mother tongues in each Census 2001 district. The first stage asks whether distance from Hindi predicts local EMI enrollment.

#strong[Controls.] Baseline consumption and inequality, urbanization, household structure, religion and social group shares, landholding, household-head education, and region fixed effects.

#figure([
#box(image("../../outputs/figures/main/map_emi_exposure.pdf", width: 94.0%))
], caption: figure.caption(
position: bottom, 
[
EMI exposure in 2007-08.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


#colbreak()
= Main result
<main-result>
#figure([
#box(image("../../outputs/figures/main/poster_emie_expected_values.pdf", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Adjusted consumption growth across the observed distribution of district EMI exposure. The line averages counterfactual predictions at each displayed percentile; the ribbon shows 95% confidence intervals.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


The current estimate is small and imprecise: once baseline district conditions are accounted for, a higher EMI share does not clearly predict faster local consumption growth.

This result is substantively important even without statistical precision. Household investments in English and schooling may improve individual opportunities without raising average living standards in the district where children were educated.

= How should we interpret it?
<how-should-we-interpret-it>
#strong[Selection.] EMI enrollment reflects household income, parental education, urban access, school supply, and beliefs about school quality. These are also plausible causes of later consumption growth.

#strong[Migration.] Benefits may follow students who move to cities rather than remain in their childhood district. Local estimates would then miss part of EMI's return.

#strong[School quality.] An English label does not guarantee English instruction. In some settings, nominal EMI schools continue to teach mainly in the regional language @lahoti2019a.

#strong[Local labor demand.] English skills are most valuable where firms and occupations reward them. Education alone cannot create enough suitable jobs.

= Instrumental variation
<instrumental-variation>
#figure([
#box(image("../../outputs/figures/main/map_linguistic_distance.pdf", width: 94.0%))
], caption: figure.caption(
position: bottom, 
[
Average linguistic distance from Hindi, constructed from Census 2001 mother-tongue composition.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


Linguistic distance offers variation in the relative cost of acquiring Hindi rather than English. The design assumes that, conditional on baseline characteristics and region, this distance affects later consumption growth through EMI exposure rather than another channel.

#colbreak()
= Why district histories matter
<why-district-histories-matter>
India's district boundaries and names changed substantially between the three data vintages. A credible pseudo-panel must solve three distinct problems.

== 1. Name resolution
<name-resolution>
Names vary because of typographical errors, punctuation, “&” versus “and,” transliteration, shortening, and official renaming. Changes may affect either district or state names. Official changes include full replacements, nativization, prefixes or suffixes, and regional-language to English switches @indiastatestories1HowDistricts2025.

== 2. Geographical change
<geographical-change>
- #strong[Partition:] one district is replaced by two or more districts.
- #strong[Merger:] two or more districts become one.
- #strong[Carve-out:] a new district is formed from parts of two or three earlier districts.
- #strong[Boundary shift:] territory transfers between districts; analytically, this is a limited carve-out.

These changes can also follow state reorganization. Equal names across years therefore do not prove equal territory @indiastatestories1HowDistricts2025.

== 3. Variable assignment
<variable-assignment>
Different quantities require different rules:

- #emph[Counts and totals:] sum additive components.
- #emph[Means and shares:] pool underlying records or numerators and denominators, then recompute.
- #emph[EMI exposure:] pool eligible children and recompute the enrollment share.
- #emph[Gini coefficients:] pool household consumption records and recompute; never average district Ginis.

When a later district crosses more than one 2001 district and household locations are unavailable, exact assignment is impossible. The preferred analysis should retain deterministic histories and report alternative assumptions separately.

= Contribution
<contribution>
This project combines nationally representative household surveys, Census language data, administrative histories, and spatial data to study a question at the intersection of education, language, inequality, and local development.

The present estimates are provisional while district histories are being strengthened. The central empirical question, data construction, and reproducible analysis are designed so that the poster updates with each improved district panel.

#block(fill: rgb("f3f0ea"), inset: 12pt, radius: 5pt, width: 100%)[
  #grid(columns: (1fr, auto), gutter: 16pt,
    [*Code, paper, and updates*\ #link("https://git.new/FwmVaXq")[git.new/FwmVaXq]\ #link("https://rishavbroy.github.io")[rishavbroy.github.io]],
    image("../../assets/repo-qr.svg", width: 1.45in)
  )
]
= Selected references
<selected-references>



#bibliography(("../../paper/references.bib"))

