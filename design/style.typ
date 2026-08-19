#let accent = rgb("#6542a6")
#let accent-soft = rgb("#f1edf8")
#let ink = rgb("#20202a")
#let muted = rgb("#626272")
#let rule-color = rgb("#d8d5df")

#let callout(body, title: [Note]) = block(
  width: 100%,
  fill: accent-soft,
  stroke: (left: (paint: accent, thickness: 2pt)),
  inset: (x: 12pt, y: 9pt),
  radius: (right: 4pt),
  breakable: true,
)[
  #text(font: "Source Sans 3", weight: 650, fill: accent)[#title]
  #v(3pt)
  #body
]

#let design-document(
  title: [Sybilant],
  subtitle: [Language design],
  body,
) = {
  set document(
    title: title,
    author: "The Sybilant Project",
    description: "Design and implementation notes for the Sybilant programming language",
  )
  set page(
    paper: "a4",
    margin: (top: 24mm, bottom: 22mm, x: 25mm),
    numbering: none,
  )
  set text(font: "Libertinus Serif", size: 10.5pt, fill: ink)
  set par(justify: true, leading: 0.7em)
  set heading(numbering: "1.1")
  set figure(numbering: "1")
  set table(inset: 6pt, stroke: (paint: rule-color, thickness: 0.45pt))
  show heading: set text(font: "Source Sans 3", weight: 650, fill: ink)
  show heading.where(level: 1): set text(size: 18pt)
  show heading.where(level: 2): set text(size: 13pt, fill: accent)
  show raw: set text(font: "Source Code Pro", size: 8.3pt)
  show link: set text(fill: accent)

  block(height: 245mm, width: 100%)[
    #v(27mm)
    #line(length: 16mm, stroke: (paint: accent, thickness: 2.2pt))
    #v(9mm)
    #text(font: "Source Sans 3", size: 10pt, weight: 650, tracking: 0.12em, fill: accent)[
      LANGUAGE DESIGN
    ]
    #v(5mm)
    #text(font: "Source Sans 3", size: 38pt, weight: 700, fill: ink)[#title]
    #v(3mm)
    #text(font: "Libertinus Serif", size: 17pt, style: "italic", fill: muted)[#subtitle]
    #v(1fr)
    #line(length: 100%, stroke: (paint: rule-color, thickness: 0.6pt))
    #v(4mm)
    #text(font: "Source Sans 3", size: 8.5pt, weight: 550, tracking: 0.06em, fill: muted)[
      SEMANTICS / TYPES / SYNTAX / IMPLEMENTATION
    ]
  ]

  pagebreak()
  counter(page).update(1)
  set page(numbering: "1", number-align: center)

  body
}
