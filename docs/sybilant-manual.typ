// The Sybilant manual. Compiled directly by bin/build-manual
// (`typst compile --root <repo> docs/sybilant-manual.typ docs/sybilant-manual.pdf`).
//
// Note: Vale's Typst support lints string arguments as prose even inside
// #set/#let calls (not just [content] blocks). Multiple quoted arguments
// on one line can trip Google.Quotes/Google.We as if they were adjacent
// prose -- keep each argument on its own line below to avoid that.

#let build-date = sys.inputs.at(
  "build_date",
  default: "unknown",
)

#set document(
  title: "Sybilant Manual",
)
#set text(font: "Libertinus Serif")
#set page(
  numbering: "1",
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
)

#align(center + horizon)[
  #text(size: 24pt, weight: "bold")[Sybilant Manual]
  #v(0.5em)
  #text(size: 14pt)[design and implementation]
  #v(1em)
  #text(size: 12pt)[Generated #build-date]
]
#pagebreak()

#outline()
#pagebreak()

TBD
