-- Link citations in named application excerpts to the full paper.
-- Anonymous samples omit sample-full-paper-url and therefore remain unlinked.

function Pandoc(doc)
  local value = doc.meta["sample-full-paper-url"]
  doc.meta["sample-full-paper-url"] = nil
  if value == nil then
    return doc
  end

  local full_paper_url = pandoc.utils.stringify(value)
  if full_paper_url == "" then
    return doc
  end

  doc.blocks = doc.blocks:walk({
    Cite = function(citation)
      return pandoc.Link(pandoc.Inlines({citation}), full_paper_url)
    end
  })
  return doc
end
