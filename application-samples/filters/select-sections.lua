-- Retain the document preamble plus the sections named in sample-sections.
-- Section IDs are ordinary Pandoc/Quarto Header identifiers.

local selected = nil

local function selected_ids(meta)
  local ids = {}
  local value = meta["sample-sections"]
  if value == nil then
    return ids
  end
  if value.t == "MetaList" then
    for _, item in ipairs(value) do
      ids[pandoc.utils.stringify(item)] = true
    end
  else
    ids[pandoc.utils.stringify(value)] = true
  end
  return ids
end

function Meta(meta)
  selected = selected_ids(meta)
  return meta
end

function Pandoc(doc)
  if selected == nil or next(selected) == nil then
    return doc
  end

  local out = pandoc.List()
  local seen_top_level = false
  local active = false
  local active_level = nil

  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" then
      if block.level == 1 then
        seen_top_level = true
      end
      if active and block.level <= active_level then
        active = false
        active_level = nil
      end
      if selected[block.identifier] then
        active = true
        active_level = block.level
      end
    end

    if (not seen_top_level) or active then
      out:insert(block)
    end
  end

  doc.blocks = out
  return doc
end
