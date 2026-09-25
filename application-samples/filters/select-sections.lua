-- Retain the document preamble plus the sections named in sample-sections.
-- Section IDs are ordinary Pandoc/Quarto Header identifiers. When a selected
-- subsection is nested inside an omitted section, keep the ancestor heading as
-- context without retaining the ancestor's unselected body.

local selected = nil

local function selected_ids(meta)
  local ids = {}
  local value = meta["sample-sections"]
  if value == nil then
    return ids
  end
  -- Pandoc represents metadata sequences as List values in current releases.
  -- pandoc.utils.type() is the supported way to distinguish metadata values.
  local value_type = pandoc.utils.type(value)
  if value_type == "List" or value_type == "MetaList" then
    for _, item in ipairs(value) do
      ids[pandoc.utils.stringify(item)] = true
    end
  else
    ids[pandoc.utils.stringify(value)] = true
  end
  return ids
end

local function ancestor_ids(blocks, selected_ids_set)
  local ancestors = {}
  local stack = {}

  for _, block in ipairs(blocks) do
    if block.t == "Header" then
      for level = block.level, 6 do
        stack[level] = nil
      end
      if selected_ids_set[block.identifier] then
        for level = 1, block.level - 1 do
          local ancestor = stack[level]
          if ancestor ~= nil and ancestor ~= "" then
            ancestors[ancestor] = true
          end
        end
      end
      stack[block.level] = block.identifier
    end
  end
  return ancestors
end

function Meta(meta)
  selected = selected_ids(meta)
  return meta
end

function Pandoc(doc)
  if selected == nil or next(selected) == nil then
    return doc
  end

  local context = ancestor_ids(doc.blocks, selected)
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

    if (not seen_top_level) or active or (block.t == "Header" and context[block.identifier]) then
      out:insert(block)
    end
  end

  doc.blocks = out
  return doc
end
