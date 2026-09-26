-- Retain the sections named in sample-sections and preserve the numbering of
-- retained sections, figures, tables, and equations from the full paper.

local selected = nil
local reference_labels = nil

local function meta_strings(value)
  local out = {}
  if value == nil then
    return out
  end
  local value_type = pandoc.utils.type(value)
  if value_type == "List" or value_type == "MetaList" then
    for _, item in ipairs(value) do
      out[pandoc.utils.stringify(item)] = true
    end
  else
    out[pandoc.utils.stringify(value)] = true
  end
  return out
end

local function reference_label_map(value)
  local out = {}
  if value == nil then
    return out
  end
  for key, item in pairs(value) do
    out[key] = pandoc.utils.stringify(item)
  end
  return out
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

local section_counters = {"section", "subsection", "subsubsection", "paragraph", "subparagraph"}

local function numeric_parts(number)
  local parts = {}
  for part in string.gmatch(number or "", "[^.]+") do
    local value = tonumber(part)
    if value == nil then
      return nil
    end
    table.insert(parts, value)
  end
  if #parts == 0 then
    return nil
  end
  return parts
end

local function counter_reset(id)
  local number = reference_labels[id]
  if number == nil or number == "" then
    return nil
  end

  local prefix = string.match(id, "^([^-]+)-")
  if prefix == "sec" then
    local parts = numeric_parts(number)
    if parts == nil or #parts > #section_counters then
      return nil
    end
    local commands = {}
    for i, value in ipairs(parts) do
      local target = value
      if i == #parts then
        target = target - 1
      end
      table.insert(commands, string.format("\\setcounter{%s}{%d}", section_counters[i], target))
    end
    return pandoc.RawBlock("latex", table.concat(commands, "\n"))
  end

  local counter = ({tbl = "table", fig = "figure", eq = "equation"})[prefix]
  local value = tonumber(number)
  if counter ~= nil and value ~= nil and math.floor(value) == value then
    return pandoc.RawBlock("latex", string.format("\\setcounter{%s}{%d}", counter, value - 1))
  end
  return nil
end

local function block_crossref_id(block)
  if block.t == "Figure" or block.t == "Table" or block.t == "Div" then
    local id = block.identifier or ""
    local prefix = string.match(id, "^([^-]+)-")
    if prefix == "fig" or prefix == "tbl" or prefix == "eq" then
      return id
    end
  end
  if block.t == "RawBlock" and block.format == "latex" then
    local id = string.match(block.text, "\\label{([A-Za-z]+%-[A-Za-z0-9_-]+)}")
    if id ~= nil then
      local prefix = string.match(id, "^([^-]+)-")
      if prefix == "tbl" or prefix == "fig" or prefix == "eq" then
        return id
      end
    end
  end
  return nil
end

function Meta(meta)
  selected = meta_strings(meta["sample-sections"])
  reference_labels = reference_label_map(meta["sample-reference-labels"])
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

  local function insert_numbered(block)
    local id = block_crossref_id(block)
    if id ~= nil then
      local reset = counter_reset(id)
      if reset ~= nil then
        out:insert(reset)
      end
    end
    out:insert(block)
  end

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

    local retain = (not seen_top_level) or active or (block.t == "Header" and context[block.identifier])
    if retain then
      if block.t == "Header" then
        local reset = counter_reset(block.identifier)
        if reset ~= nil then
          out:insert(reset)
        end
        out:insert(block)
      else
        insert_numbered(block)
      end
    end
  end

  doc.blocks = out
  return doc
end
