-- Retain the document preamble plus the sections named in sample-sections.
-- This filter runs after Quarto resolves cross-references. For LaTeX output,
-- Quarto's resolved reference text still contains a raw \ref{...}; before
-- dropping omitted targets, resolve those raw refs from the full paper's label
-- file and mark them as full-paper references.

local selected = nil
local full_paper_url = nil
local reference_labels = {}

local function selected_ids(meta)
  local ids = {}
  local value = meta["sample-sections"]
  if value == nil then
    return ids
  end
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

local function has_class(el, class_name)
  for _, class in ipairs(el.classes or {}) do
    if class == class_name then
      return true
    end
  end
  return false
end

local function read_reference_labels(path)
  local labels = {}
  local handle = io.open(path, "r")
  if handle == nil then
    error("Writing-sample reference index is missing: " .. path)
  end
  for line in handle:lines() do
    local id, number = string.match(line, "^\\newlabel{([^}]+)}{{([^}]*)}")
    if id ~= nil and number ~= nil and number ~= "" then
      labels[id] = number
    end
  end
  handle:close()
  return labels
end

local function full_paper_suffix()
  local suffix = pandoc.Inlines({pandoc.Space(), pandoc.Str("(")})
  if full_paper_url ~= nil and full_paper_url ~= "" then
    suffix:insert(pandoc.Link("full paper", full_paper_url))
  else
    suffix:insert(pandoc.Str("full"))
    suffix:insert(pandoc.Space())
    suffix:insert(pandoc.Str("paper"))
  end
  suffix:insert(pandoc.Str(")"))
  return suffix
end

local function resolve_reference_number(inlines, target_id)
  local number = reference_labels[target_id]
  if number == nil then
    error("Full-paper reference index has no label for " .. target_id)
  end

  return inlines:walk({
    RawInline = function(raw)
      if raw.format == "tex" or raw.format == "latex" then
        local id = string.match(raw.text, "^\\ref{%s*([^}]+)%s*}$")
        if id == target_id then
          return pandoc.Str(number)
        end
      end
      return nil
    end
  })
end

local function externalize_crossref(link)
  if not has_class(link, "quarto-xref") then
    return nil
  end

  local target_id = string.gsub(link.target or "", "^#", "")
  if selected[target_id] then
    return nil
  end

  local out = resolve_reference_number(pandoc.Inlines(link.content), target_id)
  out:extend(full_paper_suffix())
  return out
end

function Meta(meta)
  selected = selected_ids(meta)
  local url = meta["sample-full-paper-url"]
  if url ~= nil then
    full_paper_url = pandoc.utils.stringify(url)
  end
  local aux = meta["sample-reference-aux"]
  if aux ~= nil then
    reference_labels = read_reference_labels(pandoc.utils.stringify(aux))
  end
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
  return doc:walk({Link = externalize_crossref})
end
