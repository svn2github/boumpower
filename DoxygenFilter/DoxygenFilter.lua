-------------------------------------------------------------------------------
-- $HeadURL$
-- $Id$
-- $Revision$
-- $Author$
-- $Date$
-------------------------------------------------------------------------------

--! @file
--! @author Olivier Baumgartner


function debugMsg(...)
  print(string.format(...))
end

function trim(s)
  return string.match(s, "^%s*(.-)%s*$")
end

--! @fn default(value, def)
function default(value, def)
  if value then
    return value
  else
    return def
  end
end


function Lua2DoxPreProcessor(filter, contents)
  local state = filter.preprocessorstate -- shortcut
  local line = trim(contents.line)
  local p

  contents.isMagic   = state.magicLongComment 		-- false or magicLongComment
  contents.isComment = state.LongComment 			-- false or LongComment

  if contents.isMagic then
    -- magicLongComment
	contents.magicData = line
	contents.format = "%s%s"
  elseif contents.isComment then
    contents.comment = line
	contents.format = "%s%s"
  elseif string.find(line, "%-%-%!") == 1 then		-- begins with --!
    contents.isMagic = true
	contents.comment = string.sub(line, 4)
	contents.magicData = trim(contents.comment)
	contents.format = "%s//! %s"
  elseif string.find(line, "%-%-%[%[%!") == 1 then	-- begin with --[[!
    contents.isMagic = true
	state.magicLongComment = true
	contents.comment = string.sub(line, 6)
	contents.magicData = trim(contents.comment)
	contents.format = "%s/** %s"
  elseif string.find(line, "%-%-%[%[") == 1 then	-- begin with --[[
    contents.isComment = true
	state.LongComment = true
	contents.comment = string.sub(line, 5)
	contents.format = "%s/*%s"
  elseif string.find(line, "%-%-") == 1 then		-- begin with --
    contents.isComment = true
	contents.comment = string.sub(line, 3)
	contents.format = "%s//%s"

  -- following use non trimed line to keep indent info
  --! @todo more accurate comment detection
  elseif string.find(contents.line, "%-%-^%!") then		    -- comment
    p = string.find(contents.line, "%-%-^%!")
	contents.code = string.sub(contents.line, 1, p-1)
	contents.comment = string.sub(contents.line, p+3)
	contents.format = "%s//%s"
  elseif string.find(contents.line, "%-%-%!%<") then		    -- --!<
    p = string.find(contents.line, "%-%-%!%<")
	contents.isMagic = true
	contents.code = string.sub(contents.line, 1, p-1)
	contents.comment = string.sub(contents.line, p+4)
	contents.magicData = trim(contents.comment)
	contents.format = "%s//<%s"
  else
    contents.code = contents.line
    contents.format = "%s%s"
  end
  local state = nil
  return -- don't return done=true, leave more pre-processing
end

function Lua2DoxPostProcessorAll(filter, contents, line)
  return string.format(contents.format, default(contents.code, ""), default(contents.comment, "")).."\n", true
end


function loadContentFromFile(filter, fileName)
  local contents
  local done
  debugMsg("runFilterOnFile %s", fileName)
  filter.inputFileName = fileName 				-- store this information
  for line in io.lines(fileName) do
    contents = {} -- create the table
	contents.line = line
	for i, preprocessor in ipairs(filter.preprocessors) do
	  done = preprocessor.func(filter, contents)
	  if done then break end
	end
	table.insert(filter.filecontents, contents)
  end
  debugMsg("%s lines read", #filter.filecontents)
end

function outputToStream(filter, stream)
  local line, done
  debugMsg("outputToStream")
  filter.stream = stream 				-- store this information
  for i, contents in ipairs(filter.filecontents) do
    line = nil
	for i, postprocessor in ipairs(filter.postprocessors) do
	  line, done = postprocessor.func(filter, contents, line)
	  if done then break end
	end
	if line then
	  stream:write(line) -- it's the post processor job to add \n or else
	else
	  line = string.format("%s line %d post processor error or missing\n", arg[0], i)
	  stream:write(line) -- it's the post processor job to add \n or else
	  debugMsg(line)
	  return
	end
  end
end

function newFilter()
  local filter = {} 				-- holds all filter settings and states
  filter.filecontents = {}			-- holds data to process
  filter.preprocessors = {}			-- pre processing filters
  filter.preprocessorstate = {}		-- used by pre-processors
  filter.postprocessors = {}		-- post processing filters
  filter.postprocessorstate = {}	-- used by post-processors
  return filter
end

function addPreProcessor(filter, func)
  table.insert(filter.preprocessors, { func = func })
end

function addPostProcessor(filter, func)
  table.insert(filter.postprocessors, { func = func })
end

function main()
  debugMsg("Script name = %s", arg[0])
  for i,value in ipairs(arg) do
    debugMsg("arg[%d] = %s", i, value)
  end

  local job = newFilter()

  addPreProcessor(job, Lua2DoxPreProcessor)
  addPostProcessor(job, Lua2DoxPostProcessorAll)

  loadContentFromFile(job, arg[1])
  -- afterLoadEvent()
  -- beforeOutputEvent()
  outputToStream(job, io.stdout)
  -- afterOutputEvent()
end

main()
