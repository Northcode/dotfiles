local ok, dap = pcall(require, "dap")
if not ok then
	return
end

local util = require("lspconfig.util")

local root_files = {
	-- Single-module projects
	{
		"build.xml", -- Ant
		"pom.xml", -- Maven
		"settings.gradle", -- Gradle
		"settings.gradle.kts", -- Gradle
	},
	-- Multi-module projects
	-- { "build.gradle", "build.gradle.kts" },
}

local contains = function(t, e)
	if next(t) == nil then
		return false
	end
	for _, v in ipairs(t) do
		if v == e then
			return true
		end
	end
	return false
end

local function resolve_classname()
	--[[ if (vim.bo.filetype ~= "kotlin") then
    print("filetype")
    print(vim.bo.filetype)
    return
  end ]]
	local root_dir = util.root_pattern(root_files)(vim.fn.fnamemodify(vim.fn.expand("%"), ":p:h"))
	if root_dir == nil then
		return
	end

	local grep_res = vim.api.nvim_exec('! grep "fun main(arg[sv]: Array<String>)" -r ' .. root_dir, true)
	if not grep_res or string.match(grep_res, "shell returned 1") then
		vim.notify("Unable to find main func", vim.log.levels.WARN)
		return
	end

	local files = {}
	local mainfile, pkgname

	for f in string.gmatch(grep_res, "([%w+%p]+[%w+.]kt)") do
		if not contains(files, f) then
			table.insert(files, f)
		end
	end

	if #files > 1 then
		-- for _, f in ipairs(files) do
		--   if string.find(f, "Main") then
		--     mainfile = f
		--     vim.print(mainfile)
		--     break
		--   end
		-- end
		--
		-- if not mainfile then
		--   for _, f in ipairs(files) do
		--     if string.find(f, "Application") then
		--       mainfile = f
		--       break
		--     end
		--   end
		-- end
		-- if not mainfile then
		--   vim.notify("Multiple files contain 'fun main'", vim.log.levels.ERROR)
		--   return
		-- end
		return
	else
		mainfile = files[1]
	end
	assert(mainfile, "Could not find a file matching 'fun main(args: Array<String>)'")

	for line in io.lines(mainfile) do
		local match = line:match("package ([a-z\\.]+)")
		if match then
			pkgname = match
			break
		end
	end
	assert(pkgname, "Could not find package name for current class")
	local classname = pkgname .. "." .. vim.fn.fnamemodify(mainfile, ":t:r") .. "Kt"
	print("mainClass: " .. classname)
	return classname
end

dap.defaults.kotlin.auto_continue_if_many_stopped = false

dap.adapters.kotlin = {
	type = "executable",
	command = "kotlin-debug-adapter",
	options = {
		initialize_timeout_sec = 60,
		disconnect_timeout_sec = 60,
	},
}

dap.configurations.kotlin = {
	{
		name = "kotlin: Launch",
		type = "kotlin",
		request = "launch",
		projectRoot = function()
			local root = util.root_pattern(root_files)(vim.fn.fnamemodify(vim.fn.expand("%"), ":p:h"))
			print("projectRoot: " .. root)
			return root
		end,
		mainClass = function()
			return resolve_classname()
		end,
		jsonLogFile = "/home/andreas/kotlin-dap.log.json",
		enableJsonLogging = true,
	},
	{
		name = "kotlin: Attach",
		type = "kotlin",
		request = "attach",
		hostName = "127.0.0.1",
		port = 5005,
		timeout = 20,
		projectRoot = util.root_pattern(root_files)(vim.fn.fnamemodify(vim.fn.expand("%"), ":p:h")),
		mainClass = function()
			return resolve_classname()
		end,
	},
}
