
on run (argv)
	--set argv to {"/Users/peter/analysis/ConcentricDirectionQuest/demo_counter.avi", "/Users/peter/analysis/ConcentricDirectionQuest/demo_counter.mov", "/Users/peter/analysis/ConcentricDirectionQuest/demo_counter.settings"}
	tell application "QuickTime Player"
		activate
		if (argv contains "--savesettings") then
			«event MVWRsxps» with «class repl» given «class expk»:«constant expkMooV», «class expd»:((item 3 of argv) as POSIX file)
		else
			close every document
			set theDoc to open ((item 1 of argv) as POSIX file)
			with timeout of 3600 seconds
				try
					export (item 1 of theDoc) with «class repl» given «class expd»:((item 2 of argv) as POSIX file), «class expk»:«constant expkMooV», «class setf»:((item 3 of argv) as POSIX file)
				on error
					export (item 1 of theDoc) with «class repl» given «class expd»:((item 2 of argv) as POSIX file), «class expk»:«constant expkMooV»
				end try
			end timeout
			close item 1 of theDoc
		end if
	end tell
end run