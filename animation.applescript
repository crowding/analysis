
on run (argv)
	--set argv to {"demo_crowding.avi", "demo_crowding.mov", "demo_crowding.settings", "/Users/peter/analysis"}
	set item 1 of argv to (item 4 of argv & "/" & item 1 of argv)
	set item 2 of argv to (item 4 of argv & "/" & item 2 of argv)
	set item 3 of argv to (item 4 of argv & "/" & item 3 of argv)
	tell application "QuickTime Player 7"
		activate
		if (argv contains "--savesettings") then
			save export settings for QuickTime movie to ((item 3 of argv) as POSIX file) with replacing
		else
			close every document
			set theDoc to open ((item 1 of argv) as POSIX file)
			with timeout of 3600 seconds
				try
					export (item 1 of theDoc) to ((item 2 of argv) as POSIX file) as QuickTime movie using settings ((item 3 of argv) as POSIX file) with replacing
				on error
					export (item 1 of theDoc) to ((item 2 of argv) as POSIX file) as QuickTime movie with replacing
				end try
			end timeout
			close item 1 of theDoc
		end if
	end tell
end run
