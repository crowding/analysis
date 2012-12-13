--use this script snippet to save the most recent compression settings from Quicktime Player.

tell application "QuickTime Player 7"
	activate
	tell document 1
		save export settings for QuickTime movie to POSIX file "/Users/peter/analysis/demo_kitaoka.settings"
	end tell
end tell