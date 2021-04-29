-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.CycleWindows
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myMod :: KeyMask
myMod = mod4Mask        -- Sets modkey to super/windows key

myTerm :: String
myTerm = "termite"    -- Sets default terminal

myBrowser :: String
myBrowser = "brave"  -- Sets qutebrowser as browser

myEditor :: String
myEditor =  myTerm ++ " -e vim" --"emacsclient -c -a emacs "  -- Sets emacs as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"   -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask          -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "lxsession &"
	  spawnOnce "numlockx on &"
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom &"
          spawnOnce "nm-applet &"
          spawnOnce "volumeicon &"
          spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 256 --height 22 &"
	  spawnOnce "caffeine"
	  spawnOnce "xfce4-power-manager"
          --spawnOnce "conky -c $HOME/.config/conky/chimera.conkyrc" -- emacs daemon for the emacsclient
          --spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
          -- spawnOnce "kak -d -s mysession &"  -- kakoune daemon for better performance
          -- spawnOnce "urxvtd -q -o -f &"      -- urxvt daemon for better performance
          setWMName "LG3D"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }


-- two grids
myAppGrid = [
	("Brave", "brave"),
	("LibreWolf", "librewolf"),
	("Virt-Manager", "virt-manager"),
	("Music", myTerm ++ " -e ncmpcpp"),
	("CAD", "freecad"),
	("Games", "steam"),
	("Videolezioni", "librewolf https://idp.polito.it")
	      ]

myPowerMenu = [
	("Shutdown", "systemctl poweroff"),
	("Suspend", "systemctl suspend"),
	("Reboot", "systemctl reboot"),
	("Hibernate", "systemctl hibernate"),
	("Lock", "i3exit blurlock")
	     ]

-- Various scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm,
		  NS "music" spawnMusic findMusic manageTerm,
		  NS "nvtop" spawnNVT findNVT manageTerm,
		  NS "htop" spawnHT findHT manageTerm
		]
    where 
	spawnTerm  = myTerm ++ " -t scratchpad"
	findTerm   = title =? "scratchpad"
	manageTerm = customFloating $ W.RationalRect l t w h 
		where 
			h = 0.9
			w = 0.9
			t = 0.95 -h
			l = 0.95 -w
	spawnMusic = myTerm ++ " -t Music -e ncmpcpp"
	spawnNVT   = myTerm ++ " -t NVidiaTop -e nvtop"
	spawnHT    = myTerm ++ " -t Htop -e htop"
	findMusic  = title =? "Music"
	findNVT    = title =? "NVidiaTop"
	findHT     = title =? "Htop"

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts
tall      = renamed [Replace "tall"]
	    $ windowNavigation
	    $ smartBorders
	    $ addTabs shrinkText myTabTheme
	    $ subLayout [] (smartBorders Simplest)
	    $ limitWindows 12
	    $ mySpacing 8
	    $ ResizableTall 1 (3/300) (1/2) []
magnify   = renamed [Replace "magnify"]
	    $ windowNavigation
	    $ smartBorders
	    $ addTabs shrinkText myTabTheme
	    $ subLayout [] (smartBorders Simplest)
	    $ magnifier
	    $ limitWindows 12
	    $ mySpacing 8
	    $ ResizableTall 1 (3/300) (1/2) []
threeRow  = renamed [Replace "threeRow"]
	    $ windowNavigation
	    $ smartBorders
	    $ addTabs shrinkText myTabTheme
	    $ subLayout [] (smartBorders Simplest)
	    $ limitWindows 3 
	    $ mySpacing 8
	    $ ThreeCol 1 (3/300) (1/2) 
floats    = renamed [Replace "floats"]
	    $ smartBorders
	    $ limitWindows 20 simplestFloat
tabs      = renamed [Replace "tabs"]
	    $ tabbed shrinkText myTabTheme

myTabTheme = def {
	fontName            = myFont,
	activeColor	    = "#46d9ff",
	inactiveColor	    = "#313846",
	activeBorderColor   = myFocusColor,
	inactiveBorderColor = "#282c34",
	activeTextColor	    = "#282c34",
	inactiveTextColor   = "#d0d0d0"
		  }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| magnify
                               --  ||| noBorders monocle
                                 ||| floats
                                 ||| noBorders tabs
                               --  ||| grid
                               --  ||| spirals
                               --  ||| threeCol
                                 ||| threeRow

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces = [" I ", " II ", " III ", " IV ", " V ", " VI ", " VII ", " IIX ", " IX "] -- ++ ["NSP"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "mpv"     --> doShift ( myWorkspaces !! 7 )
     , className =? "Gimp"    --> doShift ( myWorkspaces !! 8 )
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] <+> namedScratchpadManageHook myScratchPads


-- my keybindings
myKeys :: [(String, X ())]
myKeys = 
	[
		("M-C-r", spawn("xmonad --recompile && xmonad --restart")), --recompiles and restarts xmonad
		("M-C-q", io exitSuccess), --quits xmonad
		
		--two run prompts
		("C-<Space>", spawn "rofi -show combi"),
		("C-d", spawn "dmenu_run -i -p \"Run: \""),
		

		--window and navigation
		("M-S-q", kill1), --kills focused window
		("M-S-a", killAll), --kills all windows in focused workpace
		("M-.", nextScreen), --moves focus to other monitor
		("M-,", prevScreen), -- same
		("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP), --moves current window to next ws
		("M-S-<KP_Substract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP), --moves current window to prev ws
		
		("M-t", withFocused $ windows . W.sink), --retiles focused floating window
		
		("M-d", decWindowSpacing 4), --decrease windows spacing	
		("M-i", incWindowSpacing 4), --increase windows spacing

		("M-j", windows W.focusDown), --focus to prev window
		("M-k", windows W.focusUp), --focus to next window
		("M-S-m", windows W.swapMaster), --promotes current window to master
		("M-S-j", windows W.swapDown), --swap with next window
		("M-S-k", windows W.swapUp), --swap with prev window
		("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab), --classic Alt-Tab
		("M-<Tab>", sendMessage NextLayout), --cycles layouts
		("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), --fulls
		
		("M-h", sendMessage Shrink), --shrink horizontal
		("M-l", sendMessage Expand), --grow horizontal
		("M-M1-j", sendMessage MirrorShrink), --shrink vertical
		("M-M1-k", sendMessage MirrorExpand), --grow vertical

		--My applications
		("M-<Return>", spawn(myTerm)), --spawn terminal
		("M-<KP_Enter>", spawn(myTerm)),
		("C-b", spawn(myBrowser)), --spawn browser
		("M-C-e", spawn "~/scripts/power_menu.sh"), --launches dmenu prompt with various power options
		--("M-S-s" spawn "steam"), --launches steam
		("M-w", spawn "qutebrowser"), --launches qutebrowser
		("C-f", spawn "librewolf"), --launches librewolf
		("M-S-f", spawn(myTerm ++ " -e ranger")), --launches terminal file manager
		--("<Print>", spawn "scrot -m ~/storage/Pictures/screenshots") --does a screenshots and saves it in the path 
		
		--Functions Keys
		("<F2>", spawn "xbacklight -inc -10"), --decrease brightness
		("<F3>", spawn "xbacklight -inc +10"), --increase brightness
		("<F4>", spawn "~/scripts/search.sh"), --launches search prompt
		("<F5>", spawn "i3exit lock"), --locks screen
		("<F6>", spawn "pactl set-sink-mute $(pactl list short sinks | awk '{print $(1)}') toggle"), --set mute
		("<F7>", spawn "pactl set-sink-volume $(pactl list short sinks | awk '{print $(1)}') -5%"), --dec volume
		("<F8>", spawn "pactl set-sink-volume $(pactl list short sinks | awk '{print $(1)}') +5%"), --inc volume 
		("<F9>", spawn (myTerm ++ " -e ncmpcpp")), --launches music player
		("<F10>", spawn "mpc prev"), --plays previous song
		("<F11>", spawn "mpc toggle"), --toggles play/pause
		("<F12>", spawn "mpc next"), --plays next song

		
		-- Grids
		("C-g g", spawnSelected' myAppGrid),
		("C-g b", bringSelected $ mygridConfig myColorizer), --brings selected window to master of WS
		("C-g t", goToSelected $ mygridConfig myColorizer), --switch to selected window
		("C-g p", spawnSelected' myPowerMenu),

		-- Scratchpads
		("M-s <Return>", namedScratchpadAction myScratchPads "terminal"), --launches terminal
		("M-s m", namedScratchpadAction myScratchPads "music"), --launches ncmpcpp
		("M-s n", namedScratchpadAction myScratchPads "nvtop"), --launches nvtop
		("M-s h", namedScratchpadAction myScratchPads "htop") --launches htop
	]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/myxmobar0"
    xmproc1 <- spawnPipe "xmobar $HOME/.config/xmobar/myxmobar1"
    xmproc2 <- spawnPipe "xmobar ~/.config/xmobar/xmobarBottom0"
    xmproc3 <- spawnPipe "xmobar ~/.config/xmobar/xmobarBottom1"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
                               -- <+> fullscreenEventHook  -- this does NOT work right if using multi-monitors!
        , modMask            = myMod
        , terminal           = myTerm
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"           -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" "" . clickable              -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""  . clickable     -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60               -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                        , ppExtras  = [windowCount]                                     -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
