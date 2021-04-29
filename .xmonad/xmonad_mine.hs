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
myMod = mod4Mask

myTerm :: String
myTerm = "termite"

myBrowser :: String
myBrowser = "brave"

myEditor :: String
myEditor = myTerm + " -e vim"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#282c34"

myFocusColor :: String
myFocusColor = "#ee4266"

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartUp :: X ()
myStartUp = do
	spawnOnce "lxsession &"
	spawnOnce "nitrogen --restore &"
	spawnOnce "picom &"
	spawnOnce "nm-applet &"
	spawnOnce "volumeicon &"
	spawnOnce "trayer -edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34 --height 22 &"
	spawnOnce "xfce4-power-manager &"
	setWMName "LG3D"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x28,0x2c,0x34) --lowest inactive background
	(0x28,0x2c,0x34) --highest inactive baackground
	(0xc7,0x92,0xea) --active bg
	(0xc0,0xa7,0x9a) --inactive fg
	(0x28,0x2c,0x34) --active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight   = 40,
	  gs_cellwidth    = 200,
	  gs_cellpadding  = 6,
	  gs_originFractX = 0.5,
	  gs_originFractY = 0.5,
	  gs_font  	  = myFont
	}

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
		{ gs_cellheight    = 40,
		  gs_cellwidth     = 200,
		  gs_cellpadding   = 6,
		  gs_originFractX  = 0.5,
		  gs_originFractY  = 0.5,
		  gs_font          = myFont
		}
-- two grids
myAppGrid = [
	("Brave", "brave"),
	("LibreWolf", "librewolf"),
	("Virt-Manager", "virt-manager"),
	("Music", myTerm + " -e ncmpcpp"),
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
		  NS "music" spawnTerm findTerm manageTerm,
		  NS "nvtop" spawnTerm findTerm manageTerm,
		  NS "htop" spawnTerm findTerm manageTerm
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

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i ) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i ) True (Border i i i i) True


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


--theme for ShowWMName
mySWMNTheme :: SWNConfig
mySWMNTheme = def {
	swn_font    = "xft:Ubuntu:bold:size=60",
	swn_fade    = 1.0,
	swn_bgcolor = "#1c1f24",
	swn_color   = "#ffffff"
		  }

-- Layout hook
myLayoutHook = avoidStruts $mouseResize $ windowArrange $ T.toggleLayouts floats
	       $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
		where
		 myDefaultLayout =  tall
				||| magnify
				||| noBorders tabs
				||| threeRow

myWorkspaces = [" I ", " II ", " III ", " IV ", " V ", " VI ", " VII ", " IIX ", " IX "]
myWorkspacesIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
	
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"<action>"
	where i = fromJust $ M.lookup ws myWorkspacesIndices


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
		("M1-s" spawn "steam"), --launches steam
		("M-w", spawn "qutebrowser"), --launches qutebrowser
		("C-f", spawn "librewolf"), --launches librewolf
		("M-S-f", spawn(myTerm ++ " -e ranger")), --launches terminal file manager
		("<Print>", spawn "scrot -m ~/storage/Pictures/screenshots") --does a screenshots and saves it in the path 
		
		--Functions Keys
		("<F2>", spawn "xbacklight -inc -10"), --decrease brightness
		("<F3>", spawn "xbacklight -inc +10"), --increase brightness
		("<F4>", spawn "~/scripts/search.sh"), --launches search prompt
		("<F5>", spawn "i3exit blurlock"), --locks screen
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
		where nonNSP	     = WSIs (return (\ws -> W.tag ws /= "NSP"))
		      nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))


main :: IO()
main = do
	xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc0"
	xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc1"
	xmonad $ ewmh def {
	
		manageHook         =  ( isFullscreen --> doFullFloat ) <+> manageDocks,   -- <+>myManageHooks before manageDocks
		handleEventHook    = serverModeEventHookCmd
				     <+> serverModeEventHook
				     <+> serverModeEventHookF "XMONAD__PRINT" (io . putStrLn)
				     <+> docksEventHook,
		modMask            = myMod,
		terminal	   = myTerm,
		startupHook 	   = myStartUp,
		layoutHook	   = showWMName' mySWMNTheme $ myLayoutHook,
		workspaces	   = myWorkspaces,
		borderWidth	   = myBorderWidth,
		normalBorderColor  = myNormColor,
		focusedBorderColor = myFocusColor,
		logHook		   = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
			{
				ppOutput  = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
				ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]",
				ppVisible = xmobarColor "#98be65" "" . clickable,
				ppHidden  = xmobarColor "#82aaff" "" . wrap "*" "" . clickable,
				ppHiddenNoWindows = xmobarColor "#c792ea" "" . clickable,
				ppTitle   = xmobarColor "#b3afc2" "" . shorten 60,
				ppSep	  = "<fc=#666666> <fn=1>|</fn> </fc>",
				ppUrgent  = xmobarColor "#c45500" "" . wrap "!" "!",
				ppExtras  = [windowCount],
				ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]
			}
	} `additionalKeysP` myKeys
