--
-- xmonad config file
-- by mjheagle
--

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Util.Run
import Dzen

-- set terminal
myTerminal      = "urxvtc"

-- whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- width of the window border in pixels.
myBorderWidth   = 1

-- "windows key" is usually mod4Mask, left alt is mod1mask
myModMask       = mod4Mask

wkspcs          = zip (["web","media","chat"] ++ map show [4..5]) [1..5]
mkWkspcClkbl i  = "^ca(1, xdotool key super+" ++ show(snd i) ++ ")" ++ fst i ++ "^ca()"
myWorkspaces    = map mkWkspcClkbl wkspcs

-- border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#0055ff"

------------------------------------------------------------------------
-- key bindings. add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return),         spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ),         spawn "dmenu_run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ),         kill)
    , ((mod1Mask,           xK_F4    ),         kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ),         sendMessage NextLayout)
    , ((modm,               xK_backslash ),     sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ),         setLayout $ XMonad.layoutHook conf)

    -- go to previous workspace
    , ((modm,               xK_Tab   ),         toggleWS)

    -- move/shift windows left/right
    , ((modm,               xK_Left  ),         prevWS)
    , ((modm,               xK_Right ),         nextWS)
    , ((modm .|. shiftMask, xK_Left  ),         shiftToPrev)
    , ((modm .|. shiftMask, xK_Right ),         shiftToNext)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ),         refresh)

    -- Move focus to the next window
    , ((modm,               xK_j     ),         windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ),         windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ),         windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return),         windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ),         windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ),         windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ),         sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ),         sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ),         withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm,               xK_bracketleft),    sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm,               xK_bracketright),   sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    , ((modm .|. controlMask, xK_b   ),         sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ),         io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm,               xK_q     ),         spawn "xmonad --recompile && killall dzen2; xmonad --restart")

    -- lock
    , ((modm .|. shiftMask, xK_l     ),         spawn "xautolock -locknow")

    -- power
    , ((modm,               xK_Delete),         spawn "/home/mhiggin5/programs/bash/exit.sh")

    -- transdown
    , ((modm,               xK_Down  ),         spawn "transset-df -p --min 0.2 --dec 0.1")

    -- transup
    , ((modm,               xK_Up    ),         spawn "transset-df -p --inc 0.1")

    -- compositing
    , ((modm,               xK_o     ),         spawn "compositing toggle")

    -- chromium
    , ((modm,               xK_g     ),         spawn "/home/mhiggin5/programs/bash/launch.sh chromium")

    -- desktop ssh
    , ((modm,               xK_s     ),         spawn "urxvtc -title ssh -e /home/mhiggin5/programs/bash/ssh-arch-phoenix.sh")

    -- dmenucmd
    , ((modm,               xK_p     ),         spawn "dmenu_run")

    -- htop
    , ((modm,               xK_grave ),         spawn "urxvtc -title htop -e htop")
    , ((modm .|. shiftMask, xK_grave ),         runOrRaise "urxvtc -title htop -e htop" (title =? "htop"))

    -- minbrowser
    , ((modm,               xK_w     ),         spawn "dwb")

    -- mutt
    , ((modm,               xK_e     ),         spawn "/home/mhiggin5/programs/bash/launch.sh mutt")

    -- ranger
    , ((modm,               xK_d     ),         spawn "urxvtc -e ranger")

    -- weechat
    , ((modm,               xK_i     ),         spawn "/home/mhiggin5/programs/bash/launch.sh weechat")

    -- dmenuwatchvideo
    , ((modm .|. shiftMask, xK_y     ),         spawn "home/mhiggin5/programs/bash/flash-video-dmenu.sh")

    -- mediactrl
    , ((modm .|. shiftMask, xK_z     ),         spawn "/home/mhiggin5/programs/c/mediactrl/mediactrl -p")
    , ((modm .|. shiftMask, xK_c     ),         spawn "/home/mhiggin5/programs/c/mediactrl/mediactrl -t")
    , ((modm .|. shiftMask, xK_v     ),         spawn "/home/mhiggin5/programs/c/mediactrl/mediactrl -s")
    , ((modm .|. shiftMask, xK_b     ),         spawn "/home/mhiggin5/programs/c/mediactrl/mediactrl -n")

    -- mpd controls
    , ((modm,               xK_z     ),         spawn "mpc prev")
    , ((modm,               xK_c     ),         spawn "mpc toggle")
    , ((modm,               xK_v     ),         spawn "mpc stop")
    , ((modm,               xK_b     ),         spawn "mpc next")

    -- msearch
    , ((modm,               xK_r     ),         spawn "urxvtc -title msearch -e /home/mhiggin5/programs/python/msearch.py -k")

    -- ncmpcpp
    , ((modm,               xK_n     ),         spawn "/home/mhiggin5/programs/bash/launch.sh ncmpcpp")

    -- utub
    , ((modm,               xK_u     ),         spawn "urxvtc -title utub -e utub-curses")

    -- volume
    , ((modm,               xK_Next  ),         spawn "/home/mhiggin5/programs/bash/ossvol -d 1")
    , ((modm,               xK_Prior ),         spawn "/home/mhiggin5/programs/bash/ossvol -i 1")

    -- watchvideo
    , ((modm,               xK_y     ),         spawn "urxvtc -title fmplayer -e /home/mhiggin5/programs/python/flash-mplayer.py")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{comma,period,slash}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{comma,period,slash}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_slash] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Chromium"       --> doShift (myWorkspaces !! 0)
    , title     =? "ncmpcpp"        --> doShift (myWorkspaces !! 1)
    , title     =? "utub"           --> doShift (myWorkspaces !! 1)
    , title     =? "fmplayer"       --> doShift (myWorkspaces !! 1)
    , title     =? "msearch"        --> doShift (myWorkspaces !! 1)
    , title     =? "mutt"           --> doShift (myWorkspaces !! 2)
    , title     =? "weechat"        --> doShift (myWorkspaces !! 2)
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook h = dynamicLogWithPP $ defaultPP
    -- display current workspace inverted
    { ppCurrent         = dzenColor "#222222" "#AAAAAA" . pad

    -- display other workspaces which contain windows as brighter
    , ppHidden          = dzenColor "#AAAAAA" "" . pad

    -- display other workspaces with no windows darker
    , ppHiddenNoWindows = dzenColor "#777777" "" . pad

    -- display the current layout
    , ppLayout          = dzenColor "#777777" "" . pad

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#FF0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = pad . shorten 100

    -- no separator between workspaces
    , ppWsSep           = ""

    -- object separator 
    , ppSep             = "|"

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do 
    {- let dzenOpts = "-ta r -y 0 -xs 1 -bg \"#222222\" -fg \"#AAAAAA\" -fn \"-*-termsyn-medium-*-*-*-11-*-*-*-*-*-iso8859-2\"" -}
    {- d <- spawnPipe "dzen2 -ta r -y 0 -xs 1 -bg \"#222222\" -fg \"#AAAAAA\" -fn \"-*-termsyn-medium-*-*-*-11-*-*-*-*-*-iso8859-2\"" -}
    {- spawn "conky -c ~/.config/conky/arch-slaptop-xmonad | dzen2 -ta r -y 0 -xs 2 -bg \"#222222\" -fg \"#AAAAAA\" -fn \"-*-termsyn-medium-*-*-*-11-*-*-*-*-*-iso8859-2\"" -}
    d <- spawnDzen myLeftBar
    spawnToDzen "conky -c ~/.config/conky/arch-slaptop-xmonad" myRightBar
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,
        logHook            = myLogHook d,
        startupHook        = myStartupHook
        }

    where
        myLeftBar :: DzenConf
        myLeftBar = defaultDzen
            { bgColor   = Just "#222222"
            , fgColor   = Just "#AAAAAA"
            , font      = Just "-*-termsyn-medium-*-*-*-11-*-*-*-*-*-iso8859-2"
            }
        myRightBar :: DzenConf
        myRightBar = defaultDzen
            { bgColor   = Just "#222222"
            , fgColor   = Just "#AAAAAA"
            , screen    = Just 1
            , alignment = Just RightAlign
            , font      = Just "-*-termsyn-medium-*-*-*-11-*-*-*-*-*-iso8859-2"
            }
