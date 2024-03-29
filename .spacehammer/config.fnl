;; Copyright (c) 2017-2020 Ag Ibragimov & Contributors
;;
;;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;;
;;; Contributors:
;;   Jay Zawrotny <jayzawrotny@gmail.com>
;;
;;; URL: https://github.com/agzam/spacehammer
;;
;;; License: MIT
;;


(require-macros :lib.macros)
(local windows (require :windows))
(local slack (require :slack))

(local {:concat concat
        :logf logf} (require :lib.functional))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WARNING
;; Make sure you are customizing ~/.spacehammer/config.fnl and not
;; ~/.hammerspoon/config.fnl
;; Otherwise you will lose your customizations on upstream changes.
;; A copy of this file should already exist in your ~/.spacehammer directory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of Contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [x] w - windows
;; [x] |-- w - Last window
;; [x] |-- cmd + hjkl - jumping
;; [x] |-- hjkl - halves
;; [x] |-- alt + hjkl - increments
;; [x] |-- shift + hjkl - resize
;; [x] |-- n, p - next, previous screen
;; [x] |-- shift + n, p - up, down screen
;; [x] |-- g - grid
;; [x] |-- m - maximize
;; [x] |-- c - center
;; [x] |-- u - undo
;;
;; [x] a - apps
;; [x] |-- e - emacs
;; [x] |-- g - chrome
;; [x] |-- f - firefox
;; [x] |-- i - iTerm
;; [x] |-- s - Slack
;; [x] |-- b - Safari
;;
;; [x] j - jump
;;
;; [x] m - media
;; [x] |-- h - previous track
;; [x] |-- l - next track
;; [x] |-- k - volume up
;; [x] |-- j - volume down
;; [x] |-- s - play\pause
;; [x] |-- a - launch player
;;
;; [x] cmd-n - next-app
;; [x] cmd-p - prev-app

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activator
  [app-name]
  "
  A higher order function to activate a target app. It's useful for quickly
  binding a modal menu action or hotkey action to launch or focus on an app.
  Takes a string application name
  Returns a function to activate that app.

  Example:
  (local launch-emacs (activator \"Emacs\"))
  (launch-emacs)
  "
  (fn activate []
    (windows.activate-app app-name)))

(fn toggle-console
  []
  "
  A simple action function to toggle the hammer spoon console.
  Change the keybinding in the common keys section of this config file.
  "
  (if-let [console (hs.console.hswindow)]
   (hs.closeConsole)
   (hs.openConsole)))

(fn lock-screen
  []
  "
  Function that locks screen
  "
  (fn activate []
    (hs.caffeinate.lockScreen)))

(fn insert-text
  [text]
  "
  Send text to current window
  "
  (fn a []
    (hs.eventtap.keyStrokes text)))


(fn web-open
  [thing]
  "
  Function to open urls in safari
  "
  (fn activate []
    (windows.activate-app "Safari")
    (hs.osascript.applescript (.. "
        tell application \"Safari\"
            tell window 1
                set current tab to (make new tab with properties {URL:\"" thing "\"})
            end tell
        end tell
        "))))

(fn mute-meet
  []
  "
  Mute/unmute meet running in chrome
  "
  (hs.alert.show "🔇")
  (hs.osascript.applescript "
    tell application \"Google Chrome\"
    activate
    repeat with theWindow in windows
      set i to 0
      repeat with theTab in tabs of theWindow
        set i to i + 1
        if URL of theTab starts with \"https://meet.google.com\" then
          set index of theWindow to 1
          set active tab index of theWindow to i
          tell application \"System Events\" to keystroke \"d\" using command down
          return
        end if
      end repeat
    end repeat
  end tell
  "))

(fn toggle-dark
  []
  "
  Toggle dark mode
  "
  (hs.osascript.applescript "
  tell application \"System Events\"
    tell appearance preferences
        set dark mode to not dark mode
    end tell
  end tell
  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you would like to customize this we recommend copying this file to
;; ~/.hammerspoon/private/config.fnl. That will be used in place of the default
;; and will not be overwritten by upstream changes when spacehammer is updated.
(local music-app "Music")

(local return
       {:key :space
        :title "Back"
        :action :previous})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local window-jumps
       [{:mods [:cmd]
         :key "hjkl"
         :title "Jump"}
        {:mods [:cmd]
         :key :h
         :action "windows:jump-window-left"
         :repeatable true}
        {:mods [:cmd]
         :key :j
         :action "windows:jump-window-above"
         :repeatable true}
        {:mods [:cmd]
         :key :k
         :action "windows:jump-window-below"
         :repeatable true}
        {:mods [:cmd]
         :key :l
         :action "windows:jump-window-right"
         :repeatable true}])

(local window-halves
       [{:key "hjkl"
         :title "Halves"}
        {:key :h
         :action "windows:resize-half-left"
         :repeatable true}
        {:key :j
         :action "windows:resize-half-bottom"
         :repeatable true}
        {:key :k
         :action "windows:resize-half-top"
         :repeatable true}
        {:key :l
         :action "windows:resize-half-right"
         :repeatable true}])

(local window-increments
       [{:mods [:alt]
         :key "hjkl"
         :title "Increments"}
        {:mods [:alt]
         :key :h
         :action "windows:resize-inc-left"
         :repeatable true}
        {:mods [:alt]
         :key :j
         :action "windows:resize-inc-bottom"
         :repeatable true}
        {:mods [:alt]
         :key :k
         :action "windows:resize-inc-top"
         :repeatable true}
        {:mods [:alt]
         :key :l
         :action "windows:resize-inc-right"
         :repeatable true}])

(local window-resize
       [{:mods [:shift]
         :key "hjkl"
         :title "Resize"}
        {:mods [:shift]
         :key :h
         :action "windows:resize-left"
         :repeatable true}
        {:mods [:shift]
         :key :j
         :action "windows:resize-down"
         :repeatable true}
        {:mods [:shift]
         :key :k
         :action "windows:resize-up"
         :repeatable true}
        {:mods [:shift]
         :key :l
         :action "windows:resize-right"
         :repeatable true}])

(local window-move-screens
       [{:key "n, p"
         :title "Move next\\previous screen"}
        {:mods [:shift]
         :key "n, p"
         :title "Move up\\down screens"}
        {:key :n
         :action "windows:move-south"
         :repeatable true}
        {:key :p
         :action "windows:move-north"
         :repeatable true}
        {:mods [:shift]
         :key :n
         :action "windows:move-west"
         :repeatable true}
        {:mods [:shift]
         :key :p
         :action "windows:move-east"
         :repeatable true}])

(local window-bindings
       (concat
        [return
         {:key :w
          :title "Last Window"
          :action "windows:jump-to-last-window"}]
        window-jumps
        window-halves
        window-increments
        window-resize
        window-move-screens
        [{:key :m
          :title "Maximize"
          :action "windows:maximize-window-frame"}
         {:key :f
          :title "Toggle fullscreen"
          :action (fn [] (: (hs.window.focusedWindow) :toggleFullScreen))}
         {:key :q
          :title "Force quit current application"
          :action (fn [] (: (: (hs.window.focusedWindow) :application) :kill9))}
         {:key :c
          :title "Center"
          :action "windows:center-window-frame"}
         {:key :g
          :title "Grid"
          :action "windows:show-grid"}
         {:key :u
          :title "Undo"
          :action "windows:undo-action"}]))

(local text-bindings
       [return
        {:key :1
         :title "👍"
         :action (insert-text "👍")}
        {:key :2
         :title "😀"
         :action (insert-text "😀")}
        {:key :3
         :title "🤣"
         :action (insert-text "🤣")}
        {:key :4
         :title "❤️"
         :action (insert-text "❤️")}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local app-bindings
       [return
        {:key :e
         :title "Emacs"
         :action (activator "Emacs")}
        {:key :g
         :title "Chrome"
         :action (activator "Google Chrome")}
        {:key :f
         :title "Firefox"
         :action (activator "Firefox")}
        {:key :t
         :title "iTerm"
         :action (activator "iTerm2")}
        {:key :s
         :title "Slack"
         :action (activator "Slack")}
        {:key :z
         :title "Teams"
         :action (activator "Microsoft Teams")}
        {:key :c
         :title "Calendar"
         :action (activator "Calendar")}
        {:key :d
         :title "Dash"
         :action (activator "Dash")}
        {:key :x
         :title "Mail"
         :action (activator "Mail")}
        {:key :o
         :title "Bitwarden"
         :action (activator "Bitwarden")}
        {:key :b
         :title "Safari"
         :action (activator "Safari")}
        {:key :n
         :title "Notes"
         :action (activator "Notes")}
        {:key :w
         :title "Discord"
         :action (activator "Discord")}
        {:key :a
         :title "Authy Desktop"
         :action (activator "Authy Desktop")}
        {:key :r
         :title "TablePlus"
         :action (activator "TablePlus")}
        {:key :v
         :title "Reminders"
         :action (activator "Reminders")}
        {:key :m
         :title music-app
         :action (activator music-app)}])

(local media-bindings
       [return
        {:key :s
         :title "Play or Pause"
         :action "multimedia:play-or-pause"}
        {:key :h
         :title "Prev Track"
         :action "multimedia:prev-track"}
        {:key :l
         :title "Next Track"
         :action "multimedia:next-track"}
        {:key :j
         :title "Volume Down"
         :action "multimedia:volume-down"
         :repeatable true}
        {:key :k
         :title "Volume Up"
         :action "multimedia:volume-up"
         :repeatable true}
        {:key :a
         :title (.. "Launch " music-app)
         :action (activator music-app)}])

(local system-bindings
       [return
        {:key :l
         :title "Lock"
         :action (lock-screen)}
        {:key :s
         :title "Screenshot"
         :action (activator "Screenshot")}
        {:key :p
         :title "System Preferences"
         :action (activator "System Preferences")}
        {:key :d
         :title "Dark mode"
         :action toggle-dark}
        {:key :a
         :title "Activity Monitor"
         :action (activator "Activity Monitor")}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Menu & Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(local menu-items
       [{:key :p
         :title "System"
         :items system-bindings}
        {:key :w
         :title "Window"
         :items window-bindings}
        {:key :a
         :title "Apps"
         :items app-bindings}
        {:key :j
         :title "Jump"
         :action "windows:jump"}
        {:key :e
         :title "Emacs"
         :action (activator "Emacs")}
        {:key :0
         :title "Mute meet"
         :action mute-meet}
        {:key :t
         :title "iTerm"
         :action (activator "iTerm")}
        {:key :s
         :title "Safari"
         :action (activator "Safari")}
        {:key :q
         :title "Quit current application"
         :action (fn [] (: (: (hs.window.focusedWindow) :application) :kill))}
        {:key :i
         :title "Insert text"
         :items text-bindings}
        {:key :m
         :title "Music"
         :items media-bindings}])

(local common-keys
       [{:mods [:alt :ctrl]
         :key :space
         :action "lib.modal:activate-modal"}
        {:mods []
         :key :f18
         :action mute-meet}
        {:mods []
         :key :f20
         :action "lib.modal:activate-modal"}
        {:mods [:cmd :ctrl]
         :key "`"
         :action toggle-console}
        {:key :f19
         :action (lock-screen)}
        {:mods [:ctrl :option]
         :key :l
         :title "Lock"
         :action (lock-screen)}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local browser-keys
       [{:mods [:ctrl :shift]
         :key :k
         :action (fn [] (hs.eventtap.keyStroke [] :pageup))}
        {:mods [:ctrl :shift]
         :key :j
         :action (fn [] (hs.eventtap.keyStroke [] :pagedown))}
        {:mods [:cmd :shift]
         :key :l
         :action "chrome:open-location"}
        {:mods [:alt]
         :key :k
         :action "chrome:next-tab"}
        {:mods [:alt]
         :key :j
         :action "chrome:prev-tab"}])

(local safari-config
       {:key "Safari"
        :keys browser-keys})

(local chrome-config
       {:key "Google Chrome"
        :keys browser-keys })

(local firefox-config
       {:key "Firefox"
        :keys browser-keys})

(local vscode-keys
       [{:mods [:ctrl]
         :key "["
         :action (fn [] (hs.eventtap.keyStroke [] :escape))}])

(local vscode-config
       {:key "Code"
        :keys vscode-keys})

(local hammerspoon-config
       {:key "Hammerspoon"
        :items (concat
                menu-items
                [{:key :r
                  :title "Reload Console"
                  :action hs.reload}
                 {:key :c
                  :title "Clear Console"
                  :action hs.console.clearConsole}])
        :keys []})


(local discord-keys
       [{:mods [:ctrl]
         :key "["
         :action (fn [] (hs.eventtap.keyStroke [] :escape))}])

(local discord-config
       {:key "Discord"
        :keys discord-keys})

(local slack-config
       {:key "Slack"
        :keys [{:mods [:alt]
                :key  :g
                :action "slack:scroll-to-bottom"}
               {:mods [:alt]
                :key :h
                :action "slack:prev-element"}
               {:mods [:alt]
                :key :l
                :action "slack:next-element"}
               {:mods [:alt]
                :key :t
                :action "slack:thread"}
               {:mods [:alt]
                :key :p
                :action "slack:prev-day"}
               {:mods [:alt]
                :key :n
                :action "slack:next-day"}
               {:mods [:ctrl :shift]
                :key :k
                :action "slack:scroll-up"
                :repeat true}
               {:mods [:ctrl :shift]
                :key :j
                :action "slack:scroll-down"
                :repeat true}]})

(local apps
       [safari-config
        chrome-config
        firefox-config
        vscode-config
        hammerspoon-config
        discord-config
        slack-config])

(local config
       {:title "Main Menu"
        :items menu-items
        :keys common-keys
        :enter (fn [] (windows.hide-display-numbers))
        :exit  (fn [] (windows.hide-display-numbers))
        :apps apps
        :hyper {:key :F18}
        :modules {:windows {:center-ratio "80:50"}}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

config
