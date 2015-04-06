;;; ahk-mode.el --- Major mode for editing AHK (AutoHotkey and AutoHotkey_L)

;; Copyright (C) 2015 by Rich Alesi

;; Author: Rich Alesi
;; URL: https://github.com/ralesi/ahk-mode
;; Version: 1.5.3
;; Keywords: ahk, AutoHotkey, hotkey, keyboard shortcut, automation

;; Based on work from
;; xahk-mode - Author:   Xah Lee ( http://xahlee.org/ ) - 2012
;; ahk-mode - Author:   Robert Widhopf-Fenk

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software Foundation; either
;; GPL version 2 or 3.

;;; Commentary:

;; A major mode for editing AutoHotkey (AHK) script. Supports commenting,
;; indentation, syntax highlighting, and help lookup both localling and on
;; the web.
;; Features
;; - Commenting - provide functions for block and standard commenting
;; - Imenu - jump to a function / label within a buffer
;; - Execute scripts
;; - Auto complete - adds options for `company-mode' and `auto-complete-mode'

;; TODO:
;; - Movement - move between labels and functions
;; - Indentation - indent based on current style in ahk-chm
;; - Lookup reference - both on the web and through the installed CHM file
;; - Execute scripts - support redirects of error to stdout
;; - Debugging features

;; Notes on indentation
;; Indentation is styled with bracing on current line of if / else statements
;; or on empty next line.

;; Block types that can affect indentation
;; comments - ; AAA
;; - previous block beginning brace = +0
;; - indentation level is skipped when determining position for current line
;; function - AAA(.*) { ... } = +1
;; function - AAA(.*) { } = +0 ... } = +1
;; label - AAA: = 0
;; Keybindings (next line) AAA:: = +1
;; Keybindings (current line) AAA:: =+0
;; Open block {( +1 on next
;; Close block {( -1 on current
;; Class AAA.* { ... } = +1
;; #if[WinActive] (.*)\n = +1 
;; #if[WinActive]$ = -1
;; [Rr]eturn = -1
;; for .*\n { .. } = +1
;; loop .*\n { .. } = +1

;;; INSTALL

;; Open the file, then type “Alt+x eval-buffer”. You are done. Open
;; any ahk script, then type “Alt+x ahk-mode”, you'll see the
;; source code syntax colored.

;; To have emacs automatically load the file when it restarts, and
;; automatically use the mode when opening files ending in “.ahk”, do this:

;; This package is located within Melpa.  To install, add 
;; ("melpa" . "http://melpa.org/packages/") to package-archives and
;; execute "M-x package-install > ahk-mode"

;;; FEATURES

;; When opening a script file you will get:
;; - syntax highlighting
;; - indentation and command help
;; - autocomplete and company support

;;; HISTORY

;; version 1.5.2, 2015-03-07 improved auto complete to work with ac and company-mode
;; version 1.5.3, 2015-04-05 improved commenting and added imenu options

;;; Code:



;;; Compatibility
(eval-and-compile
  ;; `defvar-local' for Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))

;;; Requirements

(eval-when-compile
  (require 'font-lock)
  (require 'cl)
  (require 'thingatpt )
  (require 'rx)
  (if (fboundp 'auto-complete)
      (require 'auto-complete-config)))

;;; Customization

(defconst ahk-mode-version "1.5.3"
  "Version of `ahk-mode'")

(defgroup ahk-mode nil
  "Major mode for editing AutoHotkey script."
  :group 'languages
  :prefix "ahk-"
  :link '(url-link :tag "Github" "https://github.com/ralesi/ahk-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ahk-mode"))

(defcustom ahk-mode-hook '(ahk-mode-hook-activate-filling)
  "Hook functions run by `ahk-mode'."
  :type 'hook
  :group 'ahk-mode)

(defcustom ahk-indentation tab-width
  "The indentation level."
  :type 'integer
  :group 'ahk-mode)

(defcustom ahk-user-path nil
  "Use custom path to autohotkey executable"
  :type 'string
  :group 'ahk-mode)

(defcustom ahk-registry "HKEY_CLASSES_ROOT\\AutoHotkeyScript\\Shell\\Open\\Command"
  "Registry location for autohotkey install"
  :type 'string
  :group 'ahk-mode)

(defvar ahk-path
  (let ((reg-data (shell-command-to-string (format "reg query \"%s\"" ahk-registry))))
    (file-name-directory
                          (replace-regexp-in-string "\\\\" "/" (cadr (split-string reg-data "\\\"")))))
  "Path of installed autohotkey executable")

(defvar ahk-path-exe
  (concat ahk-path "AutoHothkey.exe" )
  "Path of installed autohotkey executable")

(defvar ahk-help-chm
  (concat ahk-path "AutoHotkey.chm")
  "Path of installed autohotkey help file")

(defvar ahk-spy-exe
  (concat ahk-path "AU3_Spy.exe")
  "Path of installed autohotkey help file")

(defun ahk-installed-p ()
  (file-exists-p ahk-path-exe)
  "Predicate function to check existense of autohotkey executable")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ahk$"  . ahk-mode))

;;; keymap
(defvar ahk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "C-c C-r") 'ahk-lookup-chm)
    (define-key map (kbd "C-c C-?") 'ahk-lookup-web)
    (define-key map (kbd "C-c i") 'ahk-indent-message)
    (define-key map (kbd "C-c C-c") 'ahk-comment-dwim)
    (define-key map (kbd "C-c C-b") 'ahk-comment-block-dwim)
    map)
  "Keymap for Autohotkey major mode.")

;;; menu
(easy-menu-define ahk-menu ahk-mode-map
  "AHK Mode Commands"
  '("AHK"
    ["Lookup webdocs on command" ahk-lookup-web]
    ["Execute script" ahk-run-script]
    "---"
    ["Version" ahk-version]))

;;; syntax table
(defvar ahk-mode-syntax-table nil "Syntax table for `ahk-mode'.")

(setq ahk-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; these are also allowed in variable names
        (modify-syntax-entry ?#  "w" synTable)
        (modify-syntax-entry ?_  "w" synTable)
        (modify-syntax-entry ?@  "w" synTable)
        ;; some additional characters used in paths and switches
        (modify-syntax-entry ?\\  "w" synTable)
        (modify-syntax-entry ?\;  "< b" synTable)
        ;; for multiline comments
        (modify-syntax-entry ?\/  ". 14" synTable)
        (modify-syntax-entry ?*  ". 23"   synTable)
        ;; New line
        (modify-syntax-entry ?\n "> b"  synTable)
        ;; ` is escape
        (modify-syntax-entry ?` "\\" synTable)
        ;; allow single quoted strings
        (modify-syntax-entry ?' "\"" synTable)
        ;; the rest is
        ;; (modify-syntax-entry ?. "." synTable)
        ;; (modify-syntax-entry ?: "." synTable)
        ;; (modify-syntax-entry ?- "." synTable)
        ;; (modify-syntax-entry ?! "." synTable)
        ;; (modify-syntax-entry ?$ "." synTable)
        ;; (modify-syntax-entry ?% "." synTable)
        ;; (modify-syntax-entry ?^ "." synTable)
        ;; (modify-syntax-entry ?& "." synTable)
        ;; (modify-syntax-entry ?~ "." synTable)
        ;; (modify-syntax-entry ?| "." synTable)
        ;; (modify-syntax-entry ?? "." synTable)
        ;; (modify-syntax-entry ?< "." synTable)
        ;; (modify-syntax-entry ?> "." synTable)
        ;; (modify-syntax-entry ?, "." synTable)
        synTable))

;;; imenu support

(defconst ahk-imenu-generic-expression
  '(("Functions"   "^\s*\\(.*\\)(.*)[\n]{" 1)
    ("Labels"      "^\s*\\([A-Za-z0-9^:]+\\):\n" 1)
    ("Keybindings" "^\s*\\(.+?\\)::" 1)
    ("Comments"    "^; \\(.+\\)" 1))
  "imenu index for `ahk-mode'")

(defun ahk-run-script ()
  "Run ahk-script"
  (interactive)
  (let*
      ((file (shell-quote-argument (buffer-file-name)))
       (optional-ahk-exe (and (stringp ahk-user-path)
                              (file-exists-p ahk-user-path)))
       (ahk-exe-path (shell-quote-argument (if optional-ahk-exe
                                               ahk-user-path
                                             ahk-path-exe))))
    (if (and (stringp ahk-user-path)
             (not optional-ahk-exe))
        (error "Error: optional-ahk-exe is not found.")
      (save-window-excursion
        (async-shell-command (format "%s %s" ahk-exe-path file))))))

(defun ahk-command-prompt ()
  "Determine command at point, and prompt if nothing found"
  (let ((myword (or  (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))
                       (thing-at-point 'symbol))
                     (read-string "Command: "))))
    myword))

(defun ahk-lookup-web ()
  "Look up current word in AutoHotkey's reference doc.
Launches default browser and opens the doc's url."
  (interactive)
  (let* ((acap (ahk-command-prompt))
         (myurl2 (concat "http://ahkscript.org/docs/commands/" acap ".htm" ))
         (myurl1 (concat "http://www.autohotkey.com/docs/commands/" acap ".htm" )))
    (browse-url myurl2)))

(defun ahk-lookup-chm ()
  "Look up current word in AutoHotkey's reference doc.
Launches autohotkey help in chm file."
  (interactive)
  (let* ((acap (ahk-command-prompt))
         (myurl (concat "http://ahkscript.org/docs/commands/" acap ".htm" )))
    ;; v1
    ;; (setq myurl (concat "http://www.autohotkey.com/docs/commands/" myword ".htm" ))
    ;; v2
    (browse-url myurl)))

(defun ahk-mode-hook-activate-filling ()
  "Activates `auto-fill-mode' and truncates lines."
  (progn
    (setq truncate-lines nil)
    (auto-fill-mode 1)))

(defun ahk-version ()
  "Show the `ahk-mode' version in the echo area."
  (interactive)
  (message (concat "ahk-mode version " ahk-mode-version)))

;;;; indentation
(defun ahk-calc-indentation (str &optional offset)
  (let ((i (* (or offset 0) ahk-indentation)))
    (while (string-match "\t" str)
      (setq i (+ i tab-width)
            str (replace-match "" nil t str)))
    (setq i (+ i (length str)))
    i))

;; the follwing regexp is used to detect if a condition is a one line statement or not,
;; i.e. it matches one line statements but should not match those where the THEN resp.
;; ELSE body is on its own line ...
(defvar ahk-one-line-if-regexp
  (concat "^\\([ \t]*\\)" ;; this is used for indentation
          "\\("
          "If\\(Not\\)?\\("
          (regexp-opt '("InString" "InStr"
                        "Less" "Greater" "Equal"
                        "LessOrEqual" "GreaterOrEqual"
                        ))
          "\\)[^,\n]*,[^,\n]*,[^,\n]*,"
          "\\|"
          "If\\(Not\\)?Exist[^,\n]*,[^,\n]*,"
          "\\|"
          "Else[ \t]+\\([^I\n][^f\n][^ \n]\\)"
          "\\)"))

(defun ahk-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (looking-at "^[ \t]*$") (not (bobp)))
      (forward-line -1))
    (current-indentation)))

(defun ahk-indent-message ()
  (interactive)
  (message (format "%s" (ahk-calc-indentation))))

(defun ahk-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent 0)
        (opening-brace nil)
        (else nil)
        (closing-brace)
        (block-skip nil)
        (case-fold-search t))
    ;; do a backward search to determine the indentation level
    (save-excursion
      (beginning-of-line)

      ;; if beginning with a comment, indent based on previous line
      (if (looking-at "^\\([ \t*]\\);")
          (setq indent (ahk-previous-indent))
        ;; save type of current line
        (setq opening-brace (looking-at "^\\([ \t]*\\)[{(]"))
        (setq else          (looking-at "^\\([ \t]*\\)Else[ \r\n]"))
        (setq closing-brace (looking-at "^\\([ \t]*\\)[)}]"))
        ;; check previous non-empty line
        (skip-chars-backward " \r\t\n")
        (beginning-of-line)
        (when (looking-at "^\\([ \t]*\\)[)}]")
          (goto-char (match-end 0))
          (backward-list)
          (skip-chars-backward " \r\t\n")
          (beginning-of-line)
          (setq block-skip t))
        ;; skip commented lines backward
        (while (and (looking-at "^;") (not (bobp)))
          (forward-line -1))
        ;; is it a label
        (if (looking-at "^[^: \n]+:")
            (if (and (not opening-brace)
                     (not block-skip)
                     (looking-at "^[^: ]+:\\([^:\n]*:\\)?[ \t]*$"))
                (setq indent ahk-indentation)
              (setq indent 0))
          ;; is it an opening { or (
          (if (looking-at "^\\([ \t]*\\)[{(]")
              (setq indent (ahk-calc-indentation (match-string 1) 1))
            ;; is it a Return at the first level?
            (if (and (looking-at "^\\([ \t]*\\)[rR]eturn")
                     (= (ahk-calc-indentation (match-string 1)) ahk-indentation))
                (setq indent (ahk-calc-indentation (match-string 1) -1))
              ;; If/Else with body on next line, but not opening { or (
              (if (and (not opening-brace)
                       (not block-skip)
                       (looking-at "^\\([ \t]*\\)\\(If\\|Else\\)")
                       (not (looking-at ahk-one-line-if-regexp)))
                  (setq indent (ahk-calc-indentation (match-string 1) 1))
                ;; two lines back was a If/Else thus indent like it
                (if (and (not opening-brace)
                         ;; (not else)
                         (save-excursion
                           (beginning-of-line)
                           (skip-chars-backward " \r\t\n")
                           (beginning-of-line)
                           (setq indent nil)
                           ;; backtrace nested Ifs
                           (while (and (looking-at "^\\([ \t]*\\)\\(If\\|Else\\)")
                                       (not (looking-at ahk-one-line-if-regexp)))
                             (setq indent (ahk-calc-indentation (match-string 1)))
                             (beginning-of-line)
                             (skip-chars-backward " \r\t\n")
                             (beginning-of-line))
                           indent))
                    (setq indent indent)
                  ;; the last resort, indent as the last line
                  (if (looking-at "^\\([ \t]*\\)")
                      (setq indent (ahk-calc-indentation (match-string 1)))))))))))
    ;; check for special tokens
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\([ \t]*\\)[})]")
          (setq indent (- indent ahk-indentation))
        (if (or (looking-at "^[ \t]*[^,: \t\n]*:")
                (looking-at "^;;;"))
            (setq indent 0))))

    ;; set negative indentation to 0
    (if (< indent 0)
        (setq indent 0))

    (let ((point (point-marker)))
      (beginning-of-line)
      (if (looking-at "^[ \t]+")
          (replace-match ""))
      (indent-to indent)
      (if  (not (marker-position point))
          (if (re-search-forward "[^ \t]" (point-max) t)
              (goto-char (1- (point))))
        (goto-char point)
        (set-marker point nil)))
    (if (bolp)
        (goto-char (+ (point) indent)))))

(defun ahk-indent-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (ahk-indent-line)
      (forward-line 1))
    (ahk-indent-line)
    (set-marker end nil)))

;;;; commenting

(defun ahk-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For details, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start ";")
        (comment-end ""))
    (comment-dwim arg)))

(defun ahk-comment-block-dwim (arg)
  "Comment or uncomment current line or region using block notation.
For details, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-style 'extra-line)
        (comment-start "/*")
        (comment-end "*/"))
    (comment-dwim arg)))

;;; font-lock

(defvar ahk-commands
  '("Abort" "AboveNormal" "Add" "All" "Alnum" "Alpha" "AltSubmit" "AlwaysOnTop" "And" "Asc" "AutoSize" "AutoTrim" "Background" "BackgroundTrans" "BelowNormal" "Between" "BitAnd" "BitNot" "BitOr" "BitShiftLeft" "BitShiftRight" "BitXOr" "BlockInput" "Border" "Bottom" "Break" "Button" "Buttons" "ByRef" "Cancel" "Capacity" "Caption" "Catch" "Ceil" "Center" "Check" "Check3" "Checkbox" "Checked" "CheckedGray" "Checks" "Choose" "ChooseString" "Chr" "Click" "ClipWait" "Close" "Color" "ComboBox" "Contains" "Continue" "Control" "ControlClick" "ControlFocus" "ControlGet" "ControlGetFocus" "ControlGetPos" "ControlGetText" "ControlList" "ControlMove" "ControlSend" "ControlSendRaw" "ControlSetText" "CoordMode" "Count" "Critical" "DDL" "Date" "DateTime" "Days" "Default" "Delete" "DeleteAll" "Delimiter" "Deref" "Destroy" "DetectHiddenText" "DetectHiddenWindows" "Digit" "Disable" "Disabled" "Displays" "Drive" "DriveGet" "DriveSpaceFree" "DropDownList" "Edit" "Eject" "Else" "Enable" "Enabled" "EnvAdd" "EnvDiv" "EnvGet" "EnvMult" "EnvSet" "EnvSub" "EnvUpdate" "Error" "ExStyle" "Exist" "Exit" "ExitApp" "Exp" "Expand" "FileAppend" "FileCopy" "FileCopyDir" "FileCreateDir" "FileCreateShortcut" "FileDelete" "FileEncoding" "FileGetAttrib" "FileGetShortcut" "FileGetSize" "FileGetTime" "FileGetVersion" "FileInstall" "FileMove" "FileMoveDir" "FileOpen" "FileRead" "FileReadLine" "FileRecycle" "FileRecycleEmpty" "FileRemoveDir" "FileSelectFile" "FileSelectFolder" "FileSetAttrib" "FileSetTime" "FileSystem" "Finally" "First" "Flash" "Float" "FloatFast" "Floor" "Focus" "Font" "For" "Format" "FormatTime" "GetKeyState" "Gosub" "Goto" "Grid" "Group" "GroupActivate" "GroupAdd" "GroupBox" "GroupClose" "GroupDeactivate" "Gui" "GuiClose" "GuiContextMenu" "GuiControl" "GuiControlGet" "GuiDropFiles" "GuiEscape" "GuiSize" "HKCC" "HKCR" "HKCU" "HKEY_CLASSES_ROOT" "HKEY_CURRENT_CONFIG" "HKEY_CURRENT_USER" "HKEY_LOCAL_MACHINE" "HKEY_USERS" "HKLM" "HKU" "HScroll" "Hdr" "Hidden" "Hide" "High" "Hotkey" "Hours" "ID" "IDLast" "Icon" "IconSmall" "If" "IfEqual" "IfExist" "IfGreater" "IfGreaterOrEqual" "IfInString" "IfLess" "IfLessOrEqual" "IfMsgBox" "IfNotEqual" "IfWinActive" "IfWinExist" "IfWinNotActive" "IfWinNotExist" "Ignore" "ImageList" "ImageSearch" "In" "IniDelete" "IniRead" "IniWrite" "Input" "InputBox" "Integer" "IntegerFast" "Interrupt" "Is" "Join" "KeyHistory" "KeyWait" "LTrim" "Label" "LastFound" "LastFoundExist" "Left" "Limit" "Lines" "List" "ListBox" "ListHotkeys" "ListLines" "ListVars" "ListView" "Ln" "Lock" "Log" "Logoff" "Loop" "Low" "Lower" "Lowercase" "MainWindow" "Margin" "MaxSize" "Maximize" "MaximizeBox" "Menu" "MinMax" "MinSize" "Minimize" "MinimizeBox" "Minutes" "Mod" "MonthCal" "Mouse" "MouseClick" "MouseClickDrag" "MouseGetPos" "MouseMove" "Move" "MsgBox" "Multi" "NA" "No" "NoActivate" "NoDefault" "NoHide" "NoIcon" "NoMainWindow" "NoSort" "NoSortHdr" "NoStandard" "NoTab" "NoTimers" "Normal" "Not" "Number" "Off" "Ok" "On" "OnExit" "Or" "OutputDebug" "OwnDialogs" "Owner" "Parse" "Password" "Pause" "Pic" "Picture" "Pixel" "PixelGetColor" "PixelSearch" "Pos" "PostMessage" "Pow" "Priority" "Process" "ProcessName" "Progress" "REG_BINARY" "REG_DWORD" "REG_EXPAND_SZ" "REG_MULTI_SZ" "REG_SZ" "RGB" "RTrim" "Radio" "Random" "Range" "Read" "ReadOnly" "Realtime" "Redraw" "RegDelete" "RegRead" "RegWrite" "Region" "Relative" "Reload" "Rename" "Report" "Resize" "Restore" "Retry" "Return" "Right" "Round" "Run" "RunAs" "RunWait" "Screen" "Seconds" "Section" "See" "Send" "SendInput" "SendLevel" "SendMessage" "SendMode" "SendPlay" "SendRaw" "Serial" "SetBatchLines" "SetCapslockState" "SetControlDelay" "SetDefaultMouseSpeed" "SetEnv" "SetFormat" "SetKeyDelay" "SetLabel" "SetMouseDelay" "SetNumlockState" "SetRegView" "SetScrollLockState" "SetStoreCapslockMode" "SetTimer" "SetTitleMatchMode" "SetWinDelay" "SetWorkingDir" "ShiftAltTab" "Show" "Shutdown" "Sin" "Single" "Sleep" "Slider" "Sort" "SortDesc" "SoundBeep" "SoundGet" "SoundGetWaveVolume" "SoundPlay" "SoundSet" "SoundSetWaveVolume" "SplashImage" "SplashTextOff" "SplashTextOn" "SplitPath" "Sqrt" "Standard" "Status" "StatusBar" "StatusBarGetText" "StatusBarWait" "StatusCD" "StringCaseSense" "StringGetPos" "StringLeft" "StringLen" "StringLower" "StringMid" "StringReplace" "StringRight" "StringSplit" "StringTrimLeft" "StringTrimRight" "StringUpper" "Style" "Submit" "Suspend" "SysGet" "SysMenu" "Tab" "Tab2" "TabStop" "Tan" "Text" "Theme" "Thread" "Throw" "Tile" "Time" "Tip" "ToggleCheck" "ToggleEnable" "ToolTip" "ToolWindow" "Top" "Topmost" "TransColor" "Transform" "Transparent" "Tray" "TrayTip" "TreeView" "Trim" "Try" "TryAgain" "Type" "UnCheck" "Unicode" "Unlock" "Until" "UpDown" "Upper" "Uppercase" "UrlDownloadToFile" "UseErrorLevel" "VScroll" "Var" "Vis" "VisFirst" "Visible" "Wait" "WaitClose" "WantCtrlA" "WantF2" "WantReturn" "While-loop" "WinActivate" "WinActivateBottom" "WinClose" "WinGet" "WinGetActiveStats" "WinGetActiveTitle" "WinGetClass" "WinGetPos" "WinGetText" "WinGetTitle" "WinHide" "WinKill" "WinMaximize" "WinMenuSelectItem" "WinMinimize" "WinMinimizeAll" "WinMinimizeAllUndo" "WinMove" "WinRestore" "WinSet" "WinSetTitle" "WinShow" "WinWait" "WinWaitActive" "WinWaitClose" "WinWaitNotActive" "Wrap" "Xdigit" "Yes" "ahk_class" "ahk_group" "ahk_id" "ahk_pid" "bold" "global" "italic" "local" "norm" "static" "strike" "underline" "xm" "xp" "xs" "ym" "yp" "ys")
  "AHK keywords.")

(defvar ahk-directives
  '("#ClipboardTimeout" "#CommentFlag" "#ErrorStdOut" "#EscapeChar" "#HotkeyInterval" "#HotkeyModifierTimeout" "#Hotstring" "#If" "#IfTimeout" "#IfWinActive" "#IfWinExist" "#Include" "#InputLevel" "#InstallKeybdHook" "#InstallMouseHook" "#KeyHistory" "#LTrim" "#MaxHotkeysPerInterval" "#MaxMem" "#MaxThreads" "#MaxThreadsBuffer" "#MaxThreadsPerHotkey" "#MenuMaskKey" "#NoEnv" "#NoTrayIcon" "#Persistent" "#SingleInstance" "#UseHook" "#Warn" "#WinActivateForce")
  "AHK directives")

(defvar ahk-functions
  '("ACos" "ASin" "ATan" "Abs" "Asc" "Ceil" "Chr" "ComObjActive" "ComObjArray" "ComObjConnect" "ComObjCreate" "ComObjEnwrap" "ComObjError" "ComObjFlags" "ComObjGet" "ComObjMissing" "ComObjParameter" "ComObjQuery" "ComObjType" "ComObjUnwrap" "ComObjValue" "Cos" "DllCall" "Exp" "FileExist" "Floor" "Func" "Functions" "GetKeyName" "GetKeySC" "GetKeyState" "GetKeyVK" "IL_Add" "IL_Create" "IL_Destroy" "InStr" "IsByRef" "IsFunc" "IsLabel" "IsObject" "LV_Add" "LV_Delete" "LV_DeleteCol" "LV_GetCount" "LV_GetNext" "LV_GetText" "LV_Insert" "LV_InsertCol" "LV_Modify" "LV_ModifyCol" "LV_SetImageList" "Ln" "Log" "Mod" "NumGet" "NumPut" "OnMessage" "RegExMatch" "RegExReplace" "RegisterCallback" "Round" "SB_SetIcon" "SB_SetParts" "SB_SetText" "Sin" "Sqrt" "StrGet" "StrLen" "StrPut" "SubStr" "TV_Add" "TV_Delete" "TV_Get" "TV_GetChild" "TV_GetCount" "TV_GetNext" "TV_GetParent" "TV_GetPrev" "TV_GetSelection" "TV_GetText" "TV_Modify" "Tan" "VarSetCapacity" "WinActive" "WinExist")
  "AHK functions.")

(defvar ahk-variables
  '("A_AhkPath" "A_AhkVersion" "A_AppData" "A_AppDataCommon" "A_AutoTrim" "A_BatchLines" "A_CaretX" "A_CaretY" "A_ComputerName" "A_ControlDelay" "A_Cursor" "A_DD" "A_DDD" "A_DDDD" "A_DefaultMouseSpeed" "A_Desktop" "A_DesktopCommon" "A_DetectHiddenText" "A_DetectHiddenWindows" "A_EndChar" "A_EventInfo" "A_ExitReason" "A_FileEncoding" "A_FormatFloat" "A_FormatInteger" "A_Gui" "A_GuiControl" "A_GuiControlEvent" "A_GuiEvent" "A_GuiHeight" "A_GuiWidth" "A_GuiX" "A_GuiY" "A_Hour" "A_IPAddress1" "A_IPAddress2" "A_IPAddress3" "A_IPAddress4" "A_ISAdmin" "A_IconFile" "A_IconHidden" "A_IconNumber" "A_IconTip" "A_Index" "A_Is64bitOS" "A_IsAdmin" "A_IsCompiled" "A_IsCritical" "A_IsPaused" "A_IsSuspended" "A_IsUnicode" "A_KeyDelay" "A_Language" "A_LastError" "A_LineFile" "A_LineNumber" "A_LoopField" "A_LoopFileAttrib" "A_LoopFileDir" "A_LoopFileExt" "A_LoopFileFullPath" "A_LoopFileLongPath" "A_LoopFileName" "A_LoopFileName," "A_LoopFileShortName" "A_LoopFileShortPath" "A_LoopFileSize" "A_LoopFileSizeKB" "A_LoopFileSizeMB" "A_LoopFileTimeAccessed" "A_LoopFileTimeCreated" "A_LoopFileTimeModified" "A_LoopReadLine" "A_LoopRegKey" "A_LoopRegName" "A_LoopRegName," "A_LoopRegSubkey" "A_LoopRegTimeModified" "A_LoopRegType" "A_MDAY" "A_MM" "A_MMM" "A_MMMM" "A_MSec" "A_Min" "A_Mon" "A_MouseDelay" "A_MyDocuments" "A_Now" "A_NowUTC" "A_NumBatchLines" "A_OSType" "A_OSVersion" "A_PriorHotkey" "A_PriorKey" "A_ProgramFiles" "A_Programs" "A_ProgramsCommon" "A_PtrSize" "A_RegView" "A_ScreenDPI" "A_ScreenHeight" "A_ScreenWidth" "A_ScriptDir" "A_ScriptFullPath" "A_ScriptHwnd" "A_ScriptName" "A_Sec" "A_Space" "A_StartMenu" "A_StartMenuCommon" "A_Startup" "A_StartupCommon" "A_StringCaseSense" "A_Tab" "A_Temp" "A_ThisFunc" "A_ThisHotkey" "A_ThisLabel" "A_ThisMenu" "A_ThisMenuItem" "A_ThisMenuItemPos" "A_TickCount" "A_TimeIdle" "A_TimeIdlePhysical" "A_TimeSincePriorHotkey" "A_TimeSinceThisHotkey" "A_TitleMatchMode" "A_TitleMatchModeSpeed" "A_UserName" "A_WDay" "A_WinDelay" "A_WinDir" "A_WorkingDir" "A_YDay" "A_YEAR" "A_YWeek" "A_YYYY" "Clipboard" "ClipboardAll" "ComSpec" "ErrorLevel" "False" "ProgramFiles" "True" "Variable")
  "AHK variables.")

(defvar ahk-keys
  '("Alt" "AltDown" "AltTab" "AltTabAndMenu" "AltTabMenu" "AltTabMenuDismiss" "AltUp" "AppsKey" "BS" "BackSpace" "Browser_Back" "Browser_Favorites" "Browser_Forward" "Browser_Home" "Browser_Refresh" "Browser_Search" "Browser_Stop" "CapsLock" "Control" "Ctrl" "CtrlBreak" "CtrlDown" "CtrlUp" "Del" "Delete" "Down" "End" "Enter" "Esc" "Escape" "F1" "F10" "F11" "F12" "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F2" "F20" "F21" "F22" "F23" "F24" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "Home" "Ins" "Insert" "Joy1" "Joy10" "Joy11" "Joy12" "Joy13" "Joy14" "Joy15" "Joy16" "Joy17" "Joy18" "Joy19" "Joy2" "Joy20" "Joy21" "Joy22" "Joy23" "Joy24" "Joy25" "Joy26" "Joy27" "Joy28" "Joy29" "Joy3" "Joy30" "Joy31" "Joy32" "Joy4" "Joy5" "Joy6" "Joy7" "Joy8" "Joy9" "JoyAxes" "JoyButtons" "JoyInfo" "JoyName" "JoyPOV" "JoyR" "JoyU" "JoyV" "JoyX" "JoyY" "JoyZ" "LAlt" "LButton" "LControl" "LCtrl" "LShift" "LWin" "LWinDown" "LWinUp" "Launch_App1" "Launch_App2" "Launch_Mail" "Launch_Media" "Left" "MButton" "Media_Next" "Media_Play_Pause" "Media_Prev" "Media_Stop" "NumLock" "Numpad0" "Numpad1" "Numpad2" "Numpad3" "Numpad4" "Numpad5" "Numpad6" "Numpad7" "Numpad8" "Numpad9" "NumpadAdd" "NumpadClear" "NumpadDel" "NumpadDiv" "NumpadDot" "NumpadDown" "NumpadEnd" "NumpadEnter" "NumpadHome" "NumpadIns" "NumpadLeft" "NumpadMult" "NumpadPgdn" "NumpadPgup" "NumpadRight" "NumpadSub" "NumpadUp" "PGDN" "PGUP" "Pause" "PrintScreen" "RAlt" "RButton" "RControl" "RCtrl" "RShift" "RWin" "RWinDown" "RWinUp" "Right" "ScrollLock" "Shift" "ShiftDown" "ShiftUp" "Space" "Tab" "Up" "Volume_Down" "Volume_Mute" "Volume_Up" "WheelDown" "WheelLeft" "WheelRight" "WheelUp" "XButton1" "XButton2")
  "AHK keywords for keys.")

(defvar ahk-operators
  '("!" "!=" "&" "&&	" "&=" "*	" "**" "*=" "+" "++" "+=" "-" "--" "-=" "." ".	" ".=" "/" "//	" "//=" "/=" ":=" "<" "<<" "<<=	" "<=" "<>" "=" "==" ">" ">=" ">>" ">>=" "?:" "AND" "NOT" "OR" "^" "^=" "|" "|=" "||" "~" "~=" ",")
  "AHK operators.")

(defvar ahk-commands-regexp (regexp-opt ahk-commands 'words))
(defvar ahk-functions-regexp (regexp-opt ahk-functions 'words))
(defvar ahk-directives-regexp (regexp-opt ahk-directives 'words))
(defvar ahk-variables-regexp (regexp-opt ahk-variables 'words))
(defvar ahk-keys-regexp (regexp-opt ahk-keys 'words))
(defvar ahk-operators-regexp (regexp-opt ahk-operators))

(defvar ahk-font-lock-keywords nil )
(setq ahk-font-lock-keywords
      `(
        ;; keybindings
        ("^\\([^\t\n:=]+\\)::"            . (1 font-lock-constant-face))
        ;; labels
        ("^\\([^\t\n: ^=]+\\):"            . (1 font-lock-builtin-face))
        ;; variables
        ("%[^% ]+%"                        . font-lock-variable-name-face)
        (,ahk-commands-regexp              . font-lock-regexp-grouping-backslash)
        (,ahk-functions-regexp             . font-lock-function-name-face)
        (,ahk-directives-regexp            . font-lock-keyword-face)
        (,ahk-variables-regexp             . font-lock-variable-name-face)
        (,ahk-keys-regexp                  . font-lock-constant-face)
        (,ahk-operators-regexp             . font-lock-type-face)
        ;; note: order matters
        ))

;; keyword completion
(defvar ahk-kwdList nil "AHK keywords.")

(defvar ahk-all-keywords nil "list of all ahk keywords")
(setq ahk-all-keywords (append ahk-commands ahk-functions ahk-variables))


(setq ahk-kwdList (make-hash-table :test 'equal))
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-commands)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-functions)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-directives)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-variables)
(mapc (lambda (x) (puthash x t ahk-kwdList)) ahk-keys)
(put 'ahk-kwdList 'risky-local-variable t)


(defun ahk-completion-at-point ()
  "Complete the current work using the list of all syntax's."
  (interactive)
  (let ((pt (point)))
    (if (and (or (save-excursion (re-search-backward "\\<\\w+"))
                 (looking-at "\\<\\w+"))
             (= (match-end 0) pt))
        (let ((start (match-beginning 0))
              (prefix (match-string 0))
              (completion-ignore-case t)
              completions)
          (list start pt (all-completions prefix ahk-all-keywords) :exclusive 'no)))))

(defvar ac-source-ahk nil
      "Completion for AHK mode")

(defvar ac-source-keys-ahk nil
      "Completion for AHK keys mode")

(defvar ac-source-directives-ahk nil
      "Completion for AHK directives mode")

(setq ac-source-ahk
      '((candidates . (all-completions ac-prefix ahk-all-keywords))
        (limit . nil)
        (symbol . "f")))

(setq ac-source-directives-ahk
      '((candidates . (all-completions ac-prefix ahk-directives))
        (limit . nil)
        (symbol . "d")))

(setq ac-source-keys-ahk
      '((candidates . (all-completions ac-prefix ahk-keys))
        (limit . nil)
        (symbol . "k")))

;; clear memory
;; (setq ahk-commands nil)
;; (setq ahk-functions nil)
;; (setq ahk-directives nil)
;; (setq ahk-variables nil)
;; (setq ahk-keys nil)

(define-derived-mode ahk-mode prog-mode "Autohotkey Mode"
  "Major mode for editing AutoHotkey script (AHK).

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key Bindings
\\{ahk-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (set-syntax-table ahk-mode-syntax-table)

  (setq major-mode 'ahk-mode
        mode-name "AHK"
        local-abbrev-table ahk-mode-abbrev-table)

  ;; ui
  (use-local-map ahk-mode-map)
  (easy-menu-add ahk-menu)

  ;; imenu
  (setq-local imenu-generic-expression ahk-imenu-generic-expression)
  (setq-local imenu-sort-function 'imenu--sort-by-position)

  ;; font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((ahk-font-lock-keywords) nil t))

  ;; clear memory
  ;; (setq ahk-commands-regexp nil)
  ;; (setq ahk-functions-regexp nil)
  ;; (setq ahk-variables-regexp nil)
  ;; (setq ahk-keys-regexp nil)

  (setq-local comment-start ";")
  (setq-local comment-end   "")
  (setq-local comment-start-skip ";+ *")

  (setq-local block-comment-start     "/*")
  (setq-local block-comment-end       "*/")
  (setq-local block-comment-left      " * ")
  (setq-local block-comment-right     " *")
  (setq-local block-comment-top-right "")
  (setq-local block-comment-bot-left  " ")
  (setq-local block-comment-char      ?*)

  (setq-local indent-line-function   'ahk-indent-line)
  (setq-local indent-region-function 'ahk-indent-region)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)

  ;; completion
  (add-hook 'completion-at-point-functions 'ahk-completion-at-point nil t)

  ;; add to auto-complete sources if ac is loaded
  (if (listp 'ac-modes)
      (add-to-list 'ac-modes 'ahk-mode)
    (add-to-list 'ac-sources  'ac-source-ahk)
    (add-to-list 'ac-sources  'ac-source-directives-ahk)
    (add-to-list 'ac-sources  'ac-source-keys-ahk))

  (run-mode-hooks 'ahk-mode-hook))

(provide 'ahk-mode)

;;; ahk-mode.el ends here
