#! /usr/bin/env python
# coding: utf-8

# pylint: disable=all

"""为方向键, 控制键, 功能键等生成ASCII字符序列.

本程序依靠人为设定的规则生成ASCII字符序列. 对于常用按键, 如果PUTTY产生了
字符序列, 则不另外生成, 只在emacs端将字符序列映射到对应的按键.

PUTTY产生的字符序列可以通过cat命令查看.
emacs端接收到的字符序列可以通过命令`C-h l`查看.
autohotkey中按键的名字可参考: https://www.autohotkey.com/docs/KeyList.htm
"""

import collections


ModifierKey = collections.namedtuple("ModifierKey", [
    "emacs",   # 键在emacs中的名字
    "hotkey",  # 键在autohotkey中的名字
    "value",   # 键的值, 此键与其他键组合时这个值为字符序列的前缀
])

OrdinaryKey = collections.namedtuple("OrdinaryKey", [
    "type",       # 键的类型. "f"表示功能键, "c"表示普通键
    "emacs",      # 键在emacs中的名字
    "hotkey",     # 键在autohotkey中的名字
    "value",      # (此键位于修饰键后面时)键的值
    "modifiers",  # 指定此键可以与那些修饰键结合, 为tuple类型
])

# 修饰键信息.
modifier_keys = {
    "C":    ModifierKey("C-",      "^",    r"\e[a"),   # Ctrl
    "S":    ModifierKey("S-",      "+",    r"\e[b"),   # Shift
    "A":    ModifierKey("M-",      "!",    r"\e[c"),   # Alt
    "CS":   ModifierKey("C-S-",    "^+",   r"\e[d"),   # Ctrl+Shift
    "CA":   ModifierKey("C-M-",    "^!",   r"\e[f"),   # Ctrl+Alt
    "SA":   ModifierKey("S-M-",    "+!",   r"\e[g"),   # Shift+Alt
    "CSA":  ModifierKey("C-S-M-",  "^+!",  r"\e[h"),   # Ctrl+Shift+Alt
}

# 对于任何一个实体按键(非修饰键), 考虑如下两种条件:
#   1. PUTTY产生了字符序列;
#   2. emacs捕获了字符序列;
# 如果仅条件1成立, 则只需在emacs端将PUTTY产生的字符序列映射成对应的键;
# 如果仅条件2成立, 则只需用autohotkey在本地机器生成emacs所捕获的字符序列;
# 如果条件1和条件2都不成立, 则既要在本地机器上生成字符序列, 又要在emacs端将生
# 成的字符序列映射成对应的键.
modifiers_all = ("C", "S", "A", "CS", "CA", "SA", "CSA")
# 对于方向键, Ctrl和Alt键修饰时PUTTY会产生对应的字符序列, 这里将两者排除
modifiers_arrow = ("S", "CS", "CA", "SA", "CSA")
# 对于home, end等键, Alt键修饰时PUTTY会产生对应的字符序列, 这里予以排除
modifiers_home = ("C", "S", "CS", "CA", "SA", "CSA")

# 方向键和功能键信息
function_keys = [
    OrdinaryKey("f",  "up",         "Up",         "A",    modifiers_arrow),
    OrdinaryKey("f",  "down",       "Down",       "B",    modifiers_arrow),
    OrdinaryKey("f",  "right",      "Right",      "C",    modifiers_arrow),
    OrdinaryKey("f",  "left",       "Left",       "D",    modifiers_arrow),
    OrdinaryKey("f",  "insert",     "Insert",     "2~",   modifiers_home),
    OrdinaryKey("f",  "delete",     "Delete",     "3~",   modifiers_home),
    OrdinaryKey("f",  "home",       "Home",       "1~",   modifiers_home),
    OrdinaryKey("f",  "end",        "End",        "4~",   modifiers_home),
    OrdinaryKey("f",  "prior",      "PgUp",       "5~",   modifiers_home),
    OrdinaryKey("f",  "next",       "PgDn",       "6~",   modifiers_home),
    OrdinaryKey("f",  "return",     "Enter",      "7~",   modifiers_home),
    OrdinaryKey("f",  "backspace",  "Backspace",  "F",    modifiers_all),
    OrdinaryKey("f",  "tab",        "tab",        "H",    modifiers_all),
    OrdinaryKey("f",  "f1",         "F1",         "11~",  modifiers_all),
    OrdinaryKey("f",  "f2",         "F2",         "12~",  modifiers_all),
    OrdinaryKey("f",  "f3",         "F3",         "13~",  modifiers_all),
    OrdinaryKey("f",  "f4",         "F4",         "14~",  modifiers_all),
    OrdinaryKey("f",  "f5",         "F5",         "15~",  modifiers_all),
    OrdinaryKey("f",  "f6",         "F6",         "17~",  modifiers_all),
    OrdinaryKey("f",  "f7",         "F7",         "18~",  modifiers_all),
    OrdinaryKey("f",  "f8",         "F8",         "19~",  modifiers_all),
    OrdinaryKey("f",  "f9",         "F9",         "20~",  modifiers_all),
    OrdinaryKey("f",  "f10",        "F10",        "21~",  modifiers_all),
    OrdinaryKey("f",  "f11",        "F11",        "23~",  modifiers_all),
    OrdinaryKey("f",  "f12",        "F12",        "24~",  modifiers_all),
]

# 非字符普通按键, 因每一个键对应两个字符, 不受shift键修饰.
#   功能完好的键:         "]", "\", "@", "^"
#   putty不能handle的键:  ":"
#   emacs不能handle的键:  "["
ordinary_keys = [
    # lower part of key, do not need shift key
    OrdinaryKey("c",  ",",  ",",  ";{:03d}".format(ord(",")),  ("C", "CA")),
    OrdinaryKey("c",  ".",  ".",  ";{:03d}".format(ord(".")),  ("C", "CA")),
    OrdinaryKey("c",  "/",  "/",  ";{:03d}".format(ord("/")),  ("C", "CA")),
    OrdinaryKey("c",  ";",  ";",  ";{:03d}".format(ord(";")),  ("C", "CA")),
    OrdinaryKey("c",  "'",  "'",  ";{:03d}".format(ord("'")),  ("C", "CA")),
    OrdinaryKey("c",  "-",  "-",  ";{:03d}".format(ord("-")),  ("C", "CA")),
    OrdinaryKey("c",  "=",  "=",  ";{:03d}".format(ord("=")),  ("C", "CA")),
    OrdinaryKey("c",  "`",  "`",  ";{:03d}".format(ord("`")),  ("C", "CA")),
    OrdinaryKey("c",  "0",  "0",  ";{:03d}".format(ord("0")),  ("C", "CA")),
    OrdinaryKey("c",  "1",  "1",  ";{:03d}".format(ord("1")),  ("C", "CA")),
    OrdinaryKey("c",  "2",  "2",  ";{:03d}".format(ord("2")),  ("C", "CA")),
    OrdinaryKey("c",  "3",  "3",  ";{:03d}".format(ord("3")),  ("C", "CA")),
    OrdinaryKey("c",  "4",  "4",  ";{:03d}".format(ord("4")),  ("C", "CA")),
    OrdinaryKey("c",  "5",  "5",  ";{:03d}".format(ord("5")),  ("C", "CA")),
    OrdinaryKey("c",  "6",  "6",  ";{:03d}".format(ord("6")),  ("C", "CA")),
    OrdinaryKey("c",  "7",  "7",  ";{:03d}".format(ord("7")),  ("C", "CA")),
    OrdinaryKey("c",  "8",  "8",  ";{:03d}".format(ord("8")),  ("C", "CA")),
    OrdinaryKey("c",  "9",  "9",  ";{:03d}".format(ord("9")),  ("C", "CA")),
    # high part of key, need shift key
    OrdinaryKey("c",  "<",  "<",  ";{:03d}".format(ord("<")),  ("C", "CA")),
    OrdinaryKey("c",  ">",  ">",  ";{:03d}".format(ord(">")),  ("C", "CA")),
    OrdinaryKey("c",  "?",  "?",  ";{:03d}".format(ord("?")),  ("C", "CA")),
    OrdinaryKey("c",  '"',  '"',  ";{:03d}".format(ord('"')),  ("C", "CA")),
    OrdinaryKey("c",  "{",  "{",  ";{:03d}".format(ord("{")),  ("C", "CA")),
    OrdinaryKey("c",  "}",  "}",  ";{:03d}".format(ord("}")),  ("C", "CA")),
    OrdinaryKey("c",  "|",  "|",  ";{:03d}".format(ord("|")),  ("C", "CA")),
    OrdinaryKey("c",  "_",  "_",  ";{:03d}".format(ord("_")),  ("C", "CA")),
    OrdinaryKey("c",  "+",  "+",  ";{:03d}".format(ord("+")),  ("C", "CA")),
    OrdinaryKey("c",  "~",  "~",  ";{:03d}".format(ord("~")),  ("C", "CA")),
    OrdinaryKey("c",  "!",  "!",  ";{:03d}".format(ord("!")),  ("C", "CA")),
    OrdinaryKey("c",  "#",  "#",  ";{:03d}".format(ord("#")),  ("C", "CA")),
    OrdinaryKey("c",  "$",  "$",  ";{:03d}".format(ord("$")),  ("C", "CA")),
    OrdinaryKey("c",  "%",  "%",  ";{:03d}".format(ord("%")),  ("C", "CA")),
    OrdinaryKey("c",  "&",  "&",  ";{:03d}".format(ord("&")),  ("C", "CA")),
    OrdinaryKey("c",  "*",  "*",  ";{:03d}".format(ord("*")),  ("C", "CA")),
    OrdinaryKey("c",  "(",  "(",  ";{:03d}".format(ord("(")),  ("C", "CA")),
    OrdinaryKey("c",  ")",  ")",  ";{:03d}".format(ord(")")),  ("C", "CA")),
]

########################## generate autohotkey script ##########################

# 可以在这里中添加一些autohotkey的设置命令
autohotkey_header="""\
#SingleInstance force
#Hotstring EndChars `t`n
SetKeyDelay, 0

#ifWinActive, ahk_class PuTTY
{
"""

# 可以在这里添加一些autohotkey的其他键
autohotkey_tailer="""
Numpad0::0
Numpad1::1
Numpad2::2
Numpad3::3
Numpad4::4
Numpad5::5
Numpad6::6
Numpad7::7
Numpad8::8
Numpad9::9
NumpadDot::.
NumpadDiv::/
NumpadMult::*
NumpadAdd::+
NumpadSub::-
NumpadEnter::Enter
}  ;; PuTTY


::jbos::jobs
^p::SendInput  Ok12306@Self{^}
"""

# autohotkey语法: `Delete::SendInput  [ESC][3~`
with open("hotkey.ahk", "w") as dstfile:
    dstfile.write(autohotkey_header)
    for okey in function_keys + ordinary_keys:
        for mkey in okey.modifiers:
            mkey = modifier_keys[mkey]
            # autohotkey中, `ESC`键用`[ESC]`来表示
            mkey_value = mkey.value.replace("\e", "{ESC}")
            dstfile.write("{:<30s} {}\n".format(
                mkey.hotkey + okey.hotkey + "::SendInput",
                mkey_value + okey.value))
    dstfile.write(autohotkey_tailer)

############################ generate emacs script #############################

emacs_header="""\
;;; -*- lexical-binding: t; -*-
;;; personal-putty-keys.el --- Key mappings for putty.

;;; Commentary:

;; This file maps the character sequences to the correspondent keys for
;; putty run on Windows system. Note that some keys are defined by an
;; AutoHotKey script because of a known bug of putty on Windows System:
;;             http://emacswiki.org/emacs/PuTTY

;; This file is automatically generated by key-generator.py. If you want
;; to do any change in this file, please modify key-generator.py instead.

;;; Code:

(defun setup-input-decode-map ()
"""

# 对于PUTTY下有字符序列产生的键, 这里将其字符序列映射回对应的键. 直接定义
# input-decode-map在emacsclient下不管用, 需要将其包装成一个函数, 请参考:
#    https://www.reddit.com/r/emacs/comments/4wnfnb/inputdecodemap_doesnt_work_for_terminal/
emacs_tailer="""
  (define-key input-decode-map "\eOA"     [up])
  (define-key input-decode-map "\eOB"     [down])
  (define-key input-decode-map "\eOC"     [right])
  (define-key input-decode-map "\eOD"     [left])
  (define-key input-decode-map "\e[A"     [C-up])
  (define-key input-decode-map "\e[B"     [C-down])
  (define-key input-decode-map "\e[C"     [C-right])
  (define-key input-decode-map "\e[D"     [C-left])
  (define-key input-decode-map "\e\eOA"   [M-up])
  (define-key input-decode-map "\e\eOB"   [M-down])
  (define-key input-decode-map "\e\eOC"   [M-right])
  (define-key input-decode-map "\e\eOD"   [M-left])

  (define-key input-decode-map "\e[2~"    [insert])
  (define-key input-decode-map "\e[3~"    [deletechar])
  (define-key input-decode-map "\e[1~"    [home])
  (define-key input-decode-map "\e[4~"    [end])
  (define-key input-decode-map "\e[5~"    [prior])
  (define-key input-decode-map "\e[6~"    [next])
  (define-key input-decode-map "\e\e[2~"  [M-insert])
  (define-key input-decode-map "\e\e[3~"  [M-delete])
  (define-key input-decode-map "\e\e[1~"  [M-home])
  (define-key input-decode-map "\e\e[4~"  [M-end])
  (define-key input-decode-map "\e\e[5~"  [M-prior])
  (define-key input-decode-map "\e\e[6~"  [M-next])

  (define-key input-decode-map "\e[11~"   [f1])
  (define-key input-decode-map "\e[12~"   [f2])
  (define-key input-decode-map "\e[13~"   [f3])
  (define-key input-decode-map "\e[14~"   [f4])
  (define-key input-decode-map "\e[15~"   [f5])
  (define-key input-decode-map "\e[17~"   [f6])
  (define-key input-decode-map "\e[18~"   [f7])
  (define-key input-decode-map "\e[19~"   [f8])
  (define-key input-decode-map "\e[20~"   [f9])
  (define-key input-decode-map "\e[21~"   [f10])
  (define-key input-decode-map "\e[23~"   [f11])
  (define-key input-decode-map "\e[24~"   [f12]))

(add-hook 'tty-setup-hook #'setup-input-decode-map)

(provide 'personal-putty-keys)

;;; personal-putty-keys.el ends here
"""

tostring = lambda x: '"{}"'.format(x)
pattern_f = "  (define-key input-decode-map {:<12s} [{}])"
pattern_c = "  (define-key input-decode-map {:<12s} (kbd {}))"
with open("../modules/personal-putty-keys.el", "w") as dstfile:
    dstfile.write(emacs_header)
    for okey in function_keys + ordinary_keys:
        for mkey in okey.modifiers:
            mkey = modifier_keys[mkey]
            name = mkey.emacs + okey.emacs.replace('"', r'\"')
            value = mkey.value + okey.value
            if okey.type == "f":
                line = pattern_f.format(tostring(value), name)
            elif okey.type == "c":
                line = pattern_c.format(tostring(value), tostring(name))
            else:
                assert False, f"wrong key type: {okey.type}"
            dstfile.write(line + "\n")
    dstfile.write(emacs_tailer)
