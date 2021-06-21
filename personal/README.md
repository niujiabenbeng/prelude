# Personal Configurations on Prelude Emacs


## PUTTY和控制键

用PUTTY连接远程服务器的时候, 一些控制键及其组合键无法传递到远程服务器上. 这本质
上是Terminal的问题: 在传递按键事件的时候, 如果按键值在ASCII码内, 就直接传递此字
符; 如果按键值超出了ASCII码, 就传递一个由ASCII码组成的字符序列来表示此键值. 然而,
对于同一个按键, 不同的Terminal生成的字符序列有可能不一样, 某些Terminal甚至对一些
按键毫无反应, 这就导致了一些控制键无法在远程服务器上正确表达.

PUTTY本身内置了一个xterm模拟器. 用PUTTY连接远程服务器的时候, PUTTY实际上是在远程
服务器上开了一个Terminal. 所以会出现上述控制键的问题. 就算是本地机器开的
Terminal, 也会有这种问题.

这里采用的解决方法是: 对于那些无法处理的控制键, 先用AutoHotKey在本地机器上将其映
射成一个字符序列, 在远程机器上再将字符序列映射回对应的键.

对于一个给定的键, 考虑如下两种条件:

1. PUTTY产生了字符序列;
2. emacs捕获了字符序列;

如果仅条件1成立, 则只需在emacs端将PUTTY产生的字符序列映射成对应的键;<br>
如果仅条件2成立, 则只需用autohotkey在本地机器生成emacs所捕获的字符序列;<br>
如果条件1和条件2都不成立, 则既要在本地机器上生成字符序列, 又要在emacs端将生成的
字符序列映射成对应的键.

实际上, emacs本身已经考虑到这个问题, 在`input-decode-map`中将`xterm`可能产生的字
符序列映射成了对应的键. 具体信息可以用`emacs -Q`启动emacs, 然后用`C-h b`查看所有
的绑定键, 定位到`input-decode-map`即可查看.

为了简便起见, 我们通过一些简单的规则来生成按键的字符序列, 比如, 修饰键的字符序列
对其他键而言都一样. 而xterm中键和值的对应规则却并不十分明显. 所以, 我们仅仅check
生成的键值与`input-decode-map`中的不冲突即可.

#### emacs按键的限制

由于历史原因, emacs实现对按键的限制主要有以下几点:

1. 对于`Ctrl`键+子母键的组合不区分大小写, 比如: `C-A`与`C-a`等价.
2. 对于`Alt`键+字母键的组合emacs默认不区分大小写, 但是用户可以做不同的绑定. 比
   如: 默认状态下`M-A`与`M-a`等价, 但是用户将这两个键绑定到不同的命令之后, 它们
   就运行不同的命令.
3. 由于限制1, 对于子母键, `Ctrl+Shift+Alt`和`Ctrl+Alt`两个修饰键等价.
4. `Alt`, `Esc`, `Ctrl-[`三键等价. 所以, `Alt-a`, `Esc a`, `Ctrl-[ a`都会被映射
   为`M-a`. 注意`Esc a`键表示先按`Esc`然后按`a`, 而不是同时按.
5. 在`input-decode-map`中, `M-O`和`M-[`被当做前缀键(prefix command), 结合限制4,
   `C-[`和`M-[`都会被当做前缀, 同时`C-M-[`也会被当做前缀.
6. 在emacs中, `C-i`被映射为`Tab`, `C-m`被映射为`Enter`, `C-M-m`被映射为
   `M-Enter`, `C-M-g`被映射为`C-g`.

#### autohotkey成键策略

1. 修饰键三个: `Ctrl`, `Alt`, `Shift`. 不区分左修饰键和右修饰键. 不考虑`Win`键和
   `Fn`键. `Esc`键保留默认设置.
2. 字母按键采用PUTTY本身生成的字符序列, 原则上修饰键只用在小写字母, 大写字母上不
   用修饰键. 个人配置中也应当遵循这个规则.
3. 非字母的普通按键每一个实体按键对应两个字母, 用`Shift`切换. 所以这些按键只用
   `Ctrl`和`Alt`两个键修饰.
4. 功能按键都用所有三个修饰键修饰, 不考虑如下功能按键: `Caps lock`, `Print
   screen`, `Scroll lock`, `pause/break`.
5. 数字小键盘只保留基本的输入功能, 不用修饰键修饰.


## flycheck in emacs-lisp

prelude项目中, 作者在`init.el`中定义了一些变量, 比如`prelude-personal-dir`. 用户
在个人目录中添加自己的配置时可以使用这些变量. 由于变量的定义在load个人配置之前,
这样做在运行阶段是没什么问题的. 但是, flycheck是以文件为单位进行处理的, 在
flycheck检查个人配置代码时, 它并不知道这些变量已经在`init.el`中定义过了, 所以会
报错: `reference to free variable ‘prelude-personal-dir’`.

类似的情况会发生`load-path`中: 在`init.el`里面加载的路径在其他文件中不能被
flycheck识别, 所以会在`require`语句报错:
`Cannot open load file: No such file or directory`

第一种情况可以通过在个人配置文件中加入前置申明来解决:
`(defvar prelude-personal-dir)`
在运行过程中, 由于变量已经有值, 上面的语句什么也不做. 对于flycheck, 上面语句就
是变量的定义, 也不会报错.

第二种情况可以通过设置`flycheck-emacs-lisp-load-path`来解决. 在设定了所有
load-path之后, 添加如下语句:
`(setq-default flycheck-emacs-lisp-load-path load-path)`

prelude本身实现的模块并不满足flycheck规范, 会导致在个人配置中, 如果`require`了相
关的模块, 也会在该`require`语句上报错. 解决方法是: 在这些文件中, 用`Local
Variables`的方式关闭掉`flycheck-mode`. 具体可参考`personal/settings.el`的配置.


## backspace和delete

在PUTTY环境下, backspace键产生的键值为DEL, delete键产生的键值为"\e[3~".
在emacs中, DEL, delete, deletechar是3个不同的键. 通常, backspace键值会映射成DEL,
delete键值会映射成deletechar, delete和DEL两者的功能似乎是一样的.


## global keymap

| Keybinding      | description                    |
| --------------- | ------------------------------ |
| `C-x \`         | `align-regexp`                 |
| `C-^`           | `crux-top-join-line`           |
| `M-z`           | `zop-up-to-char`               |
| `C-=`           | `expand-region`                |
| `C--`           | `shrink-region`                |
| `C-RET`         | `crux-smart-open-line`         |
| `C-;`           | `comment-line`                 |
| `<f5>`          | `personal-run-current-script`  |
| `C-<f5>`        | `kill-compilation`             |
| `C-<tab>`       | `personal-pattern-replace`     |
| `[f4]`          | `personal-neotree-project-dir` |
| `C-M-\`         | `indent-region`                |


## prelude keymap

| Keybinding | description                                                   |
|------------|---------------------------------------------------------------|
| `C-c c d`  | Duplicate current line/region.                                |
| `C-c c D`  | Duplicate current line/region and comment previous one.       |
| `C-c c e`  | Eval a bit of Emacs Lisp code and replace it with its result. |
| `C-c c i`  | Search for a symbol, only for buffers that contain code.      |
| `C-c c k`  | Kill all open buffers except the one you're currently in.     |
| `C-c c n`  | Fix indentation in buffer and strip whitespace.               |
| `C-c c p`  | display the full path of current file.                        |
| `C-c c r`  | Rename the current buffer and its visiting file if any.       |
| `C-c c s`  | Swap two active windows.                                      |
| `C-c c u`  | Select and open file in various sources.                      |
| `C-c c w`  | display default window configuration.                         |


## emacs列操作

| key       | description                                       |
|:---------:|:--------------------------------------------------|
| C-x SPC   | `rectangle-mark-mode`: 选中区域高亮以列模式显示   |
| C-x r c   | `clear-rectangle`: 用空白字符替代矩形区域内容     |
| C-x r d   | `delete-rectangle`: 删除列                        |
| C-x r w   | `copy-rectangle`: 拷贝列                          |
| C-x r y   | `yank-rectangle`: 粘贴列                          |
| C-x r k   | `kill-rectangle`: 删除列, 并放入kill-ring         |
| C-x r o   | `open-rectangle`: 在矩形区域处插入空白字符        |
| C-x r t   | `string-rectangle`: 替换矩形区域的内容            |
| C-x r N   | `rectangle-number-lines`: 插入行号到每一行        |


## helm

helm是一个非常强大的emacs插件. 网上有一个非常全面的教程:

`https://tuhdo.github.io/helm-intro.html`

这里列举其中的要点以及我们做出的选择, 并阐明原因.

* 不用`helm-autoresize-mode`, 窗口大小变来变去比较晃眼.
* 在`helm-buffer`, 可以用`C-SPC`选中, `M-a`全选.
* 在`helm-buffer`, 可以用`C-c C-i`将选中的item插入到当前buffer中.
* 在`helm-buffer`, 可以用`C-t`切换`helm-buffer`的窗口位置.
* 在`helm-buffer`, 光标位于一个文件或者buffer时, 可以用`C-s`查找.
* 在`helm-M-x`之后, `Tab`键打开当前命令的文档.
* 在`helm-find-files`之后, `C-l`跳转到目录, `C-r`回退到上一个位置.
* `helm-regexp`只能做递增搜索(在尾部修改`regex`), 否则会很慢, 甚至会卡死.

一些有用的绑定键

* C-c h o / C-o: helm-swoop, 在buffer中进行查找
* C-c h SPC / vv: helm-all-mark-rings, 显示所有的mark-ring
* C-c h i: helm-semantic-or-imenu, 显示所有的tag
* C-c h b: helm-resume, resume the previous Helm session

* M-y: helm-show-kill-ring, 显示所有的kill内容
* C-o: helm-occur-from-isearch (isearch的时候显示所有查找的内容)
* C-c C-l: helm-minibuffer-history (mini-buffer中显示历史记录)
* C-c C-l: helm-comint-input-ring (shell-mode中显示所有输入命令)


## Ivy, Counsel and Swiper

Ivy是一个补全框架, 类似于helm. 和Counsel, Swiper结合之后, 能提供helm类似的功能.
目前选择用helm, 以后有时间有机会可以尝试一下ivy.

参考:

1. `https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html`
2. `https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/`
3. `http://oremacs.com/swiper/`


## helm-projectile

一些有用的绑定键

* `C-c p p`: projectile-switch-project, 切换工程目录
* `C-c p h`: helm-projectile, 列举当前工程的buffer, file, 和所有工程目录
* `C-c p a`: projectile-find-other-file, 打开与当前文件同名但不同后缀的文件
* `C-c p b`: projectile-switch-to-buffer, 列出属于当前工程的buffer
* `C-c p c`: projectile-compile-project, 编译当前工程
* `C-c p e`: projectile-recentf, 列出当前工程中最近访问的文件
* `C-c p f`: projectile-find-file, 在当前工程中找文件
* `C-c p s g`: projectile-grep, 在当前工程中用grep查找

参考:

1. `https://tuhdo.github.io/helm-projectile.html`

## neotree & dired

neotree为emacs的侧边栏组件, dired为emacs的文件管理器, 由于两者都可以对文件进行拷
贝, 删除, 重名名, 等操作, 这里除了各自的绑定键之外, 也将对应的键绑定到`C-c d`上.
这一部分按键请参考`which-key`提示.

neotree绑定键

* {: shrink-window-horizontally, 缩小侧边栏
* }: enlarge-window-horizontally, 增大侧边栏
* tab: neotree-quick-look, 在左侧窗口查看文件
* ret: neotree-quick-look, 在左侧窗口打开文件
* C-tab: personal-neotree-view-file-other-window, 在右侧窗口查看文件
* C-ret: personal-neotree-open-file-other-window, 在右侧窗口打开文件


## c++配置

#### cpp-lsp

c++配置lsp-clangd, 目前用的c++ language server为clangd.

clangd的二进制文件下载: https://releases.llvm.org/download.html

跳转和补全用clangd, linter用clang-tidy, auto-formatter用clang-format.

clangd和clang-tidy需要compile_commands.json, 可以用`bear make -ik all`来生成.


## python配置

#### python-lsp

python配置用python-lsp, 目前用的python language server为python-lsp-server.

python-lsp-server支持的插件如下:

1. jedi (enabled): 补全和跳转. 重要的基础功能.
2. mccabe (enabled): 检查程序圈复杂度.
3. pylint (disabled): 检查程序的错误和风格.
4. pycodestyle (disabled): 检查程序的风格.
5. pydocstyle (enabled): 检查文档的风格.
6. pyflakes (disabled): 检查程序的错误.
7. flake8 (enabled): pycodestyle, pyflakes, mccabe的组合.
8. autopep8 (disabled): 自动排版工具, 依赖pycodestyle.
9. yapf (disabled): 自动排版工具, 可深度定制.

市场上常见的linter和auto-formatter工具如下:

|             | 类型           | 检查错误 | 检查风格 | 配置支持类型 |
|:-----------:|:--------------:|:--------:|:--------:|:------------:|
| pylint      | linter         | 是       | 是       | PFBL         |
| pycodestyle | linter         | 否       | 是       | PFL          |
| Pyflakes    | linter         | 是       | 否       | --           |
| flake8      | linter         | 是       | 是       | PFL          |
| autopep8    | auto-formatter | --       | --       | PFL          |
| black       | auto-formatter | --       | --       | --           |
| yapf        | auto-formatter | --       | --       | PFBL         |

说明:

1. 配置支持类型中, P表示项目级别, F表示文件级别, B表示代码块级别, L表示行级别.
2. black是python基金会维护的开源项目, 提供一致的排版体验, 不支持定制. yapf是
   google维护的开源项目, 提供可定制的排版功能. 目前, pyls只支持yapf, 不支持
   black.

选择:

1. 跳转和补全用jedi.
2. linter用flake8或pylint, 选pylint, 因为pylint可定制化更好.
3. auto-formatter用autopep8或者yapf. 选yapf, 因为yapf可定制化更好.

#### pylintrc

pylint的配置文件为:pylintrc. 如果python文件不在module中, 则pylint不会在其父目录
寻找pylintrc, 这里我们通过修改pylint的源码来确保pylint能找到对应的配置文件.
源码位于: `/pylint/config/find_default_config_files.py`
在`find_default_config_files`函数中, 去掉对`__init__.py`的限制即可.
