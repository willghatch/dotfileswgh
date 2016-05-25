" Vim color file

" based on sift theme and my ad-hoc emacs theme
" http://www.vim.org/scripts/script.php?script_id=1472

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name="wgh-dark"

hi Normal       guifg=#909090 guibg=#050505
hi NonText      guifg=#787878 guibg=#050505

" syntax highlighting """"""""""""""""""""""""""""""""""""""""

hi Comment		guifg=#683d8b gui=italic cterm=italic
hi Title		guifg=#09f909  gui=none
hi Underlined   guifg=#49f95f gui=none

hi Statement    guifg=#efef70  "gui=none
hi Type			guifg=#60e5bf  gui=none
hi Constant		guifg=#0ad8ff
hi Number		guifg=#eabaff
hi PreProc      guifg=#f085b4
hi Identifier   guifg=#50f0b4 gui=bold
hi Special		guifg=#88a0d4
hi Operator		guifg=#f0da00
"hi Keyword		guifg=green
"hi Error        guibg=#408452
hi Function     guifg=#c08a50 guibg=bg gui=bold "or green 50b3b0
hi Conditional	guifg=#f8f024 guibg=bg
hi Repeat		guifg=#ffa400 guibg=bg gui=bold
hi Exception	guifg=#dfff80
"hi Ignore       guifg=grey40
"hi Todo			guifg=orangered guibg=yellow2
"hi Label gui=None guifg=LightGreen guibg=bg
"highlight Operator gui=None guifg=#daca65 guibg=bg
"highlight Keyword gui=bold guifg=grey guibg=bg
"highlight Exception gui=none guifg=#ea5460 guibg=bg

"end syntax highlighting """""""""""""""""""""""""""""""""""""

" highlight groups
hi Directory	guifg=#bbd0df

hi DiffAdd      guibg=#9af5c0 guifg=#05293d
hi DiffDelete   guibg=#aa0500 guifg=#a5293d
hi DiffChange   guibg=#0ab5c0 guifg=#05293d
hi DiffText     guibg=#aae5d0 guifg=#05293d

hi ErrorMsg     guibg=#ff4545

hi Cursor       guibg=#cad5c0 guifg=#05293d
hi CursorLine   cterm=NONE guibg=#0c1d23 ctermbg=235
hi CursorColumn cterm=NONE guibg=#0c1d23 ctermbg=235

hi Folded       guibg=#201328 guifg=#BBDDCC
hi FoldColumn	guibg=#130014 guifg=#dbcaa5
"hi FoldColumn	guibg=#83a5cd guifg=#70459F
hi LineNr       guibg=#161616 guifg=#309f9f
"hi LineNr       guibg=#081c30 guifg=#80a0dA
hi StatusLine	guibg=#20aaea guifg=#202050 gui=bold
hi StatusLineNC	guibg=#2a90c0 guifg=#204050 gui=bold

hi Search       guibg=#00006f guifg=#77c7ff
hi IncSearch	guibg=#a33067

hi VertSplit	guibg=#325f95 guifg=grey50 gui=none
hi ModeMsg    	guifg=#00AACC
hi MoreMsg      guifg=SeaGreen
hi Question    	guifg=#AABBCC
hi SpecialKey	guifg=#90dcb0
hi Visual       guifg=#4a8FcF guibg=#032F5F
"hi VisualNOS
hi WarningMsg	guifg=salmon
"hi WildMenu
"hi Menu
"hi Scrollbar
"hi Tooltip

hi Matchmaker               guifg=#FFFFFF guibg=#3364b5

hi SignColumn        guibg=#112121
hi GitGutterAdd      guifg=#003000 guibg=#33a333 gui=bold ctermfg=2 ctermbg=4
hi GitGutterChange   guifg=#000050 guibg=#0383f3 gui=bold ctermfg=3 ctermbg=4
hi GitGutterDelete   guifg=#000000 guibg=#a33333 gui=bold ctermfg=1 ctermbg=4

hi Pmenu        guibg=#3a6595 guifg=#9aadd5
hi PmenuSel     guibg=#4a85ba guifg=#b0d0f0
hi MatchParen   guibg=#200080 guifg=#f0f080

" Common groups that link to default highlighting.
" You can specify other highlighting easily.
"hi link String	Constant
