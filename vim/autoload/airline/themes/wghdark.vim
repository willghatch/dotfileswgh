" vim-airline companion theme of Wghdark
" (https://github.com/Pychimp/vim-wghdark)

let g:airline#themes#wghdark#palette = {}

let g:airline#themes#wghdark#palette.accents = {
      \ 'red': [ 'yellow' , '' , 160 , '' , '' ],
      \ }

let s:N1 = [ 'black' , 'cyan' , 17 , 81 ]
let s:N2 = [ '#00afd7' , '#161616' , 255 , 238 ]
let s:N3 = [ '#00afd7' , '#202020' , 85 , 234 ]
let g:airline#themes#wghdark#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#wghdark#palette.normal_modified = {
      \ 'airline_c': [ '#00afd7' , '#54010b' , 255 , 53 , '' ] ,
      \ }


let s:I1 = [ 'black' , 'magenta' , 17 , 5  ]
let s:I2 = [ '#00afd7' , '#161616' , 255 , 27 ]
let s:I3 = [ '#00afd7' , '#202020' , 15 , 17 ]
let g:airline#themes#wghdark#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#wghdark#palette.insert_modified = {
      \ 'airline_c': [ '#00afd7' , '#54010b' , 255 , 53 , '' ] ,
      \ }
let g:airline#themes#wghdark#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#09643f' , s:I1[2] , 129 , '' ] ,
      \ }


let g:airline#themes#wghdark#palette.replace = copy(g:airline#themes#wghdark#palette.insert)
let g:airline#themes#wghdark#palette.replace.airline_a = [ s:I1[0] , '#ff2121' , s:I1[2] , 196 , '' ]
let g:airline#themes#wghdark#palette.replace.airline_z = [ s:I1[0] , '#ff2121' , s:I1[2] , 196 , '' ]
let g:airline#themes#wghdark#palette.replace_modified = g:airline#themes#wghdark#palette.insert_modified

let s:V1 = [ 'black' , 'green' , 232 , 50 ]
let s:V2 = [ '#00afd7' , '#161616' , 232 , 2 ]
let s:V3 = [ '#00afd7' , '#202020' , 15 , 28 ]
let g:airline#themes#wghdark#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#wghdark#palette.visual_modified = {
      \ 'airline_c': [ '#00afd7' , '#54010b' , 255 , 53 , '' ] ,
      \ }

let s:IA = [ '#777777' , '#c7c7c7' , 239 , 234 , '' ]
let g:airline#themes#wghdark#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#wghdark#palette.inactive_modified = {
      \ 'airline_c': [ '#ff3535' , '' , 97 , '' , '' ] ,
       \ }

let g:airline#themes#wghdark#palette.tabline = {
      \ 'airline_tab':      ['#00afd7', '#161616',  85, 234, ''],
      \ 'airline_tabsel':   ['#161616', '#00afd7',  234, 85 , ''],
      \ 'airline_tabtype':  ['#00afd7', '#202020',  85, 234, ''],
      \ 'airline_tabfill':  ['#00afd7', '#161616',  85, 234, ''],
      \ 'airline_tabmod':   ['#54010b', '#00afd7',  85, 124, ''],
      \ }

"warning on the bottom right corner
let s:WI = [ '#54d632', '#472323', 255, 166 ]
let g:airline#themes#wghdark#palette.normal.airline_warning = [
     \ s:WI[0], s:WI[1], s:WI[2], s:WI[3]
     \ ]

let g:airline#themes#wghdark#palette.normal_modified.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.insert.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.insert_modified.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.visual.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.visual_modified.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.replace.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

let g:airline#themes#wghdark#palette.replace_modified.airline_warning =
    \ g:airline#themes#wghdark#palette.normal.airline_warning

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#wghdark#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#343434' , '#c7c7c7' , 237 , 251 , ''     ] ,
      \ [ '#343434' , '#b3b3b3' , 237 , 250 , ''     ] ,
      \ [ '#eeeeee' , '#007fff' , 255 , 27  , ''     ] )
