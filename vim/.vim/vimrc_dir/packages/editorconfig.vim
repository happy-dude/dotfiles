" editorconfig.vim settings

" disable loading of editorconfig on fugitive and ssh buffers
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" disable editorconfig max line length indicator
let g:EditorConfig_max_line_indicator = 'none'
