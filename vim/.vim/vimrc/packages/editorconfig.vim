" editorconfig.vim settings

" disable loading of editorconfig on fugitive and ssh buffers
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" Add filetype hook for non-standard editorconfig properties
function! FiletypeHook(config)
    if has_key(a:config, 'filetype')
        let &filetype = a:config['filetype']
    endif
    return 0   " Return 0 to show no error happened
endfunction

call editorconfig#AddNewHook(function('FiletypeHook'))
