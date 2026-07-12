function _tide_item_nohist
    set -q fish_history; and test -z "$fish_history"; or return
    _tide_print_item nohist \uf02d\ no-hist
end
