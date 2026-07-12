-- CodeCompanion.nvim + AI-adjacent plugin config (render-markdown, img-clip).
--
-- NOTE: file is named codecompanion_nvim.lua (not codecompanion.lua) on purpose:
-- a lua/codecompanion.lua on the runtimepath would shadow the plugin's own
-- `codecompanion` module and break require('codecompanion'). Same reason
-- lua/hop_nvim.lua isn't named hop.lua. Load via require('codecompanion_nvim').
--
-- Uses CodeCompanion's built-in `anthropic` adapter (reads $ANTHROPIC_API_KEY).

require('render-markdown').setup({
  file_types = { 'codecompanion' },
})

require('img-clip').setup({
  default = {
    dir_path = 'assets', -- where images get saved
    extension = 'png',
    file_name = '%Y-%m-%d-%H-%M-%S',
    use_absolute_path = false,
    relative_to_current_file = false,
    prompt_for_file_name = true,
    insert_mode_after_paste = true,
    drag_and_drop = {
      enabled = true,
      insert_mode = false,
    },
  },
  -- Per-filetype overrides, e.g. for markdown / codecompanion:
  filetypes = {
    markdown = {
      url_encode_path = true,
      template = '![$CURSOR]($FILE_PATH)',
    },
    codecompanion = {
      prompt_for_file_name = false,
      template = '[Image]($FILE_PATH)',
      use_absolute_path = true,
    },
  },
})

require('codecompanion').setup({
  display = {
    chat = {
      window = {
        layout = 'vertical',
        position = 'right',
        width = 0.3,
      },
    },
  },
  interactions = {
    chat = {
      adapter = 'anthropic',
      opts = { completion_provider = 'coc' },
      roles = {
        llm = function(adapter)
          local model = adapter.schema and adapter.schema.model and adapter.schema.model.default
          return 'CodeCompanion (' .. (model or adapter.formatted_name) .. ')'
        end,
      },
      -- Slash commands (/buffer, /file, /symbols, /help) already work via the
      -- built-in "default" provider (vim.ui.select) — nothing extra needed.
      -- To get a fuzzy picker instead, the accepted providers are telescope,
      -- fzf_lua (ibhagwan/fzf-lua), mini_pick, snacks, default. NOTE: our
      -- junegunn/fzf + fzf.vim is NOT a supported provider. To upgrade, install
      -- one of those plugins and set e.g.:
      -- slash_commands = {
      --   ["buffer"] = { opts = { provider = "fzf_lua" } },
      --   ["file"]   = { opts = { provider = "fzf_lua" } },
      --   ["symbols"] = { opts = { provider = "fzf_lua" } },
      --   ["help"]   = { opts = { provider = "fzf_lua" } },
      -- },
    },
    inline = { adapter = 'anthropic' },
  },
  extensions = {
    history = {
      enabled = true,
      opts = {
        -- Keymap to open history from chat buffer (default: gh)
        keymap = 'gh',
        -- Keymap to save the current chat manually (when auto_save is disabled)
        save_chat_keymap = 'sc',
        -- Save all chats by default (disable to save only manually using 'sc')
        auto_save = true,
        -- Number of days after which chats are automatically deleted (0 to disable)
        expiration_days = 0,
        -- Picker interface (auto resolved to a valid picker)
        picker = 'default', --- ("telescope", "snacks", "fzf-lua", or "default")
        ---Optional filter function to control which chats are shown when browsing
        chat_filter = nil, -- function(chat_data) return boolean end
        -- Customize picker keymaps (optional)
        picker_keymaps = {
          rename = { n = 'r', i = '<M-r>' },
          delete = { n = 'd', i = '<M-d>' },
          duplicate = { n = '<C-y>', i = '<C-y>' },
        },
        ---Automatically generate titles for new chats
        auto_generate_title = true,
        title_generation_opts = {
          ---Adapter for generating titles (defaults to current chat adapter)
          --adapter = nil, -- "copilot"
          ---Model for generating titles (defaults to current chat model)
          --model = nil, -- "gpt-4o"
          ---Number of user prompts after which to refresh the title (0 to disable)
          refresh_every_n_prompts = 0, -- e.g., 3 to refresh after every 3rd user prompt
          ---Maximum number of times to refresh the title (default: 3)
          max_refreshes = 3,
          format_title = function(original_title)
            -- this can be a custom function that applies some custom
            -- formatting to the title.
            return original_title
          end,
        },
        ---On exiting and entering neovim, loads the last chat on opening chat
        continue_last_chat = true,
        ---When chat is cleared with `gx` delete the chat from history
        delete_on_clearing_chat = false,
        ---Directory path to save the chats
        dir_to_save = vim.fn.stdpath('data') .. '/codecompanion-history',
        ---Enable detailed logging for history extension
        enable_logging = false,

        -- Summary system
        summary = {
          -- Keymap to generate summary for current chat (default: "gcs")
          create_summary_keymap = 'gcs',
          -- Keymap to browse summaries (default: "gbs")
          browse_summaries_keymap = 'gbs',

          generation_opts = {
            adapter = nil, -- defaults to current chat adapter
            model = nil, -- defaults to current chat model
            context_size = 90000, -- max tokens that the model supports
            include_references = true, -- include slash command content
            include_tool_outputs = true, -- include tool execution results
            system_prompt = nil, -- custom system prompt (string or function)
            format_summary = nil, -- custom function to format generated summary e.g to remove <think/> tags from summary
          },
        },
      },
    },
  },
})

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'codecompanion',
  callback = function()
    vim.opt_local.cursorcolumn = false
    -- vim.opt_local.cursorline = false
  end,
})

-- Bind a key to the :PasteImage command
vim.keymap.set('n', '<leader>p', '<cmd>PasteImage<cr>', { desc = 'Paste image from clipboard' })
vim.keymap.set('n', '<leader>C', function()
  require('codecompanion').toggle()
end, { silent = true, desc = 'CodeCompanion toggle' })
vim.keymap.set('v', '<leader>C', ':CodeCompanionChat Add<CR>')
