local config = require('codecompanion.config')

assert(
  config.interactions.chat.adapter.name == 'opencode',
  'CodeCompanion chat must use OpenCode ACP'
)
assert(
  config.interactions.inline.adapter == 'anthropic',
  'Inline requests must retain their HTTP adapter'
)
assert(
  config.extensions.history.opts.title_generation_opts.adapter == 'anthropic',
  'History titles must retain their HTTP adapter'
)
assert(
  config.extensions.history.opts.summary.generation_opts.adapter == 'anthropic',
  'History summaries must retain their HTTP adapter'
)
assert(
  config.extensions.history.opts.continue_last_chat == false,
  'History must not kill a new ACP connection while restoring the last chat'
)
