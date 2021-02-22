local telescope = require('telescope')
telescope.load_extension('jumps')
telescope.setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '--hidden'
    },
    shortlen_path = true,
    winblend = 10,
    scroll_strategy = 'cycle',
    color_devicon = true,
  }
}
