local telescope = require('telescope')
telescope.load_extension('jumps')
telescope.setup{
  defaults = {
    shortlen_path = true,
    winblend = 10,
    scroll_strategy = 'cycle',
    color_devicon = true,
  }
}
