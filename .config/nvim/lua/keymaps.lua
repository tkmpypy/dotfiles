vim.keymap.set("v", "*", function()
  vim.cmd([[
    let reg = '"'
    let [save_reg, save_type] = [getreg(reg), getregtype(reg)]
    normal! gv""y
    let text = @"
    call setreg(reg, save_reg, save_type)

    let @/ = text
    call histadd('/', text)
  ]])
end, { noremap = true, silent = false, desc = "Visual Search" })
vim.keymap.set("n", "<leader>pp", function()
  vim.cmd([[
    let @* = expand('%')
  ]])
end, { noremap = true, silent = false, desc = "Register path relative" })
vim.keymap.set("n", "<leader>pP", function()
  vim.cmd([[
      let @* = expand('%:p')
    ]])
end, { noremap = true, silent = false, desc = "Register path absolute" })
vim.keymap.set("n", "<leader>pf", function()
  vim.cmd([[
    let @* = expand('%:t')
  ]])
end, { noremap = true, silent = false, desc = "Register path filename" })

vim.keymap.set("n", "<leader>wmh", "<C-w>H", { noremap = true, silent = true, desc = "Move window H" })
vim.keymap.set("n", "<leader>wmj", "<C-w>J", { noremap = true, silent = true, desc = "Move window J" })
vim.keymap.set("n", "<leader>wmk", "<C-w>K", { noremap = true, silent = true, desc = "Move window K" })
vim.keymap.set("n", "<leader>wml", "<C-w>L", { noremap = true, silent = true, desc = "Move window L" })
vim.keymap.set("n", "<leader>bn", function()
  vim.fn.execute("bnext")
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bp", function()
  vim.fn.execute("bprevious")
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bra", function()
  vim.fn.execute("bufdo e!")
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tn", function()
  vim.fn.execute("tabn")
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tp", function()
  vim.fn.execute("tabp")
end, { noremap = true, silent = true })
vim.keymap.set("n", "j", "gj", { noremap = true, silent = true })
vim.keymap.set("n", "k", "gk", { noremap = true, silent = true })
vim.keymap.set("i", "<C-c>", "<ESC><ESC>", { noremap = true, silent = true })
vim.keymap.set("t", "<C-w>N", "<C-\\><C-n>", { noremap = true, silent = true })
vim.keymap.set("t", "<ESC>", "<C-\\><C-n>", { noremap = true, silent = true })
