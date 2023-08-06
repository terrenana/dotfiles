local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    "vim-airline/vim-airline",
    "vim-airline/vim-airline-themes",
    "arcticicestudio/nord-vim",
    "neovim/nvim-lspconfig",
    "simrat39/rust-tools.nvim",
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-vsnip",
    "hrsh7th/vim-vsnip",
    "hrsh7th/cmp-path",
    "nvim-treesitter/nvim-treesitter",
    "nvim-telescope/telescope.nvim",
    "nvim-lua/plenary.nvim",
    {"windwp/nvim-autopairs", event = "InsertEnter", opts = {}},
    "tpope/vim-surround",
    "RRethy/vim-illuminate",
    "numToStr/Comment.nvim",
    "nvim-tree/nvim-web-devicons"
}

require("lazy").setup(plugins)

vim.cmd("colorscheme nord")
vim.g.airline_theme = "nord"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.nu = true
vim.opt.rnu = true

-- airline powerline fonts
vim.cmd([[let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '']])

-- disable vi compatibility
vim.opt.compatible = false

vim.opt.showmatch = true
vim.opt.ignorecase = true

-- paste with middle click
vim.opt.mouse = "v"

-- hightlight search
vim.opt.hlsearch = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

vim.opt.swapfile = false

-- keybind stuff
function map(mode, bind, cmd, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, bind, cmd, options)
end

vim.cmd("let mapleader='/'")
map("n", "<leader>w", ":wq!<cr>")
map("n", "<leader>s", ":w!<cr>")
map("n", "<leader>q", ":q!<cr>")
map("n", "<leader>f", ":Telescope find_files<cr>")
map("n", "<leader>g", ":Telescope live_grep<cr>")
map("n", "<leader>z", ":Telescope current_buffer_fuzzy_find<cr>")

-- lsp stuff
local rt = require("rust-tools")
rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- code actions
            vim.keymap.set("n", "<leader>a", rt.code_action_group.code_action_group, {buffer = bufnr})
        end
    }
})
vim.opt.completeopt = {"menuone", "noselect", "noinsert"}
vim.opt.shortmess = vim.opt.shortmess + { c = true }
vim.api.nvim_set_option("updatetime", 300)

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])

local cmp = require("cmp")
cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    mapping = {
        ['<up>'] = cmp.mapping.select_prev_item(),
        ['<down>'] = cmp.mapping.select_next_item(),
        ['<S-down>'] = cmp.mapping.scroll_docs(4),
        ['<S-up>'] = cmp.mapping.scroll_docs(-4),
        ['<esc>'] = cmp.mapping.close(),
        ['<return>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        })
    },
    sources = {
    { name = "path"},
    { name = "nvim_lsp", keyword_length = 3},
    { name = "nvim_lua_signature_help"},
    { name = "nvim_lua", keyword_length = 2},
    { name = "buffer", keyword_length = 2 },
    { name = "calc" }
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered()
    },
    formatting = {
        fields = {"menu", "abbr", "kind"},
        format = function(entry, item)
            local menu_icon = {
                nvim_lsp = "C",
                vsnip = "S",
                buffer = "B",
                path = "P"
            }
            item.menu = menu_icon[entry.source.name]
            return item
        end,
    }
})
require("nvim-treesitter.configs").setup {
    ensure_installed = {"lua", "rust", "toml"},
    auto_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
    ident = {enable = true},
    rainbow =  {
        enable = true,
        extended_mode = true,
        max_file_lines = nil,
    }
}
-- comment with keybind stuff
require("Comment").setup()

-- telescope stuff
local telescope = require("telescope")

telescope.setup {
    pickers = {
        find_files = {
            hidden = true
        }
    }
}
