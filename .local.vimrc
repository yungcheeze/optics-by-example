let g:neoformat_enabled_haskell = ['ormolu']
augroup fmt
  autocmd!
  autocmd BufWritePre * Neoformat
augroup END

nnoremap <leader>es <cmd>Esrc<cr>
nnoremap <leader>et <cmd>Espec<cr>
