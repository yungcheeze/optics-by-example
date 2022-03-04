let g:neoformat_enabled_haskell = ['ormolu']
augroup fmt
  autocmd!
  autocmd BufWritePre * Neoformat
augroup END

nnoremap <leader>es :Esrc<space>
nnoremap <leader>et :Espec<space>
