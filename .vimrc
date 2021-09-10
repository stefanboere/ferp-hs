" Move the vim node version up front
let $PATH = '/nix/store/pvnvfvimzldwbx08pj53iaf3z1wmwl94-nodejs-14.17.5/bin:' . $PATH

" Use the brittany version from the path
autocmd FileType haskell setlocal equalprg=brittany
