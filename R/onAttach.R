#' @importFrom utils packageVersion
.onAttach = function( libname, pkgname ) {
  # Runs when attached to search() path such as by library() or require()
  if ( interactive() ) {
    packageStartupMessage( 'Welcome to msmtools v', as.character( packageVersion( "msmtools" ) ),
                           '! An easy way to build augmented longitudinal datasets to be used with msm' )
    packageStartupMessage( 'If you find a bug, please report it at https://github.com/contefranz/msmtools/issues' )
  }
}
