#' @importFrom utils packageVersion
.onAttach = function( libname, pkgname ) {
  # Runs when attached to search() path such as by library() or require()
  if ( interactive() ) {
    packageStartupMessage( 'Welcome to msmtools v', as.character( packageVersion( "msmtools" ) ),
                           '! An easy way to build augmented longitudinal datasets to be used with msm' )
    packageStartupMessage( 'msmtools requires data.table (>= 1.9.6), msm (>= 1.6), survival (>= 2.38.0)',
                           ' and ggplot2 (>=3.3.3)')
    packageStartupMessage( 'For help ?msmtools or vignette( "msmtools" )' )
    packageStartupMessage( 'If you find a bug, please report it at https://github.com/contefranz/msmtools/issues' )
  }
}
