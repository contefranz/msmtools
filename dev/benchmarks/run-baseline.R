#!/usr/bin/env Rscript

options( warn = 1 )

script_arg = grep( "^--file=", commandArgs( trailingOnly = FALSE ), value = TRUE )
script_file = if ( length( script_arg ) ) {
  sub( "^--file=", "", script_arg[[ 1L ]] )
} else {
  file.path( "dev", "benchmarks", "run-baseline.R" )
}
script_dir = dirname( normalizePath( script_file, winslash = "/", mustWork = TRUE ) )
repo_root = normalizePath( file.path( script_dir, "..", ".." ), winslash = "/" )
setwd( repo_root )

if ( requireNamespace( "pkgload", quietly = TRUE ) ) {
  pkgload::load_all( repo_root, quiet = TRUE )
} else if ( requireNamespace( "devtools", quietly = TRUE ) ) {
  devtools::load_all( repo_root, quiet = TRUE )
} else {
  stop( "Install pkgload or devtools to run source-tree benchmarks.", call. = FALSE )
}

library( data.table )

has_peakram = requireNamespace( "peakRAM", quietly = TRUE )

make_synthetic = function( n_subjects, events_per_subject = 4L,
                           time_type = c( "Date", "numeric", "difftime" ),
                           include_more_status = TRUE ) {
  time_type = match.arg( time_type )
  stopifnot( n_subjects >= 1L, events_per_subject >= 2L )

  subj = rep( seq_len( n_subjects ), each = events_per_subject )
  adm_number = sequence( rep( events_per_subject, n_subjects ) )
  subject_start = rep( seq.int( 0L, by = 20L, length.out = n_subjects ),
                       each = events_per_subject )
  event_start = rep( seq.int( 0L, by = 4L, length.out = events_per_subject ),
                     times = n_subjects )

  status_levels = c( "alive", "dead_in", "dead_out" )
  label_3_by_subject = status_levels[ ( seq_len( n_subjects ) - 1L ) %% 3L + 1L ]
  label_3 = rep( label_3_by_subject, each = events_per_subject )
  label_2 = fifelse( label_3 == "alive", "alive", "dead" )

  date_in = as.Date( "2020-01-01" ) + subject_start + event_start
  date_out = date_in + 2L
  date_cens = rep( as.Date( "2020-01-01" ) +
                     seq.int( 0L, by = 20L, length.out = n_subjects ) +
                     ( events_per_subject - 1L ) * 4L + 2L,
                   each = events_per_subject )

  rehab_it = rep( c( "df", "rehab", "it", "df" ),
                  length.out = length( subj ) )

  data = data.table(
    subj = subj,
    adm_number = as.integer( adm_number ),
    label_2 = label_2,
    label_3 = label_3,
    dateIN = date_in,
    dateOUT = date_out,
    dateCENS = date_cens,
    rehab_it = rehab_it
  )

  if ( time_type == "numeric" ) {
    origin = as.Date( "2020-01-01" )
    data[ , dateIN := as.numeric( dateIN - origin ) ]
    data[ , dateOUT := as.numeric( dateOUT - origin ) ]
    data[ , dateCENS := as.numeric( dateCENS - origin ) ]
  } else if ( time_type == "difftime" ) {
    origin = as.Date( "2020-01-01" )
    data[ , dateIN := as.difftime( as.numeric( dateIN - origin ),
                                   units = "days" ) ]
    data[ , dateOUT := as.difftime( as.numeric( dateOUT - origin ),
                                    units = "days" ) ]
    data[ , dateCENS := as.difftime( as.numeric( dateCENS - origin ),
                                     units = "days" ) ]
  }

  if ( !include_more_status ) {
    data[ , rehab_it := NULL ]
  }

  data[]
}

measure = function( label, fun ) {
  gc()
  peak_mib = NA_real_
  result = NULL

  elapsed = system.time( {
    if ( has_peakram ) {
      peak = peakRAM::peakRAM( result <- fun() )
      peak_mib = peak$Peak_RAM_Used_MiB[[ 1L ]]
    } else {
      result = fun()
    }
  } )[[ "elapsed" ]]

  list( label = label, elapsed = unname( elapsed ), peak_mib = peak_mib,
        result = result )
}

augment_with_more_status = function( data ) {
  suppressWarnings(
    augment( data, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, more_status = rehab_it,
             verbose = FALSE )
  )
}

augment_without_events = function( data ) {
  suppressWarnings(
    augment( data, subj, pattern = label_3, t_start = dateIN, t_end = dateOUT,
             t_cens = dateCENS, verbose = FALSE )
  )
}

polish_augmented = function( data ) {
  if ( "augmented_int" %in% names( data ) ) {
    polish( data, subj, label_3, time = augmented_int, verbose = FALSE )
  } else if ( "augmented_num" %in% names( data ) ) {
    polish( data, subj, label_3, time = augmented_num, verbose = FALSE )
  } else {
    polish( data, subj, label_3, time = augmented, verbose = FALSE )
  }
}

equivalence_checks = function( data ) {
  converted = suppressWarnings(
    augment( copy( data ), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, convert = TRUE,
             verbose = FALSE )
  )
  as_dt = suppressWarnings(
    augment( copy( data ), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, verbose = FALSE )
  )

  no_events = augment_without_events( copy( data ) )

  list(
    convert_equivalent = isTRUE( all.equal( as.data.frame( as_dt ), converted,
                                            check.attributes = FALSE ) ),
    missing_events_equivalent = identical( as_dt$adm_number, no_events$n_events ),
    expected_columns = all( c( "status", "status_num", "n_status",
                              "augmented" ) %in% names( as_dt ) )
  )
}

side_effect_checks = function( data ) {
  augment_input = copy( data )
  before_augment_names = copy( names( augment_input ) )
  before_augment_key = copy( key( augment_input ) )
  invisible( augment_without_events( augment_input ) )

  augmented = augment_with_more_status( copy( data ) )
  polish_input = copy( augmented )
  before_polish_names = copy( names( polish_input ) )
  before_polish_key = copy( key( polish_input ) )
  invisible( polish_augmented( polish_input ) )

  list(
    augment_added_n_events = !"n_events" %in% before_augment_names &&
      "n_events" %in% names( augment_input ),
    augment_key_changed = !identical( before_augment_key, key( augment_input ) ),
    polish_names_preserved = identical( before_polish_names, names( polish_input ) ),
    polish_key_changed = !identical( before_polish_key, key( polish_input ) )
  )
}

sizes = data.table(
  size = c( "small", "medium", "large" ),
  subjects = c( 100L, 1000L, 10000L )
)
time_types = c( "Date", "numeric", "difftime" )

rows = list()
counter = 1L

for ( i in seq_len( nrow( sizes ) ) ) {
  for ( time_type in time_types ) {
    data = make_synthetic( sizes$subjects[[ i ]], time_type = time_type )
    data_rows = nrow( data )

    aug = measure( "augment", function() augment_with_more_status( copy( data ) ) )
    clean = measure( "polish", function() {
      polish_augmented( copy( aug$result ) )
    } )

    eq = equivalence_checks( data )
    side = side_effect_checks( data )

    rows[[ counter ]] = data.table(
      size = sizes$size[[ i ]],
      subjects = sizes$subjects[[ i ]],
      rows = data_rows,
      time_type = time_type,
      operation = c( "augment", "polish" ),
      elapsed_sec = round( c( aug$elapsed, clean$elapsed ), 3L ),
      peak_mib = round( c( aug$peak_mib, clean$peak_mib ), 2L ),
      convert_equivalent = eq$convert_equivalent,
      missing_events_equivalent = eq$missing_events_equivalent,
      expected_columns = eq$expected_columns,
      augment_added_n_events = side$augment_added_n_events,
      augment_key_changed = side$augment_key_changed,
      polish_names_preserved = side$polish_names_preserved,
      polish_key_changed = side$polish_key_changed
    )
    counter = counter + 1L
  }
}

baseline = rbindlist( rows )

format_bool = function( x ) ifelse( isTRUE( x ), "yes", "no" )
format_peak = function( x ) ifelse( is.na( x ), "not recorded", sprintf( "%.2f", x ) )

markdown_table = function( data ) {
  header = paste(
    "| Size | Subjects | Rows | Time type | Operation | Runtime (s) | Peak MiB | Checks | Side effects |",
    "| --- | ---: | ---: | --- | --- | ---: | ---: | --- | --- |",
    sep = "\n"
  )

  body = apply( data, 1L, function( row ) {
    checks = paste0(
      "convert=", format_bool( as.logical( row[[ "convert_equivalent" ]] ) ),
      "; n_events=", format_bool( as.logical( row[[ "missing_events_equivalent" ]] ) ),
      "; columns=", format_bool( as.logical( row[[ "expected_columns" ]] ) )
    )
    effects = paste0(
      "augment_adds_n_events=", format_bool( as.logical( row[[ "augment_added_n_events" ]] ) ),
      "; augment_key=", format_bool( as.logical( row[[ "augment_key_changed" ]] ) ),
      "; polish_names=", format_bool( as.logical( row[[ "polish_names_preserved" ]] ) ),
      "; polish_key=", format_bool( as.logical( row[[ "polish_key_changed" ]] ) )
    )
    sprintf(
      "| %s | %s | %s | %s | %s | %.3f | %s | %s | %s |",
      row[[ "size" ]],
      row[[ "subjects" ]],
      row[[ "rows" ]],
      row[[ "time_type" ]],
      row[[ "operation" ]],
      as.numeric( row[[ "elapsed_sec" ]] ),
      format_peak( as.numeric( row[[ "peak_mib" ]] ) ),
      checks,
      effects
    )
  } )

  paste( c( header, body ), collapse = "\n" )
}

out_file = file.path( script_dir, "baseline-2.0.9.md" )
session = utils::capture.output( utils::sessionInfo() )

report = c(
  "# msmtools 2.0.9 Performance Baseline",
  "",
  "This is a local developer baseline, not a public performance guarantee.",
  "The script is excluded from package builds and must be run manually.",
  "",
  paste0( "- Generated: ", format( Sys.time(), "%Y-%m-%d %H:%M:%S %Z" ) ),
  paste0( "- R version: ", R.version.string ),
  paste0( "- Platform: ", R.version$platform ),
  paste0( "- data.table: ", as.character( utils::packageVersion( "data.table" ) ) ),
  paste0( "- Peak memory: ", if ( has_peakram ) "recorded with peakRAM" else "not recorded; install peakRAM for memory baselines" ),
  "",
  markdown_table( baseline ),
  "",
  "## Notes",
  "",
  "* `augment_adds_n_events` records the current by-reference addition of `n_events` when it is omitted.",
  "* `augment_key` and `polish_key` record whether the input object's key changes by reference.",
  "* `polish_names` records whether `polish()` restores temporary columns on the input object.",
  "",
  "## Session",
  "",
  "```",
  session,
  "```"
)

writeLines( report, out_file )
message( "Wrote ", out_file )
