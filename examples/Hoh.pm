#####################################################################################
#################                   Hoh.pm                           ################
#################             Krantz Lab       July 2008             ################
#################             Krantz Lab Rev.  March 2014            ################
#####################################################################################
#############   Module to manipulate datasets in hash of hashes format  #############
#####################################################################################
package Hoh;
################################# THE CONSTRUCTOR ###################################
sub new {
  my $proto                                 = shift;
  my $package                               = ref($proto) || $proto;
  my $self                                  = {};            #the anon hash
     $self->{HOH}                           = {}; #hash of hashes
     $self->{COLUMN_EQUATIONS}              = {}; #hoh of equations that perform column math
     $self->{STATISTICS}                    = {}; #Raw stats data from binning analysis
     $self->{STATISTICS_DATA}               = {}; #Stats data as hoh derived upon saving selected stats
     $self->{BIN_DATA}                      = {}; #hash of Bin Hohs
     $self->{PRINT_ORDER}                   = []; #order data will be printed to outfile
     $self->{STATISTICS_PRINT_ORDER}        = ['MEAN', 'STANDARD_DEVIATION'];
     $self->{STATISTICS_COLUMNS_TO_ANALYZE} = [];    #list of columns to average, etc.
     $self->{BINS}                          = [];    #bins locations $min and $max values
     $self->{COLUMN_NAMES}                  = [];    #names of the data columns 
     $self->{SORTED_KEYS}                   = [];    #row keys after sort for printing to outfile
     $self->{ORIGINAL_KEYS}                 = [];    #if generate keys mode then hoh keys are in original order
     $self->{PRINT_ORDER_FILE}              = undef; #file listing order the columns are printed to the outfile
     $self->{FILE}                          = undef; #Input filename and full path
     $self->{FILE_OUT}                      = undef; #output filename and full path
     $self->{FILE_TYPE}                     = undef; #Special designator to handle unique flatfile types
     $self->{FILE_EXTENSION}                = undef; #extension of input file (dot included)
     $self->{FILE_PREEXTENSION}             = undef; #Input filename pre-extension (no dot)
     $self->{FILE_OUT_EXTENSION}            = undef; #extension of output file (dot included)
     $self->{FILE_OUT_PREEXTENSION}         = undef; #Output filename pre-extension (no dot)
     $self->{DELIMITER}                     = "\t";  #Default tab record separator in flat file
     $self->{CASE_SENSITIVE}                = undef; #set to true then data are not forced to caps

     $self->{KEY_NAMES}                     = 'KEYS'; #for printing purposes header column name for row keys
     $self->{GENERATE_KEYS}                 = undef; #make row keys when opening file
     $self->{FILEKEYS}                      = undef; #if true then print row keys to file

     $self->{COLUMN_NUMBER}                 = undef; #Number of columns in hoh
     $self->{PROBABLE_COLUMN_NUMBER}        = undef; #Best guess at number of columns in file

     $self->{UNSORTED}                      = undef; #do not sort file output
     $self->{SORT_COLUMN}                   = undef; #column key to sort against
     $self->{SORT_ORDER}                    = undef; #Ascending = 0, descending = 1
     $self->{SORT_TYPE}                     = undef; #numerical (<=>) = 0, alphabetical (cmp) = 1

     $self->{BINNED_COLUMN}                 = undef; #column used for binning
     $self->{TOLERANCE}                     = undef; #Bin center plus/minus tolerance value
     $self->{BINS_PER_DECADE}               = undef; #Log bin averaging mode
     $self->{BIN_NUMBER}                    = undef; #Number of bins used in linear averaging or tolerance mode

     $self->{HEADER_OFF}                    = undef; #if true then do not print header
     $self->{HEADER}                        = undef; #New header
     $self->{ORIGINAL_HEADER}               = undef; #prior header
     $self->{ORIGINAL_HEADER_LENGTH}        = undef; #length in lines
     $self->{FIRST_LINE}                    = undef; #first line of original file used to test file type
  return bless($self, $package);                     #return thy self
}
############################### METHOD SUBS ########################################

#Add a new column and set it to a scalar value
#arg1 = col; arg2 = scalar
sub add_scalar_column { 
  my ($self, $col, $scalar) = @_;
  my $hoh = +{ hoh($self) };
  $col = clean_column_names($col);
  $hoh -> {$_} -> {$col} = $scalar for @{[keys(%$hoh)]};
  hoh($self, %$hoh);
  extract_column_names($self);
  return $self
}

#Adds one or more empty columns but if column exists it does not overwrite or blank or delete
sub add_columns { 
  my $self = shift; 
  my @cols = @_;
  my $hoh = +{ hoh($self) };
  foreach my $col (@cols) { 
   $col = clean_column_names($col);
   foreach my $row (@{[keys(%$hoh)]}) { $hoh -> {$row} -> {$col} = undef unless defined($hoh -> {$row} -> {$col}) } 
  };
  hoh($self, %$hoh);
  extract_column_names($self);
  return $self
}

#works in scalar or array mode
sub clean_column_names {
 my @dirties = @_;
 my ($cleans, $realcleans, $reallycleans) = ([],[],[]);
 #replace all non-word chars not in set {[A-Z][a-z][0-9] and '_'} with nothing
 foreach my $dirty (@dirties) { $dirty =~ s/\W+//; push @$cleans, $dirty };
 #if key begins with a number, underscore or is undef, then add a letter 'A' in front
 foreach my $clean (@$cleans) { 
   if    ($clean =~ /^\d/) { $clean = 'A'.$clean; print "CASE A\n"; } 
   elsif ($clean =~ /^\_/) { $clean = 'A'.$clean; print "CASE B\n"; }
   elsif ($clean eq '') { $clean = 'A'.$clean; print "CASE C\n"; }; 
   push @$realcleans, $clean 
 };
 $reallycleans = [resolve_column_name_collisions(@$realcleans)];
 if (scalar(@$reallycleans) == 0) 
  { die "Column name is undefined. Please only use characters in the set {[A-Z][a-z][0-9] and '_'} in column names, aborting at $! $0" }
 elsif (scalar(@$reallycleans) == 1) { return $reallycleans->[0] }
 else { return @$reallycleans };
}

sub resolve_column_name_collisions {
 my @array = @_;
 my ($nonuniques, $uniques, $true) = ([],[],1);
 @_ = sort { push @$nonuniques, $a if $a eq $b; $a cmp $b } @array;
 foreach my $aa (@array) {
  my $bool = 0;
  foreach my $nu (@$nonuniques) { $bool = 1 if ($aa eq $nu) };
  $aa .= '1' if $bool;
  push @$uniques, $aa;
 };
 while ($true) {
  my $item = undef;
  @_ = sort { $item = $a if $a eq $b; $a cmp $b } @$uniques;
  if (defined $item) {
   my ($retry, $notfound) = ([], 1);
   foreach my $unique (reverse(@$uniques)) { if (($item eq $unique) && $notfound) { unshift @$retry, ++$unique; $notfound = 0 } else { unshift @$retry, $unique } };
   @$uniques = @$retry;
  }
  else { $true = 0 };
 };
 return @$uniques
}

#check whether cleaned column keys caused loss of a key 
sub column_name_key_check {
 my ($raw_keys, $keys) = @_;
 my ($length_bool, $undef_bool, $unique_bool) = (1,1,1);
 $length_bool = 0 unless (scalar(@$raw_keys) == scalar(@$keys)); #good column keys have same number of elements
 foreach my $key (@$keys) { if ($key eq '') { $undef_bool *= 0; last } else { $undef_bool *= 1 } };
 my @junk = sort { $unique_bool *= 0 if $a eq $b; $a cmp $b } @$clean_keys; #if col key not unique then bool = 0
 return ($length_bool * $undef_bool * $unique_bool)
}

#Simple column math routine for Hoh using native Perl interpreter
#First ARG is where the result goes; second is the column math
#column math format example: 'col(A) + col(B) * col(C) - sin(col(E))'
#spaces are not critical; functions defined in Perl or this module are possible
sub column_math {
 my ($self, $resulting_column, $math) = @_;
 my ($hoh,$eqs)   = (+{hoh($self)},+{column_equations($self)});
    $math =~ s/\s+//g;
 my @columns_used = $math =~ /col\(\w+\)/g;
 foreach my $col_match (@columns_used) {
   my ($var) = $col_match =~ /col\((\w+)\)/; 
   my $hohvar = '$hoh->{$row}->{'.qw(')."$var".qw(').'}';
   $math =~ s/col\($var\)/$hohvar/g;
 };
 foreach my $row (keys(%$hoh)) {
    $hoh->{$row}->{$resulting_column} = eval($math);
    warn "Error at column_math engine: $@" if $@;
 };
 $eqs -> {EQUATION} -> {$resulting_column} = $math; #store new column math equation in Hoh
 $eqs -> {ORDER}    -> {$resulting_column} = scalar(keys(%{  $eqs -> {EQUATION} })) + 1;
 column_equations($self, %$eqs);
 hoh($self, %$hoh);
 return $self
}

#given a column name returns the list of values from the hoh according to the last sort using the method sort_hoh
sub column {
 my ($self, $column_name) = @_;
 my $hoh = +{hoh($self)};
 my @values;
 foreach my $key (sorted_keys($self)) {
  push @values, $hoh->{$key}->{$column_name};
 };
 return @values
}

sub extract_column_names {
 my $self = shift;
 my ($hoh, $col_names) = (+{ hoh($self) }, {});
 foreach my $row_key (keys(%$hoh)) { $col_names -> {$_} = undef for keys(%{ $hoh -> {$row_key} }) };
 column_names($self, sort keys(%$col_names));
 return $self
}

sub save {
 my ($self, $file, $print_order, $sort_column, $header_off, $d, $fileKeys) = @_;
 $file = file_out($self) unless $file;
 $print_order = [ print_order($self) ] unless scalar(@$print_order);
 #add line here; if print order is missing then use column names after extracting them from the hoh
 print "No print order was specified in save routine, using column_names property instead" unless scalar(@$print_order);
 extract_column_names($self) unless scalar(@$print_order);
 $print_order = [ print_order($self, column_names($self)) ] unless scalar(@$print_order);
 die "Nothing to save. Hoh is empty, aborting $! $0" unless scalar(@$print_order);
 $sort_column = sort_column($self) unless $sort_column;
 $header_off = header_off($self) unless $header_off;
 $d = delimiter($self) unless $d; $d = "\t" unless $d; delimiter($self, $d);
 $fileKeys = filekeys($self) unless $fileKeys;
 my ($hohref, $cr, $line, $print, $keys, $filekeyheader)  = (+{ hoh($self) }, chr(13).chr(10), undef, [], [], undef);
#Header preparation
 $filekeyheader = key_names($self).$d if $fileKeys; #header as delimited list of keys in print order 
 $line = $filekeyheader.join($d, @$print_order).$cr unless $header_off; #header_off: boolean determines if header printed
 open(FH,">$file") || die "$0 can't open $file using save_hoh method. $!";
 print FH $line;
 #Sorting routine is called here depending on the settings
  if (unsorted($self)) { $keys = [ keys %$hohref ] }
  else { sort_hoh($self, $sort_column); $keys = [ sorted_keys($self) ]; $keys = [ keys %$hohref ] unless scalar(@$keys) };
#Data are printed to file according to row sort and column sort
 foreach my $row (@$keys) {
  $line = undef unless $fileKeys;
  $line = $row.$d if $fileKeys;
  $line .= $hohref->{$row}->{$_}.$d for @$print_order; #print the cols
  $line =~ s/$d$/$cr/;
  print FH $line;
 };
 close(FH);

#Argument parameters are set to module properties
 file_out($self, $file);
 print_order($self, @$print_order);
 sort_column($self, $sort_column);
 header_off($self, $header_off);
 delimiter($self, $d);
 filekeys($self, $fileKeys);
 return $self;
};

#script to preprocess file detecting and setting file_type property by header and extension
sub preprocess_file {
 my ($self, $file, $delimiter) = @_;
 $file = file($self) unless $file;
 $delimiter = delimiter($self) unless $delimiter;
 filetest($self);
 file_extension($self, file_ext($file));
 #determine file_preextension and set property
 my ($cr, $ext, $preext) = (chr(13).chr(10), file_extension($self), file($self)); 
 $preext =~ s/$ext$//;
 file_preextension($self,$preext); 
 my ($num_cols, $prob_num_cols, $first_line) = (column_number($self), probable_column_number($self), first_line($self));
 print "The probable number of columns is $prob_num_cols ... \n"; 
 unless ($num_cols) { $num_cols = $prob_num_cols; column_number($self,$num_cols) };
 # Test for various file types; e.g. test for ATF header and call ATF file to open properly
 print "File extension is ".file_extension($self)." and first line is $first_line ... \n";
 if (lc((file_extension($self)) eq '.atf') && ($first_line eq "ATF	1.0")) {
  # get column names for special case of T, I, V ATF file
  # last line of the header should have this line
  extract_atf_header($self);
  $num_cols = column_number($self);
  my $atf_header = [ split(/$cr/,original_header($self)) ];
  my $atf_header_col_line = $atf_header->[-1];
  my $atf_col_names = [ split(/\t/,$atf_header_col_line) ]; #Note atf file is tab-delimited and split on tab
  if (($num_cols==3) && (($atf_col_names->[0]) =~ /TIME/i) && (($atf_col_names->[1]) =~ /pA/i) && (($atf_col_names->[2]) =~ /mV/i)) 
   {
    column_names($self, 'T','I','V');
    generate_keys($self, 1);
    #override the delimiter since ATF files are tab-delimited
    if (delimiter($self) ne "\t") { $delimiter = "\t"; delimiter($self,"\t"); print "Delimiter is not correct. Resetting to tab.\n"; return 0 }; 
    file_type($self, 'ATF_TIVCOLUMNS');
   }
  # else use ATF column names as is
  else
   {
    column_names($self, @$atf_col_names);
    generate_keys($self, 1);
    #override the delimiter since ATF files are tab-delimited
    if (delimiter($self) ne "\t") { $delimiter = "\t"; delimiter($self,"\t"); print "Delimiter is not correct. Resetting to tab.\n"; return 0 }; 
    file_type($self, 'ATF_COLUMNS');
   };
 }
 else 
 {
  if (test_header_for_columns($first_line, $delimiter)) { 
   #test whether there is a proper header in the file for the column keys
   #if after split into array of first line all elements are alphabetic, 
   #then assume items are part of proper header
   column_names($self, line_to_array($first_line, $delimiter)); #get column names and set those
   header($self, $first_line); #grab header and set its property
   file_type($self, 'TEXT_USERCOLUMNS'); 
  }
 #if not then make the header either from column_names or with alphabet incremented keys and paste header in file
  else { 
  generate_header($self, $num_cols);
  file_type($self, 'TEXT_AUTOCOLUMNS');  
 };
};
 print "The file type is ".file_type($self)." ...\n";
 return $self
}

sub extract_atf_header {
 my ($self, $file, $line, $cr, $header) = (shift(), shift(), undef, chr(13).chr(10), undef);
  $file = file($self) unless $file;
  open (FILE, "<$file") or die $!;
  $line = <FILE>;
  $line =~ s/\s+$//;
  if ($line eq "ATF\t1.0") 
  {
    $header .= $line.$cr;
    my $header_parm = <FILE>;
    $header_parm =~ s/\s+$//;
    $header .= $header_parm.$cr;
    my ($header_lines, $columns) =  split(/\s+/, $header_parm);
    column_number($self,$columns);
    $header_lines++;
    for (my $ii = 1; $ii <= $header_lines; $ii++) 
    {
      my $header_line = <FILE>;
      $header_line =~ s/\s+$//;
      $header .= $header_line.$cr;
    };
  };
 close(FILE);
 $header =~ s/$cr$//;
 original_header($self, $header);
 original_header_length($self, scalar(split(/$cr/,$header)));
 return $self;
}

sub test_header_for_columns {
 my ($h, $d, $bool) = (shift(), shift(), 1);
 $bool *= ($_ =~ /^[a-z]/i) for (split(/$d/,$h));
 return $bool
}

sub generate_header {
 my ($self, $num_cols, $delimiter) = @_;
 my ($header, $h) = (undef, 'A');
 $num_cols = column_number($self) unless $num_cols;
 $delimiter = delimiter($self) unless $delimiter;
 for (my $i = 0; $i<$num_cols; $i++) { $header .= $h++.$delimiter };
 $header =~ s/$delimiter$//; #replace last delimiter with nothing
 column_number($self, $num_cols);
 delimiter($self, $delimiter);
 header($self, $header);
 column_names($self, @{ [ line_to_array($header, $delimiter) ] });
 return $self
}

sub generate_header_from_column_names {
 my ($self, $col_names, $delimiter) = @_;
 $col_names = [ column_names($self) ] unless $col_names;
 $delimiter = delimiter($self) unless $delimiter;
 my $header = undef;
 $header .= $_.$delimiter for @$col_names;
 $header =~ s/$delimiter$//; #replace last delimiter with nothing
 delimiter($self, $delimiter);
 column_names($self, @$column_names);
 column_number($self,scalar(@$column_names));
 header($self, $header);
 return $self
}

sub load { my $self = shift; open_hoh($self, @_); return $self}

sub open_hoh { 
 my ($self, $file, $delimiter, $cs, $gen_keys) = @_; 
 print "Opening $file ... \n";
 $delimiter = delimiter($self) unless $delimiter;
 $file      = file($self)      unless $file;
 $gen_keys  = generate_keys($self) unless $gen_keys;
 $cs = case_sensitive($self) unless $cs; 
 delimiter($self, $delimiter);
 file($self, $file);
 generate_keys($self, $gen_keys);
 case_sensitive($self, $cs);
 preprocess_file($self, $file, $delimiter) unless preprocess_file($self, $file, $delimiter);
 file_to_hoh($self, $file, $delimiter, $cs, $gen_keys, file_type($self));
 return $self 
}

sub file_to_hoh {
 my ($self, $file, $delimiter, $case_sensitive, $gen_keys, $file_type) = @_; #Note that the files are in a key-tab-value format
 my ($tab, $hohref, $key_index, $first_line)    = (chr(9), {}, 0, undef);
 $file = file($self) unless $file;
 $delimiter = delimiter($self) unless $delimiter;
 $case_sensitive = case_sensitive($self) unless $case_sensitive;
 $gen_keys = generate_keys($self) unless $gen_keys;
 $file_type = file_type($self) unless $file_type;
 open(FH, "<$file") || die "$0 can't open $file using file_to_hoh method. $!";
 my @keys = ();
 if ($file_type eq 'TEXT_USERCOLUMNS')
   {
    $first_line = <FH>;
    $first_line = cs($self, $first_line, $case_sensitive);
    $first_line =~ s/\s+$//;
    my @raw_keys = line_to_array($first_line, $delimiter);
    @keys = clean_column_names(@raw_keys);
    unless (column_name_key_check(\@raw_keys, \@keys)) { 
      generate_header($self,column_number($self),delimiter($self)); #header problem found in column name key check resorting to autogenerated columns
      @keys = column_names($self);
      print "Header problem detected in ".file($self)."\n"
    };    
    key_names($self, shift(@keys)) unless $gen_keys;
    key_names($self, 'KEYS') if $gen_keys;
   }
 elsif ($file_type eq 'TEXT_AUTOCOLUMNS')
   {
    #case where the column names are not there or not all alphabetic
    @keys = column_names($self); #Keys come from autogenerated column names
    key_names($self, shift(@keys)) unless $gen_keys;
    key_names($self, 'KEYS') if $gen_keys;
   }
 elsif ( ($file_type eq 'ATF_TIVCOLUMNS') || ($file_type eq 'ATF_COLUMNS') )
   {
    my @raw_keys = column_names($self);  #define keys as column_names
    @keys = clean_column_names(@raw_keys);
    unless (column_name_key_check(\@raw_keys, \@keys)) { 
      generate_header($self,column_number($self),delimiter($self)); #header problem found in column name key check resorting to autogenerated columns
      @keys = column_names($self);
    }; 
    generate_header_from_column_names($self);
    $gen_keys = 1;  #gen_keys should be forced to true for ATF files because there are no row keys
    key_names($self, 'KEYS');
    my $trash = <FH> for (1..(original_header_length($self)));        #Need to remove ATF header
   };
 my @original_keys = ();
 while (my $line = <FH>) {
  $line =~ s/\s+$//;
  $line = cs($self, $line, $case_sensitive);
  my @data = line_to_array($line, $delimiter);
  my $key = undef;
     $key = shift(@data) unless $gen_keys;
     $key = key_name_generator($file, $key_index++) if $gen_keys;
     push @original_keys, $key;
  my $lng = $#data;
  for my $j (0..$lng) { $hohref -> {$key} -> {$keys[$j]} = $data[$j] };
 };
 close FH;
 original_keys($self, @original_keys);
 generate_keys($self, $gen_keys);
 case_sensitive($self, $case_sensitive);
 delimiter($self, $delimiter);
 file($self, $file);
 hoh($self, %$hohref);
 return %$hohref;
}

sub key_name_generator {
 my ($prefix, $key_index) = (shift(), shift());
 return $prefix.$key_index;
}

sub open_print_order_file {
 my ($self, $file) = @_;
 print_order_file($self, $file);
 my @a = file_to_array($file);
 print_order($self, @a);
 return $self;
}

sub generate_generic_file_out {
 my $self       = shift;
 my $fileout    = file_out($self, $fileout);
 $fileout       = file_out($self, 'outfile.txt') unless $fileout;
 return $self
}

sub generate_generic_print_order {
 my $self = shift;
 my %hoh = hoh($self);
 my @print_order = sort { $a cmp $b } keys %{ $hoh{ [ keys %hoh ] -> [0]} };
 print_order($self, @print_order);
 return $self
}

sub sort_hoh {
 my ($self, $c, $order, $type) = @_;
 $c = sort_column($self) unless $c; #first resort
 $c = [ print_order($self)  ] -> [0] unless $c; #second resort 
 $c = [ column_names($self) ] -> [0] unless $c; #last resort
 $order = sort_order($self)  unless $order; #boolean see below
 $type  = sort_type($self)   unless $type; #boolean see below
 my $h  = +{ hoh($self) };
 my @sorted_keys = ();
 unless ($order) { 
  #Ascending numeric sort if $order = 0 and $type = 0 (DEFAULT)
  @sorted_keys = sort { $h->{$a}->{$c} <=> $h->{$b}->{$c} } keys %$h unless $type;
  #Ascending lexicographic sort if $order = 0 and $type = 1
  @sorted_keys = sort { $h->{$a}->{$c} cmp $h->{$b}->{$c} } keys %$h if $type;
 } 
 else { 
  #Descending numeric sort if $order = 1 and $type = 0
  @sorted_keys = sort { $h->{$b}->{$c} <=> $h->{$a}->{$c} } keys %$h unless $type;
  #Descending lexicographic sort if $order = 1 and $type = 1
  @sorted_keys = sort { $h->{$b}->{$c} cmp $h->{$a}->{$c} } keys %$h if $type;
 }; 
 sort_column($self, $c);
 sort_order($self, $order);
 sort_type($self, $type);
 sorted_keys($self, @sorted_keys);
 return $self
}

#Plot xy-data series or multiple series with multiple styles
#Save as particular graphic style
#parameters are hash
# Graphics::Color::RGB->new(red => .95, green => .94, blue => .92, alpha => 1)
# DATASETS => hash_ref of hash format X => Hoh Col Name, Y => Hoh Col Name, COLOR of set 
#             (black, red, green, blue, cyan, magenta, yellow), STYLE => Point, Line, Step, Bar
# X_LABEL => x-axis label;  Y_LABEL => y-axis label;
# FORMAT => 'pdf', 'png', 'svg'; 
# FILE => output path/file name
# TITLE => title of graph
# AUTOSCALE => Boolean (default is OFF) finds max and min of all datasets and then sets the axes accordingly
sub create_xy_plot {
 use Chart::Clicker;
 use Chart::Clicker::Context;
 use Chart::Clicker::Axis;
 use Chart::Clicker::Data::Series;
 use Chart::Clicker::Data::DataSet;
 use Chart::Clicker::Data::Marker;
 use Chart::Clicker::Renderer::Point;
 use Chart::Clicker::Renderer::Line;
 use Chart::Clicker::Renderer::Bar;
 use Chart::Clicker::Renderer::Step;
 use Graphics::Color::RGB;
 use Data::Dumper; 
 my $self = shift();
 my %parm = @_;
 my ($colors, $all_x_data, $all_y_data, $context_i, $hoh) = ([], [], [], 1, +{ hoh($self) });
 my $cc        = Chart::Clicker -> new(width => 500, padding => 10, format => $parm{'FORMAT'});
 $cc -> background_color( Graphics::Color::RGB->new(red => 1, green => 1, blue => 1) );
 $cc -> title -> text($parm{'TITLE'}) if exists($parm{'TITLE'});
 my $context   = $cc->get_context('default');
 $context -> domain_axis -> label($parm{'X_LABEL'}) if exists($parm{'X_LABEL'}); 
 $context -> range_axis  -> label($parm{'Y_LABEL'}) if exists($parm{'Y_LABEL'});
 my $datasets_hash = $parm{'DATASETS'};

 #Autoscaling of ticks and axes handled here
 if ($parm{'AUTOSCALE'}) {
  foreach my $key (keys(%$datasets_hash)) {
   my ($x_col, $y_col) =  ($datasets_hash->{$key}->{'X'}, $datasets_hash->{$key}->{'Y'}); #extract $x_col and $y_col from Hoh to data arrays
   sort_hoh($self, $x_col, 0 , 0); #sort according to x column first
   my ($x, $y) = ([column($self, $x_col)],[column($self, $y_col)]); #extract columns
   push @$all_x_data, @$x; #required to do semi-intelligent autoscaling
   push @$all_y_data, @$y; #required to do semi-intelligent autoscaling
  };
   my ($x_ticks, $y_ticks) = ( find_autoscaled_ticks($self, $all_x_data, 6), find_autoscaled_ticks($self, $all_y_data, 6) );
      $context->domain_axis->tick_values($x_ticks);
      $context->range_axis->tick_values($y_ticks);
      $context->domain_axis->tick_labels($x_ticks);
      $context->range_axis->tick_labels($y_ticks);
 print 'Autoscaling Chart::Clicker graph: x_ticks '.join(' ',@$x_ticks).' and y_ticks '.join(' ',@$y_ticks)."\n";
   my $x_tick_ends = [ $x_ticks->[0], $x_ticks->[-1] ];
   my $y_tick_ends = [ $y_ticks->[0], $y_ticks->[-1] ];
   #Due to quirks of Chart::Clicker need to add fake transparent dataset of ticks
   my $ticks_series  =  Chart::Clicker::Data::Series  -> new( 'keys' => $x_tick_ends, 'values' => $y_tick_ends, 'name' => '' );
   my $ticks_dataset =  Chart::Clicker::Data::DataSet -> new( series => [ $ticks_series ] );
      $ticks_dataset -> context('default');
      $cc            -> add_to_datasets($ticks_dataset);
   #some transparent color for the fake ticks dataset acting here as a means to make the ticks pretty
   push @$colors, Graphics::Color::RGB->new(red => 0, green => 0, blue => 0, alpha => 0); 
 };

 foreach my $key (keys(%$datasets_hash)) {
  push @$colors, $datasets_hash->{$key}->{'COLOR'};
  my ($x_col, $y_col) =  ($datasets_hash->{$key}->{'X'}, $datasets_hash->{$key}->{'Y'}); #extract $x_col and $y_col from Hoh to data arrays
  sort_hoh($self, $x_col, 0 , 0); #sort according to x column first
  my ($x, $y) = ([column($self, $x_col)],[column($self, $y_col)]); #extract columns
  my $series  =  Chart::Clicker::Data::Series  -> new( 'keys' => $x, 'values' => $y, 'name' => $y_col.' dataset' );
  my $dataset =  Chart::Clicker::Data::DataSet -> new( series => [ $series ] );
  my $new_context =  Chart::Clicker::Context->new(name => 'default'.$context_i);
     $dataset        -> context('default'.$context_i++);
     $cc             -> add_to_contexts($new_context);
     $cc             -> add_to_datasets($dataset);
 };
 $cc->color_allocator->colors($colors);

 #Set the renderer foreach series
 $context_i = 1;
 foreach my $key (keys(%$datasets_hash)) {
   my $style = $datasets_hash->{$key}->{'STYLE'};
   my $shape = $datasets_hash->{$key}->{'SHAPE'};
   my $this_context = $cc -> get_context('default'.$context_i);
      $this_context       -> domain_axis($context->domain_axis);
      $this_context       -> range_axis($context->range_axis);
      if (uc($style) eq 'LINE')            { $this_context -> renderer(Chart::Clicker::Renderer::Line  -> new()) }
   elsif (uc($style) eq 'STEP')            { $this_context -> renderer(Chart::Clicker::Renderer::Step  -> new( 'shape' => $shape )) }
   elsif (uc($style) eq 'HORIZONTAL_STEP') { $this_context -> renderer(Chart::Clicker::Renderer::Step  -> new( 'type' => 'horizontal', 'shape' => $shape )) }
   elsif (uc($style) eq 'VERTICAL_STEP')   { $this_context -> renderer(Chart::Clicker::Renderer::Step  -> new( 'type' => 'vertical', 'shape' => $shape )) }
   elsif (uc($style) eq 'POINT')           { $this_context -> renderer(Chart::Clicker::Renderer::Point -> new()) }
   elsif (uc($style) eq 'BAR')             { $this_context -> renderer(Chart::Clicker::Renderer::Bar   -> new()) };
   $cc -> set_context('default'.$context_i++, $this_context);
 };
 #dummy plot in $context is alpha = 0 (transparent) with points of radius 1 and is used to force axes scale to desired tick intervals
 $context -> renderer(Chart::Clicker::Renderer::Point -> new({ 'shape' => Geometry::Primitive::Circle->new( radius => 1 ) })); 
 $cc -> set_context('default', $context);
 $cc -> draw;
 $cc -> write($parm{'FILE'});
 return $self
}

#given a range of values return a list of ticks that covers the range
#tick increment and beginning and end are determined so that values are whole numbers if possible
#if range is less than 1 unit then one significant digit precision is used for tick interval
#The default multiples for the tick intervals are given in the $ints and $decs lists in the routine
#$vector is any length array_ref that spans that data limits in a graph
#$target_steps is the limit +/-1 (as its behavior is not perfect)
sub find_autoscaled_ticks {
 use POSIX;
 my ($self, $vector, $target_steps) = @_;
     $target_steps = 4 unless $target_steps; 
 my ($ticks, $multiple) = ([], 1);
 #The following are potential tick multiples for nice wholish numbers to look at
 my $ints  = [5,2,1]; #desired intervals when greater than 0 log in range
 my $decs  = [0.5,0.2,0.1]; #desired decimal intervals when less than 1 log in range
 my ($max, $min) = max_min_vector($vector); 
 my $range = $max - $min;
 my ($pad_min, $pad_max) = ($min - 0.05*$range, $max + 0.05*$range); #add 10% to either end of range as pad
 my $pad_range = $pad_max - $pad_min;
 my $log   = log($pad_range/$target_steps)/log(10); #factor of ten scale the data is plotted at
 #determine increments 
 my ($best_score, $best_multiple, $best_int, $best_factor_10) = ($pad_range, 1, undef, undef);
 if ($log >= 0) { #I am unsure about this if statement, and it may not be needed now; but it works for now
  #best multiple of $ints to span range in $target_steps number of steps
   TRY: while ($multiple <= 10) {
     foreach my $int (@$ints) {
       my $factor_10 = 10**int($log); 
       my $score = $factor_10 * $multiple * $int * $target_steps - $pad_range; 
       if (($score < $best_score) && ($score > 0)) 
               { $best_factor_10 = $factor_10; $best_int = $int; $best_score = $score; $best_multiple = $multiple; last TRY };
     };
     $multiple++
   };
 }
 else {
    TRY: while ($multiple <= 10) {
     foreach my $dec (@$decs) {
       my $factor_10 = 10**int($log);
       my $score = $factor_10 * $multiple * $dec * $target_steps - $pad_range; 
       if (($score < $best_score) && ($score > 0)) 
               { $best_factor_10 = $factor_10; $best_int = $dec; $best_score = $score; $best_multiple = $multiple; last TRY };
     };
     $multiple++
    };
  };
 my $tick_inc = $best_int * $best_multiple * $best_factor_10;
 #find the $begin tick value, making it an multiple of the $tick_inc 
 my $tick = 0;
    $tick = $pad_min - fmod($pad_min, $tick_inc) if $pad_min >= 0; #fmod is from POSIX module
    $tick = $pad_min - fmod($pad_min, $tick_inc) -  $tick_inc if $pad_min < 0; #fmod is from POSIX module 
 while ($tick < ($pad_max + $tick_inc)) { push @$ticks, $tick; $tick += $tick_inc };
 $ticks = [0,2,4,6,8,10] unless scalar(@$ticks); #defaults to this if data is empty 
 return $ticks
}

#typical log binning routine
sub statistics_log_bin {
 my ($self, $bin_col, $bpd, $cols_to_avg) = @_;
 my $hohref = +{ hoh($self) }; #dataset
 $bpd  = bins_per_decade($self) unless $bpd;
 $bin_col = binned_column($self) unless $bin_col;
 bins_per_decade($self, $bpd);
 binned_column($self, $bin_col);
 $cols_to_avg = statistics_columns_to_analyze($self) unless scalar(@$cols_to_avg);
 $cols_to_avg = [ keys %{ $hohref -> { [ keys %$hohref ] -> [0] } } ] unless scalar(@$cols_to_avg);
 statistics_columns_to_analyze($self, @$cols_to_avg);
 log_bins($self, hoh_list_slice($hohref, $bin_col), $bpd);
 bin_search($self, [ bins($self) ], $bin_col);
 compute_statistics_on_bins($self);
 return $self
}

#linear binning where tolerance $tol is linear step between bins
sub statistics_linear_bin {
 my ($self, $bin_col, $tol, $cols_to_avg) = @_;
 my $hohref = +{ hoh($self) }; #dataset
 $bin_col = binned_column($self) unless $bin_col;
 $tol  = tolerance($self) unless $tol;
 $cols_to_avg = statistics_columns_to_analyze($self) unless scalar(@$cols_to_avg);
 $cols_to_avg = [ keys %{ $hohref -> { [ keys %$hohref ] -> [0] } } ] unless scalar(@$cols_to_avg);
 statistics_columns_to_analyze($self, @$cols_to_avg);
 linear_bins($self, hoh_list_slice($hohref, $bin_col), $tol);
 bin_search($self, [ bins($self) ], $bin_col);
 compute_statistics_on_bins($self);
 return $self
}

 #reduce dataset by binning and averaging data from column $col 
 #within each bin set by the array ref, $bin, +/- the scalar tolerance $tol
sub statistics_bin_tolerance {
 my ($self, $bin_col, $bins, $tol, $cols_to_avg) = @_;
 my $hohref = +{ hoh($self) }; #dataset
 $bin_col = binned_column($self) unless $bin_col;
 $bins = [ bins($self) ]  unless scalar(@$bins);
 $tol  = tolerance($self) unless $tol;
 $cols_to_avg = statistics_columns_to_analyze($self) unless scalar(@$cols_to_avg);
 $cols_to_avg = [ keys %{ $hohref -> { [ keys %$hohref ] -> [0] } } ] unless scalar(@$cols_to_avg);
 statistics_columns_to_analyze($self, @$cols_to_avg);
 tolerance_bins($self, $bins, $tol);
 bin_search($self, [ bins($self) ], $bin_col);
 compute_statistics_on_bins($self); 
 return $self;
}

sub bin_search {
 my ($self, $bins, $bin_col) = @_; 
 my ($hohref, $bin_hash, $bin_cnt) = (+{ hoh($self) }, {}, 0);
 foreach my $key ( @{ [ keys %$hohref ] } ) {    
  my $bin_val = $hohref -> {$key} -> {$bin_col};
  $bin_cnt = 0;
  foreach my $bin (@$bins) {  
    if (($bin->[0] <= $bin_val) && ($bin->[1] >= $bin_val)) { 
     #then key belongs to the bin put the keys on the chain for that bin
             $bin_hash -> {'BIN'.$bin_cnt} -> {BIN_COLUMN}             = $bin_col;
             $bin_hash -> {'BIN'.$bin_cnt} -> {BIN_COLUMNS_TO_AVERAGE} = $cols_to_avg;
             $bin_hash -> {'BIN'.$bin_cnt} -> {BIN_LOWER_VALUE}        = $bin->[0];
             $bin_hash -> {'BIN'.$bin_cnt} -> {BIN_UPPER_VALUE}        = $bin->[1];
             $bin_hash -> {'BIN'.$bin_cnt} -> {BINS_PER_DECADE}        = $bpd;
     push @{ $bin_hash -> {'BIN'.$bin_cnt} -> {KEYCHAIN} }, $key;
     last;
    };
    $bin_cnt++;
  };
 };
 statistics($self,%$bin_hash);
 return $self
}

sub compute_statistics_on_bins { 
 my ($self, $bin_hash) = @_;
 my $cols_to_avg = [ statistics_columns_to_analyze($self) ];
 $bin_hash  = +{ statistics($self,%$bin_hash) } unless $bin_hash;
 my $hohref = +{ hoh($self) };
 foreach my $bin (@{ [ keys %$bin_hash ] }) { $bin_hash -> {$bin} -> {COUNT}  = scalar(@{ $bin_hash -> {$bin} -> {KEYCHAIN} }); };
 #compute sum, mean, sum of squares, mean sum of squares
 foreach my $bin (@{ [ keys %$bin_hash ] }) {
   foreach my $key (@{ $bin_hash -> {$bin} -> {KEYCHAIN} }) {
     $cols_to_avg = $bin_hash -> {'BIN'.$bin} -> {BIN_COLUMNS_TO_AVERAGE} unless $cols_to_avg;
     foreach my $col (@$cols_to_avg) {
       my $value = $hohref -> {$key} -> {$col}; 
       if ((defined $value) && (length $value > 0) && ($value ne "--") && ($col ne '')) {     
         $bin_hash -> {$bin} -> {DATA} -> {$col} -> {COUNT}          += 1;
         $bin_hash -> {$bin} -> {DATA} -> {$col} -> {SUM}            += $value;
         $bin_hash -> {$bin} -> {DATA} -> {$col} -> {SUM_OF_SQUARES} += $value**2;
       };
     };
   };
   #compute mean, mean of sum of squares, and standard deviation
   $cols_to_avg = $bin_hash -> {'BIN'.$bin} -> {BIN_COLUMNS_TO_AVERAGE} unless $cols_to_avg;
   foreach my $col (@$cols_to_avg) {
     if ($col ne '') {
      my $href = $bin_hash -> {$bin} -> {DATA} -> {$col};
      $href -> {MEAN} = ($href -> {SUM}) / ($href -> {COUNT}) if ($href -> {COUNT});
      $href -> {MEAN_SUM_OF_SQUARES} = ($href -> {SUM_OF_SQUARES}) / ($href -> {COUNT}) if ($href -> {COUNT});
      $href -> {VARIANCE} = ($href -> {MEAN_SUM_OF_SQUARES}) - ($href -> {MEAN})**2;
      $href -> {STANDARD_DEVIATION} = sqrt($href -> {VARIANCE});
     };
   };
 };
 statistics($self,%$bin_hash); 
 return $self
}

sub generate_statistics_file_out_name {
 my ($self, $fo) = (shift(), $fo);
 unless(file_out($self)) {
  my $fo_ext = file_out_extension($self);
  $fo_ext = '.txt' unless $fo_ext;
  $fo = file_preextension($self).'-stats_out'.$fo_ext;
 }
 else { $fo = file_out($self) };
 return file_out($self, $fo)
}

sub save_statistics {
 my ($self, $fileout, $print_order, $stats_print_order, $delimiter) = @_; 
 $delimiter = delimiter($self) unless $delimiter;
 $fileout = file_out($self) unless $fileout;
 $fileout = generate_statistics_file_out_name($self) unless $fileout;
 $print_order = [ print_order($self) ] unless scalar(@$print_order);
 $print_order = [ column_names($self) ] unless scalar(@$print_order); 
 $stats_print_order = [ statistics_print_order($self) ] unless scalar(@$stats_print_order);
 my ($header, $cr, $colname) = (undef, chr(13).chr(10), undef); #compose header
 my $stats_hoh = {}; #keep a record of the flatfile of the stats in memory
 my %stats_hoh_col_keys = ();
 foreach my $col (@$print_order) {
   foreach my $stat (@$stats_print_order) {
        if ($stat eq 'STANDARD_DEVIATION') { $colname = $col.'ERR' } 
     elsif ($stat eq 'MEAN')               { $colname = $col       }
     elsif ($stat eq 'VARIANCE')           { $colname = $col.'VAR' }
     elsif ($stat eq 'COUNT')              { $colname = $col.'CNT' };
     $stats_hoh_col_keys{$col}{$stat} = $colname;
     $header .= $colname.$delimiter;
   };
 };
 $header =~ s/$delimiter$/$cr/;
 open(FH,">$fileout") || die "$0 can't open $file using save_statistics method. $!";
 print FH $header; #write header
 my $hoh_stats = +{ statistics($self) };
 #sort by the bin index number
 my @sorted_stat_keys = sort { substr($a,3) <=> substr($b,3) } keys(%$hoh_stats); 
 foreach my $bin (@sorted_stat_keys) {
   my ($data, $line) = ($hoh_stats -> {$bin} -> {'DATA'}, undef);
   foreach my $col (@$print_order) { 
     foreach (@$stats_print_order) {
       $line .= $data -> {$col} -> {$_}.$delimiter;
       $stats_hoh -> {$bin} -> {($stats_hoh_col_keys{$col}{$_})} = $data -> {$col} -> {$_};
     };
   };
   $line =~ s/$delimiter$/$cr/;
   print FH $line; #write stats
 };
 statistics_data($self,%$stats_hoh);
 close(FH);
 return $self;
}

sub save_binned_datasets { 
 my ($self, $fileout_preext, $print_order, $delimiter, $fileout_ext) = @_;
 $delimiter = delimiter($self) unless $delimiter;
 $fileout_preext = file_preextension($self) unless $fileout_preext;
 $print_order = [ print_order($self) ] unless scalar(@$print_order);
 $print_order = [ column_names($self) ] unless scalar(@$print_order); 
 $fileout_ext = '.txt' unless $fileout_ext;
 my ($header, $cr, $colname) = (undef, chr(13).chr(10), undef); 
 $header .= $_.$delimiter for @$print_order; $header =~ s/$delimiter$/$cr/; #compose header
 my $hoh_stats = +{ statistics($self) };
 my $hoh       = +{ hoh($self) };
 my $bin_data  = {}; #keep the bin data in memory
 #sort by the bin index number
 my @sorted_stat_keys = sort { substr($a,3) <=> substr($b,3) } keys(%$hoh_stats); 
 foreach my $bin (@sorted_stat_keys) {
   my $fileout = $fileout_preext.$bin.$fileout_ext;
   open(FH,">$fileout") || die "$0 can't open $file using save_binned_datasets method. $!";
   print FH $header; #write header
   $keychain = $hoh_stats -> {$bin} -> {'KEYCHAIN'};
   foreach my $key (@$keychain) {
      my $line = undef;
      $line .= $hoh->{$key}->{$_}.$delimiter for @$print_order; $line =~ s/$delimiter$/$cr/;
      print FH $line; #write stat
      $bin_hohs -> {$bin} -> {$key} -> {$_} = $hoh->{$key}->{$_} for @$print_order;
    };
   close(FH);
 };
 bin_data($self,%$bin_hohs); 
 return $self
}

#Pass the time vector as first arg
#Pass the desired number of bins per decade as the second arg
#Returns a two-dimension array of time bins
# where the first and second col are the lower and upper limits of the time bins
sub log_bins {
 my ($self, $raw_vector, $num_bin) = @_;
 $num_bin = bins_per_decade($self) unless $num_bin;
 my ($bins, $factor, $vector) = ([], 10**(1/$num_bin), [ sort {$a <=> $b} @$raw_vector ]);
 if ($vector -> [0] == 0) { shift(@$vector); push @$bins, [ 0, $vector -> [0] ] }; #trick to avoid multiply by zero at time zero
 my ($max, $min) = max_min_vector($vector);
 while ($min < $max) { push @$bins, [$min, $min * $factor]; $min *= $factor };
 bins($self, @$bins);
 return $self
}

sub linear_bins {
 my ($self, $raw_vector, $tol) = @_;
 $tol = tolerance($self) unless $tol;
 my ($bins, $vector) = ([], [ sort { $a <=> $b } @$raw_vector ]);
 my ($max, $min) = max_min_vector($vector);
 while ($min < $max) { push @$bins, [$min, $min + $tol]; $min += $tol };
 bins($self, @$bins);
 return $self
}

sub tolerance_bins {
 my ($self, $bins, $tol) = @_;
 $tol = tolerance($self) unless $tol;
 my $newbins = [];
 push @$newbins, [($_ - $tol), ($_ + $tol)] for @$bins;
 bins($self, @$newbins);
 return $self
}

#Note returns extension with the dot in front 
sub file_ext { my $file = $_[0]; my ($ext) = $file =~ /(\.[^.]+)$/; return $ext }

#Pass reference to an array and returns max and min values respectively
sub max_min_vector { my $vector = shift();  return [ sort {$a <=> $b} @$vector ] -> [-1], [ sort {$b <=> $a} @$vector ] -> [-1] }

#computes mean of reference to one-dimensional array
# returns undef for a vector that has zero values to avoid division by zero
sub vector_mean { my ($a, $s, $c, $v) = (shift(), undef, undef, undef); $c = @$a; $s += $_ for @$a; $v = $s/$c if $c; return $v }

#from a hash of hashes get a $slice list (array_ref) knowing the $hoh->{@keys}->{$col}
sub hoh_list_slice { my ($h, $c, $s) = (shift(),shift(),[]); foreach (@{ [ keys %$h ] }) { push @$s, $h->{$_}->{$c} }; return $s }

#from a hash of hashes get a $Hash slice (hash_ref) knowing the $hoh->{@keys}->{$col}
sub hoh_hash_slice { my ($h, $c, $s) = (shift(),shift(),{}); foreach (@{ [ keys %$h ] }) { $s->{$_} = $h->{$_}->{$c} }; return $s }

# Makes line endings of opened text end with DOS line endings
# Runs file tests to get probable number of columns
sub filetest {
 my ($self, $file, $ending_type, $delimiter) = @_;
 $file = file($self) unless $file;
 $delimiter = delimiter($self) unless $delimiter;
 $ending_type = 'dos' unless $ending_type;
 my ($output, $dos, $unix, $mac, $newline, $convert_to) = ([], "\x0d\x0a", "\x0a", "\x0d", "{!_!_!_NEWLINE_!_!_!}", undef);
 my ($cnt, $samples, $cols_per_sample, $probable_hash, $first_line) = (0, undef, [], {}, 0);
 open(FHIN,"<$file");      # open the file
 while ( <FHIN> ) {        # while not EOF ... not EOL !
  $_ =~ s/\x0d\x0a/$newline/g; $_ =~ s/\x0a/$newline/g;  $_ =~ s/\x0d/$newline/g;
  if    (lc($ending_type) eq 'unix')  { $convert_to = $unix } 
  elsif (lc($ending_type) eq 'mac' )  { $convert_to = $mac  } 
  elsif (lc($ending_type) eq 'dos' )  { $convert_to = $dos  };
  $_ =~  s/$newline/$convert_to/g;          # convert to correct line endings
  $first_line_in = $_;
  push @$output, $_;                        # add results to array_ref
  unless ($first_line) { $first_line_in =~ s/\s+$//; first_line($self, $first_line_in); $first_line++ };
  $cnt++
 };
 print "Filetest routine found $cnt lines in $file ... \n";
 #Randomly sample 20% of file for number of elements or columns per line
 if ($cnt < 100) { $samples = $cnt } else { $samples = int(0.2 * $cnt) };
 while ($samples--) {
  my $line = $output -> [int(rand()*$cnt)];
  push @$cols_per_sample, scalar(split(/$delimiter/, $line));
 };
 $probable_hash->{$_}++ for @$cols_per_sample;
 probable_column_number($self, [ sort { $probable_hash->{$b} <=> $probable_hash->{$a} } keys %$probable_hash ] -> [0]);
 close(FHIN);
 open(FHOUT,">$file"); 
 print FHOUT $_ for @$output;
 close(FHOUT);
 return $self
}

sub save_as_chimera_attribute {
 my ($self, $chimera_attribute_name, $col_with_attribute) = (@_);
 $chimera_attribute_name = clean_chimera_attribute_name($chimera_attribute_name);
 my $cr = chr(13).chr(10);
 my $hoh_file = file($self);
 print "Making a chimera attribute file from a Hoh text file...\n";
 print "Hoh file: $hoh_file\n";
 print "Chimera attribute name: $chimera_attribute_name\n";
 print "Column in Hoh file to make attribute from: $col_with_attribute\n";
 my %hoh = hoh($self);
 my $my_residue_hash = \%hoh;
 open(CHIMERA_FILE, ">$chimera_attribute_name.txt");
 my @attribute_header  = (
  "# From Krantz Lab Hoh.pm",
  "# Chimera attribute output file for residues",
  "# Text file is $hoh_file",
  "attribute: $chimera_attribute_name",
  "match mode: 1-to-1",
  "recipient: residues"
 );
 print CHIMERA_FILE join($cr, @attribute_header).$cr;
 my @keys = sort { $a <=> $b } keys %$my_residue_hash;
 foreach my $key (@keys) { print CHIMERA_FILE "\t:$key\t".$my_residue_hash->{$key}->{$col_with_attribute}.$cr }
 close(CHIMERA_FILE);
 print "Finished writing $chimera_attribute_name.txt\n";
 return $self;
 }

sub clean_chimera_attribute_name { 
 my $name = shift;
 $name =~ s/[\W_]+//g;
 $name = 'attr'.$name if ($name =~ /^\d/); 
 return lc(substr($name, 0, 1)).substr($name, 1, length($name) - 1); 
}

sub cs {
 my ($self, $it, $cs) = @_;
 $cs = case_sensitive($self) unless $cs;
 $it = uc($it) unless $cs;
 return $it;
}

sub line_to_array {
 if ($_[1]) { return split(/\s*$_[1]\s*/, $_[0]) if $_[0] =~ /\s*$_[1]\s*/ }
 else       { return split(/\s+/, $_[0]) if $_[0] =~ /\s+/ };
}

sub file_to_array { 
my $file = shift;
my $scalar = file_to_scalar($file);
my @a = split(/\s+/, $scalar);
for my $i (0..$#a) { $a[$i] = cs($self, $a[$i]) };
return @a;
}

sub scalar_to_file {
 my ($scalar, $file) = @_;
 open (FH, ">$file") || die "Cannot open file. Aborting. $!";
 print FH "$scalar";
 close (FH);
 return $scalar;
}

sub file_to_scalar {
 my $file = shift;
 open(FH, "<$file") || die "$0 can't open $file using file_to_scalar method. $!\n";
 my $scalar = do { local $/;  <FH> }; #/change the default file separator locally 
 close (FH);
 return $scalar;
}
############################# PROPERTY SETS/GETS ###################################

###ALL HASHES

sub hoh {
  my $self = shift;
  if (@_) { %{ $self->{HOH} } = @_ };
  return %{ $self->{HOH} };
}

sub column_equations {
  my $self = shift;
  if (@_) { %{ $self->{COLUMN_EQUATIONS} } = @_ };
  return %{ $self->{COLUMN_EQUATIONS} };
}

sub statistics {
  my $self = shift;
  if (@_) { %{ $self->{STATISTICS} } = @_ };
  return %{ $self->{STATISTICS} };
}

sub statistics_data {
  my $self = shift;
  if (@_) { %{ $self->{STATISTICS_DATA} } = @_ };
  return %{ $self->{STATISTICS_DATA} };
}

sub bin_data {
  my $self = shift;
  if (@_) { %{ $self->{BIN_DATA} } = @_ };
  return %{ $self->{BIN_DATA} };
}

#### ALL ARRAYS ####

sub bins {
 my $self = shift;
 if (@_) { @{ $self->{BINS} } = @_ };
 return @{ $self->{BINS} };
}

sub print_order {
 my $self = shift;
 if (@_) {
  my @in =  @_;
  for my $i (0..$#in) { $in[$i] = cs($self, $in[$i]) };
  @{ $self->{PRINT_ORDER} } = @in; 
};
  return @{ $self->{PRINT_ORDER} };
}

sub statistics_print_order {
 my $self = shift;
 if (@_) {
  my @in =  @_;
  for my $i (0..$#in) { $in[$i] = cs($self, $in[$i]) };
  @{ $self->{STATISTICS_PRINT_ORDER} } = @in; 
};
  return @{ $self->{STATISTICS_PRINT_ORDER} };
}

sub statistics_columns_to_analyze {
 my $self = shift;
 if (@_) {
  my @in =  @_;
  for my $i (0..$#in) { $in[$i] = cs($self, $in[$i]) };
  @{ $self->{STATISTICS_COLUMNS_TO_ANALYZE} } = @in; 
 };
 return @{ $self->{STATISTICS_COLUMNS_TO_ANALYZE} };
}

sub column_names {
 my $self = shift;
 if (@_) {
  my @in =  @_;
  for my $i (0..$#in) { $in[$i] = cs($self, $in[$i]) };
  @{ $self->{COLUMN_NAMES} } = @in; 
 };
 return @{ $self->{COLUMN_NAMES} };
}

sub sorted_keys {
 my $self = shift;
 if (@_) { @{ $self->{SORTED_KEYS} } = @_ };
 return @{ $self->{SORTED_KEYS} };
}

sub original_keys { 
 my $self = shift;
 if (@_) { @{ $self->{ORIGINAL_KEYS} } = @_ };
 return @{ $self->{ORIGINAL_KEYS} };
}

###ALL SCALARS
sub unsorted {
 my $self = shift;
 if (@_) { $self->{UNSORTED} = shift };
 return $self->{UNSORTED};
}

sub sort_column {
 my $self = shift;
 if (@_) { $self->{SORT_COLUMN} = shift };
 return $self->{SORT_COLUMN};
}

sub sort_type {
 my $self = shift;
 if (@_) { $self->{SORT_TYPE} = shift };
 return $self->{SORT_TYPE};
}

sub sort_order {
 my $self = shift;
 if (@_) { $self->{SORT_ORDER} = shift };
 return $self->{SORT_ORDER};
}

sub print_order_file {
 my $self = shift;
 if (@_) { $self->{PRINT_ORDER_FILE} = shift }; 
 return $self->{PRINT_ORDER_FILE};
}

sub file_out {
 my $self = shift;
 if (@_) { $self->{FILE_OUT} = shift }; 
 return $self->{FILE_OUT};
}

sub file {
 my $self = shift;
 if (@_) { $self->{FILE} = shift }; 
 return $self->{FILE};
}

sub file_type {
 my $self = shift;
 if (@_) { $self->{FILE_TYPE} = shift }; 
 return $self->{FILE_TYPE};
}

sub file_extension {
 my $self = shift;
 $self->{FILE_EXTENSION} = shift if @_;
 $self->{FILE_EXTENSION} = file_ext(file($self)) if file($self);
 return $self->{FILE_EXTENSION};
}

sub file_out_extension {
 my $self = shift;
 if (@_) { $self->{FILE_OUT_EXTENSION} = shift }; 
 return $self->{FILE_OUT_EXTENSION};
}

sub file_preextension {
 my $self = shift;
 if (@_) { $self->{FILE_PREEXTENSION} = shift };
 return $self->{FILE_PREEXTENSION};
}

sub file_out_preextension {
 my $self = shift;
 if (@_) { $self->{FILE_OUT_PREEXTENSION} = shift };
 return $self->{FILE_OUT_PREEXTENSION};
}

sub generate_keys {
 my $self = shift;
 if (@_) { $self->{GENERATE_KEYS} = shift }; 
 return $self->{GENERATE_KEYS};
}

sub key_names {
 my $self = shift;
 if (@_) { $self->{KEY_NAMES} = shift }; 
 return $self->{KEY_NAMES};
}

sub delimiter {
 my $self = shift;
 if (@_) {  $self->{DELIMITER} = shift };
 return $self->{DELIMITER};
}

sub case_sensitive {
 my $self = shift;
 if (@_) { $self->{CASE_SENSITIVE} = shift };
 return $self->{CASE_SENSITIVE};
}

sub filekeys {
  my $self = shift;
  if (@_) { $self->{FILEKEYS} = shift };
  return $self->{FILEKEYS};
}

sub tolerance {
 my $self =shift;
 if (@_) { $self->{TOLERANCE} = shift };
 return $self->{TOLERANCE};
}

sub binned_column {
 my $self =shift;
 if (@_) { $self->{BINNED_COLUMN} = shift };
 return $self->{BINNED_COLUMN};
}

sub bins_per_decade {
 my $self =shift;
 if (@_) { $self->{BINS_PER_DECADE} = shift };
 return $self->{BINS_PER_DECADE};
}

sub bin_number {
 my $self =shift;
 if (@_) { $self->{BIN_NUMBER} = shift };
 return $self->{BIN_NUMBER};
}

sub header_off {
 my $self = shift;
 if (@_) { $self->{HEADER_OFF} = shift };
 return $self->{HEADER_OFF}
}
sub header {
 my $self = shift;
 if (@_) { $self->{HEADER} = shift };
 return $self->{HEADER};
}

sub original_header {
 my $self = shift;
 if (@_) { $self->{ORIGINAL_HEADER} = shift };
 return $self->{ORIGINAL_HEADER};
}

sub original_header_length {
 my $self = shift;
 if (@_) { $self->{ORIGINAL_HEADER_LENGTH} = shift };
 return $self->{ORIGINAL_HEADER_LENGTH};
}

sub column_number {
 my $self = shift;
 if (@_) { $self->{COLUMN_NUMBER} = shift };
 return $self->{COLUMN_NUMBER};
}

sub probable_column_number {
 my $self = shift;
 if (@_) { $self->{PROBABLE_COLUMN_NUMBER} = shift };
 return $self->{PROBABLE_COLUMN_NUMBER};
}

sub first_line {
 my $self = shift;
 if (@_) { $self->{FIRST_LINE} = shift };
 return $self->{FIRST_LINE};
}

return 1;
