use Hoh; #a non-cpan module I use to open flat files into hash of hashes format and has other with other stats methods

#It is assumed you download Hoh.pm and place it in your working directory
#Also install Step.pm in the proper place in your perl lib tree for Chart::Clicker
#To find the Chart::Clicker libraries use the following command:
# perldoc -lm Chart::Clicker 

#Some other modules used in Chart::Clicker 
use Graphics::Color::RGB;
use Geometry::Primitive::Circle;

#Setup a plot hash containing the instructions of what and how you 
#will graph your data
my $plot = {};
   $plot->{'DATASETS'} ->{'plot1'} = 
     {'X' => 'X', 'Y' => 'Y', 'COLOR' => Graphics::Color::RGB->new(red => 1, green => 0, blue => 0, alpha => 1), 'STYLE' => 'STEP', 
      'SHAPE' => Geometry::Primitive::Circle->new('radius' => 3)};

#Multi-dataset plot types in the same graph is  supported 
#  but commenting out for this demo 
#   $plot->{'DATASETS'} ->{'plot2'} = 
#     {'X' => 'X', 'Y' => 'Y', 'COLOR' => Graphics::Color::RGB->new(red => 0, green => 0, blue => 0, alpha => 1), 'STYLE' => 'Point'};
#   $plot->{'DATASETS'} ->{'plot3'} = 
#     {'X' => 'X', 'Y' => 'Y', 'COLOR' => Graphics::Color::RGB->new(red => 1, green => 0, blue => 1, alpha => 1), 'STYLE' => 'HORIZONTAL_STEP'};

#Set some general plot parameters
   $plot->{'X_LABEL'}  = 'Measurement Error (cm)';
   $plot->{'Y_LABEL'}  = 'Count';
   $plot->{'TITLE'}    = 'A Fancy Vertical-Step Plot Title';
   $plot->{'FORMAT'}   = 'png';
   $plot->{'FILE'}     = 'vertical-step-plot-example.png';
   $plot->{'AUTOSCALE'}= 1; #Autoscale feature special to Hoh.pm

#Get data from flat file to plot
my $hoh = Hoh->new();
   $hoh -> delimiter(',');
   $hoh -> generate_keys(1);
   $hoh -> load('xy-data.txt') -> create_xy_plot(%$plot);

