package Chart::Clicker::Renderer::Step;
$Chart::Clicker::Renderer::Step::VERSION = '2.88';
use Moose;

# ABSTRACT: Line renderer

extends 'Chart::Clicker::Renderer';

use Geometry::Primitive::Point;
use Graphics::Primitive::Brush;
use Graphics::Primitive::Operation::Stroke;
use Geometry::Primitive::Circle;

#number of defined points we must have around another point
#to render a line instead of a scatter

use constant MIN_DEFINED_SURROUNDING_POINTS => 5;  #B. Krantz: in this plot mode this constant may not be needed


has 'brush' => (
    is => 'rw',
    isa => 'Graphics::Primitive::Brush',
    default => sub { Graphics::Primitive::Brush->new(width => 2) }
);


has 'shape' => (
    is => 'rw',
    isa => 'Geometry::Primitive::Shape',
);


has 'shape_brush' => (
    is => 'rw',
    isa => 'Graphics::Primitive::Brush',
);

#B. Krantz Hack start
#New attribute called type which designates whether this is a vertical- or horizontal-step graph
has 'type' => (
    is  => 'rw',
    isa => 'Str',
    default   => 'vertical',
);
#B. Krantz Hack end


sub finalize {
    my ($self) = @_;

    my $width = $self->width;
    my $height = $self->height;

    my $clicker = $self->clicker;

    my $dses = $clicker->get_datasets_for_context($self->context);
    my %accum;
    foreach my $ds (@{ $dses }) {
        foreach my $series (@{ $ds->series }) {
            my $ctx = $clicker->get_context($ds->context);
            my $domain = $ctx->domain_axis;
            my $range = $ctx->range_axis;
            my $color = $clicker->color_allocator->next;
            my @vals = @{ $series->values };
            my @keys = @{ $series->keys };

#B. Krantz Hack starts: going to change those input x and y values to stepify them
            #memorize keys and vals in case needed later
            my ($step_vals, $step_keys) = ([], []);
            my $step_graph_type = lc($self->type);
            $step_graph_type = 'vertical' unless (($step_graph_type eq 'vertical') || ($step_graph_type eq 'horizontal'));          
            if    ($step_graph_type eq 'vertical') {
              ($step_keys, $step_vals) = _vertical_stepify(\@keys, \@vals); #vertical_stepify
            }
            elsif ($step_graph_type eq 'horizontal') {
              ($step_keys, $step_vals) = _horizontal_stepify(\@keys, \@vals); #horizontal_stepify
            };
            @vals = @$step_vals;
            @keys = @$step_keys;
            #Note had to change the key_count index here to reflect the
            #increase in the number of points due to stepification.
            my $kcount = scalar(@vals) - 1; 
            # OLD CODE: my $kcount = $series->key_count - 1;
#B. Krantz Hack end

            my $skip = 0;
            my $previous_x = -1;
            my $previous_y = -1;
            my $min_y_delta_on_same_x = $height / 100;

            for(0..$kcount) {

                my $key = $keys[$_];

                my $x = $domain->mark($width, $key);
                next unless defined($x);
                $skip = 1 unless defined $vals[$_];
                my $ymark = $range->mark($height, $vals[$_]);
                next unless defined($ymark);

                if($self->additive) {
                    if(exists($accum{$key})) {
                        $accum{$key} += $ymark;
                        $ymark = $accum{$key};
                    } else {
                        $accum{$key} = $ymark;
                    }
                }

                my $y = $height - $ymark;
                if( $_ == 0 || $skip ) {
                    my $lineop = Graphics::Primitive::Operation::Stroke->new(
                        brush => $self->brush->clone
                    );
                    $lineop->brush->color($color);
                    $self->do($lineop);
                    $self->move_to($x, $y);

                    my $start_new_line = 1;
                    foreach my $i ($_..($_ + MIN_DEFINED_SURROUNDING_POINTS)) {
                        if ($i > 0 && $i < @vals && !defined($vals[$i])) {
                            $start_new_line = 0;
                        }
                    }
                    if ($start_new_line) {
                        $skip = 0;
                    }
                    else {
                        my $shape = Geometry::Primitive::Circle->new(radius => 3);
                        $shape->origin(Geometry::Primitive::Point->new(x => $x, y => $y));
                        $self->path->add_primitive($shape);
                        my $fill = Graphics::Primitive::Operation::Fill->new(
                            paint => Graphics::Primitive::Paint::Solid->new(
                                color => $color
                            )
                        );
                        $self->do($fill);
                    }

                }
                else {
                    # when in fast mode, we plot only if we moved by more than
                    # 1 of a pixel on the X axis or we moved by more than 1%
                    # of the size of the Y axis.
                    if( $clicker->plot_mode ne 'fast' ||
                        $x - $previous_x > 1 ||
                        abs($y - $previous_y) > $min_y_delta_on_same_x
                      )
                    {
                        $self->line_to($x, $y);
                        $previous_x = $x;
                        $previous_y = $y;
                    }
                }

            }
            my $op = Graphics::Primitive::Operation::Stroke->new;
            $op->brush($self->brush->clone);
            $op->brush->color($color);
            $self->do($op);

#START shape hack B. Krantz
#Need to change keys and vals back to original vals and keys and need to redo $kcount
            my @vals = @{ $series->values }; #B. Krantz Hack to remember original x and y's
            my @keys = @{ $series->keys }; #B. Krantz Hack to remember original x and y's
            $kcount = $series->key_count - 1;
#END shape hack B. Krantz

            if(defined($self->shape)) {
                for(0..$kcount) {
                    my $key = $keys[$_];
                    my $x = $domain->mark($width, $key);
                    next unless defined($x);
                    my $ymark = $range->mark($height, $vals[$_]);
                    next unless defined($ymark);

                    if($self->additive) {
                        if(exists($accum{$key})) {
                            $ymark = $accum{$key};
                        } else {
                            $accum{$key} = $ymark;
                        }
                    }

                    my $y = $height - $ymark;

                    $self->move_to($x, $y);
                    $self->draw_point($x, $y, $series, $vals[$_]);
                }

                # Fill the shape
                my $op2 = Graphics::Primitive::Operation::Fill->new(
                    paint => Graphics::Primitive::Paint::Solid->new(
                        color => $color
                    )
                );
                if(defined($self->shape_brush)) {
                    $op2->preserve(1);
                }
                $self->do($op2);

                # Optionally stroke the shape
                if(defined($self->shape_brush)) {
                    my $op3 = Graphics::Primitive::Operation::Stroke->new;
                    $op3->brush($self->shape_brush->clone);
                    $self->do($op3);
                }
            }

        }
    }
    return 1;
}

sub draw_point {
    my ($self, $x, $y, $series, $count) = @_;
    my $shape = $self->shape->clone;
    $shape->origin(Geometry::Primitive::Point->new(x => $x, y => $y));
    $self->path->add_primitive($shape);
}

#B. Krantz internal stepify routines 
#Horizontal Step graph takes regular line graph data
#and converts to a horizontal-step style graph
sub _horizontal_stepify {
 my ($x, $y, $d) = (shift(), shift(), []);
 push @$d, [shift(@$x), shift(@$y)];
 while (@$x and @$y) {  
  push @$d, [shift(@$x), $d -> [-1] -> [1]];
  push @$d, [$d -> [-1] -> [0], shift(@$y)];
 };
 return [map $_ -> [0], @$d], [map $_ -> [1], @$d]
};

#Vertical Step graph takes regular line graph data
#and converts to a vertical-step style graph
sub _vertical_stepify {
 my ($x, $y, $d) = (shift(), shift(), []);
 push @$d, [shift(@$x), shift(@$y)];
 while (@$x and @$y) {  
  push @$d, [$d -> [-1] -> [0], shift(@$y)];
  push @$d, [shift(@$x), $d -> [-1] -> [1]];
 };
 return [map $_ -> [0], @$d], [map $_ -> [1], @$d]
};
#B. Krantz Hack end

__PACKAGE__->meta->make_immutable;

no Moose;

1;

__END__

=pod

=head1 NAME

Chart::Clicker::Renderer::Step - Vertical and Horizontal-step renderer

=head1 VERSION

version 2.88

=head1 SYNOPSIS

  my $step_renderer = Chart::Clicker::Renderer::Step->new(
    #Required 'type': choose 'vertical' or 'horizontal' but defaults to 'vertical'
    type => 'horizontal', 
    #optional 'shape' if you want to display datapoints distinctively
    shape => Geometry::Primitive::Circle->( radius => 3 ), 
    brush => Graphics::Primitive::Brush->new({
      #...some brush attributes
    })
  );

=head1 DESCRIPTION

Chart::Clicker::Renderer::Step renders a dataset as a special vertical- or horizontal-step line graph 
often used in statistical plots. This module's guts were borrowed from L<Chart::Clicker::Renderer::Line> and 
will use its attributes, of course, since they are preserved here. One additional attribute, 'type', 
is used to specify whether the step graph is 'vertical' or 'horizontal'. The default is 'vertical'.
As the step-style graph appears to create new points in the rendering, the 'shape' attribute can be set 
to show the actual data points on the line graph.

=for HTML <p><img src="https://github.com/bakrantz/chart-clicker-renderer-step/blob/master/examples/vertical-step-plot-example.png?raw=true" width="500" height="250" alt="Vertical Step Chart"/></p>

=head1 ATTRIBUTES

=head2 additive

If true, the lines are drawn "stacked", each key accumulates based on those drawn below it.

=head2 brush

Set/Get a L<brush|Graphics::Primitive::Brush> to be used for the lines.

=head2 shape

Set a L<shape|Geometry::Primitive::Shape> object to draw at each of the data points. 
There are many shapes available in L<Geometry::Primitive>:

=over 5

=item L<Circle|Geometry::Primitive::Circle>

=item L<Ellipse|Geometry::Primitive::Ellipse>

=item L<Point|Geometry::Primitive::Point>

=item L<Polygon|Geometry::Primitive::Polygon>

=item L<Rectangle|Geometry::Primitive::Rectangle>

=back


=head2 shape_brush

Set/Get the L<brush|Graphics::Primitive::Brush> to be used on the shapes at each 
point.  If no shape_brush is provided, then the shapes will be filled.
The brush allows you to draw a "halo" around each shape.  This sometimes help
to separate the points from the lines and make them more distinct.

=head2 type

Set/Get the type of Step graph. There are two options: 'vertical' or 'horizontal'. Default is 'vertical'.

=head1 METHODS

=head2 draw_point

Called for each point encountered on the line.

=head1 AUTHOR

Cory G Watson <gphat@cpan.org> wrote original L<Chart::Clicker::Renderer::Line>

B. Krantz modified original to make L<Chart::Clicker::Renderer::Step>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2014 by Cold Hard Code, LLC.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
