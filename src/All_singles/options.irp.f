BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "integer" )
T.set_name      ( "N_det_max_fci" )
T.set_doc       ( "Max number of determinants in the wave function" )
T.set_ezfio_dir ( "all_singles" )
T.set_ezfio_name( "N_det_max_fci" )
T.set_output    ( "output_all_singles" )
print T

T.set_type      ( "logical" )
T.set_name      ( "do_pt2_end" )
T.set_doc       ( "If true, compute the PT2 at the end of the selection" )
T.set_ezfio_name( "do_pt2_end" )
print T

T.set_type      ( "double precision" )
T.set_name      ( "pt2_max" )
T.set_doc       ( """The selection process stops when the largest PT2 (for all the states) 
is lower than pt2_max in absolute value""" )
T.set_ezfio_name( "pt2_max" )
print T
END_SHELL

