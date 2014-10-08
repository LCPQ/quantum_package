BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "integer" )
T.set_name      ( "N_states" )
T.set_doc       ( "Number of states to consider" )
T.set_ezfio_dir ( "determinants" )
T.set_ezfio_name( "N_states" )
T.set_output    ( "output_dets" )
print T


T.set_type      ( "integer" )
T.set_name      ( "N_states_diag" )
T.set_doc       ( "Number of states to consider for the diagonalization " )
T.set_ezfio_dir ( "determinants" )
T.set_ezfio_name( "N_states_diag" )
T.set_output    ( "output_dets" )
print T


T.set_name      ( "N_det_max_jacobi" )
T.set_doc       ( "Maximum number of determinants diagonalized by Jacobi" )
T.set_ezfio_name( "N_det_max_jacobi" )
print T

T.set_type      ( "logical" )
T.set_name      ( "read_wf" )
T.set_doc       ( "If true, read the wave function from the EZFIO file" )
T.set_ezfio_name( "read_wf" )
T.set_output    ( "output_dets" )
print T

END_SHELL


