BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "double precision" )
T.set_name      ( "z_one_point" )
T.set_doc       ( "z point on which the integrated delta rho is calculated" )
T.set_ezfio_dir ( "properties" )
T.set_ezfio_name( "z_one_point" )
T.set_output    ( "output_full_ci" )
print T

END_SHELL

