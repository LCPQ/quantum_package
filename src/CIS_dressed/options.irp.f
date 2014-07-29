BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "integer" )
T.set_name      ( "n_state_cis" )
T.set_doc       ( "Number of states asked for the CIS vector" )
T.set_ezfio_dir ( "cis_dressed" )
T.set_ezfio_name( "n_state_cis" )
T.set_output    ( "output_CIS" )
print T

T.set_name      ( "n_core_cis" )
T.set_doc       ( "Number of core orbitals in the dressed CIS" )
T.set_ezfio_name( "n_core_cis" )
print T

T.set_name      ( "n_act_cis" )
T.set_doc       ( "Number of active orbitals in the dressed CIS" )
T.set_ezfio_name( "n_act_cis" )
print T

T.set_type      ( "logical" )
T.set_name      ( "mp2_dressing" )
T.set_doc       ( "If true, use MP2 dressing in the dressed CIS" )
T.set_ezfio_name( "mp2_dressing" )
print T

T.set_name      ( "standard_doubles" )
T.set_doc       ( "If true, use standard doubles in the dressed CIS" )
T.set_ezfio_name( "standard_doubles" )
print T

T.set_name      ( "en_2_2" )
T.set_doc       ( "TODO")
T.set_ezfio_name( "en_2_2" )
print T

END_SHELL


