BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "double precision" )
T.set_name      ( "thresh_SCF" )
T.set_doc       ( "Threshold on the convergence of the Hartree Fock energy" )
T.set_ezfio_dir ( "Hartree_Fock" )
T.set_ezfio_name( "thresh_SCF" )
T.set_output    ( "output_Hartree_Fock" )
print T

T = EZFIO_Provider()
T.set_type      ( "integer" )
T.set_name      ( "n_it_scf_max" )
T.set_doc       ( "Maximum number of SCF iterations" )
T.set_ezfio_dir ( "Hartree_Fock" )
T.set_ezfio_name( "n_it_scf_max" )
T.set_output    ( "output_Hartree_Fock" )
print T


END_SHELL




