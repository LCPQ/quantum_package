BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "logical" )
T.set_name      ( "do_direct_integrals" )
T.set_doc       ( "If true, compute integrals on the fly" )
T.set_ezfio_dir ( "bielec_integrals" )
T.set_ezfio_name( "direct" )
T.set_output    ( "output_biints" )
print T

T.set_type      ( "logical" )
T.set_name      ( "write_mo_integrals" )
T.set_doc       ( "If true, write MO integrals in EZFIO" )
T.set_ezfio_name( "write_mo_integrals" )
print T

T.set_name      ( "write_ao_integrals" )
T.set_doc       ( "If true, write AO integrals in EZFIO" )
T.set_ezfio_name( "write_ao_integrals" )
print T

T.set_name      ( "read_mo_integrals" )
T.set_doc       ( "If true, read MO integrals in EZFIO" )
T.set_ezfio_name( "read_mo_integrals" )
print T

T.set_name      ( "read_ao_integrals" )
T.set_doc       ( "If true, read AO integrals in EZFIO" )
T.set_ezfio_name( "read_ao_integrals" )
print T

T.set_type      ( "double precision" )
T.set_name      ( "ao_integrals_threshold" )
T.set_doc       ( "If <pq|rs> < ao_integrals_threshold, <pq|rs> = 0" )
T.set_ezfio_name( "threshold_ao" )
print T

T.set_name      ( "mo_integrals_threshold" )
T.set_doc       ( "If <ij|kl> < mo_integrals_threshold, <ij|kl> = 0" )
T.set_ezfio_name( "threshold_mo" )
print T

END_SHELL


