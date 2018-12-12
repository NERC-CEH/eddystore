#!/bin/bash
#!/bin/bash
#BSUB -q short-serial
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -W 00:10
#BSUB -J jobArray[1-nIntervals]
/group_workspaces/jasmin2/eddystore/eddypro-engine-master/bin/eddypro_rp -s linux -e /group_workspaces/jasmin2/eddystore/stations/test N:/0Peter/curr/ECsystem/eddypro/jasmin/eddystore/stations/test/ini/processing{LSB_JOBINDEX}.eddypro
