#!/bin/bash
#!/bin/bash
#BSUB -q short-serial
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -J eddyjobArray[1-4]
/group_workspaces/jasmin2/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp -s linux -e N:/0Peter/curr/ECsystem N:/0Peter/curr/ECsystem/eddypro/processing${LSB_JOBINDEX}.eddypro
