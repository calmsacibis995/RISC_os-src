#
# These are the devices specific to M2000 On-Board SCSI disk based systems.
#

class(extra_devices) {
        link(dsk/sdc0d0s0, root)
        link(dsk/sdc0d0s1, swap)
        link(dsk/sdc0d0s6, usr)
        link(dsk/sdc0d0vh, vh)
        link(rdsk/sdc0d0s0, rroot)
        link(rdsk/sdc0d0s6, rusr)
        link(rdsk/sdc0d0vh, rvh)
}

