diskutil unmount /dev/disk1
dd if=floppy.flp of=/dev/disk1
diskutil mount /dev/disk1
diskutil eject /dev/disk1