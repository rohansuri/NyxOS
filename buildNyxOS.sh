rm -rf ./bootload.bin #remove without confirmation
rm -rf ./kernel.bin
rm -rf ./floppy.iso
rm -rf ./floppy.flp
rm -rf ./floppy.img
rm -rf ./cdiso/floppy.flp
rm -rf ./tmp-loop
nasm -f bin -o bootload.bin bootload.asm #produce flat binary
nasm -f bin -o kernel.bin kernel.asm
dd if=/dev/zero bs=512 count=2880 > floppy.img
dd conv=notrunc if=bootload.bin of=floppy.img
rm -R tmp-loop
dev=`hdid -nobrowse -nomount floppy.img` #-nomount similar to -mount suppressed
mkdir tmp-loop && mount -t msdos ${dev} tmp-loop && cp kernel.bin tmp-loop
#mount is used to mount file systems -t is used to indicate the file system type
#so here our type is msdos (FAT12) right?
diskutil unmount tmp-loop
echo 'unmounted folder'
hdiutil detach ${dev}
rm -R tmp-loop
cp ./floppy.img ./floppy.flp
cp ./floppy.flp ./cdiso/floppy.flp
mkisofs -o floppy.iso -b floppy.flp cdiso/
echo 'done'