	BITS 16

	jmp short bootloader_start	; Jump past disk description section
	nop				; Pad out before disk description

;BIOS parameter block(BPB)
; ------------------------------------------------------------------
; Disk description table, to make it a valid floppy
; Note: some of these values are hard-coded in the source!
; Values are those used by IBM for 1.44 MB, 3.5" diskette
;these bytes must occupy exactly as the table given here http://wiki.osdev.org/FAT

OEMLabel		db "NYXBOOT "	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 1		; Reserved sectors for boot record, not a part of root directory!
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
					; (224 * 32 = 7168 = 14 sectors to read) one entry in root directory is 32bytes. hence total entries=224*32
					; 7168/512=14 sectors to be read..which will contain 
LogicalSectors		dw 2880		; Number of logical sectors
MediumByte		db 0F0h		; Media descriptor byte, bit0-0 if single sided else 1,bit1-0 if it has 9sectors per fat,1 if 8
;bit2-density, 0 if it has 80 tracks, 1 if it has 40 tracks. bit3-0 if fixed,1 if removable,bit4-7 unused and always 1.
;so oxf0 means single sided,9sectors per FAT,80 tracks,and removable disk.
SectorsPerFat		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides			dw 2		; Number of sides/heads, heads/cylinder
HiddenSectors		dd 0		; Number of hidden sectors,
LargeSectors		dd 0		; Number of LBA sectors
;end of BPB
;Start of Extended Boot Record 
DriveNo			dw 0		; Drive No: 0 	This number is useless because the media is likely to be moved to another machine and inserted in a drive with a different drive number.
Signature		db 41		; Drive signature: 41 for floppy
VolumeID		dd 00000000h	; Volume ID: any number, serial number is unique! and assigned by utility that formats it(current
;time and date may be used to assign serial number)
VolumeLabel		db "NYXOS      "; Volume Label: any 11 chars
FileSystem		db "FAT12   "	; File system type: don't change! 8 bytes!!exact
;end of EBR

; ------------------------------------------------------------------
; Main bootloader code

bootloader_start:
;The first three bytes EB 3C and 90 disassemble to JMP SHORT 3C NOP. 
;(The 3C value may be different.) The reason for this is to jump over the disk format information 
;(the BPB and EBPB). Since the first sector of the disk is loaded into ram at location 0x0000:0x7c00
; and executed, without this jump, the processor would attempt to execute data that isn't code.
	mov ax, 07C0h			; Set up 4K of stack space above buffer
	add ax, 544			; 8k buffer = 512 paragraphs + 32 paragraphs (loader) [ 16bytes= 1 paragraph]
						;512*16=8192=8k buffer  32*16(loader)=512 bytes for the bootloader!
						; = 8192+512=8704bytes
	;544(base 10) = 220(base 16)
	cli				; Disable interrupts while changing stack
	mov ss, ax ;stack segment=09E0
	;9e00-7c00=2200 (base 16) = 8704(base 10)
	mov sp, 4096;stack pointer=1000(in base 16)
	;so ss:sp=AE00 and we are loaded at 7C00 diff between them = 3200
	;4k stack space above the buffer+loader space!
	;NOTE=push decrements the stack pointer and pop increments the stack pointer. Hence for a 4k stack we
	;already point to end of stack :)
	sti				; Restore interrupts

	mov ax, 07C0h			; Set data segment to where we're loaded
	mov ds, ax

	; NOTE: A few early BIOSes are reported to improperly set DL
;http://www.ctyme.com/intr/rb-0621.htm
	cmp dl, 0 ;dl must be set by BIOS to indicate the boot drive!!
	je no_change;see DriveNo above in BPB

	mov [bootdev], dl		; Save boot device number-0x00 for the boot floppy drive. 
	mov ah, 8			; Get drive parameters
	int 13h
	jc near fatal_disk_error
	and cx, 3Fh			; Maximum sector number- device parameters when returned have lower 5 bits of cx reg to indicate max no of sectors!
	;so anding with 3f gives lower 5 bits
	mov [SectorsPerTrack], cx	; Sector numbers start at 1
	movzx dx, dh			; Maximum head number  , movzx-move zero extend!
	add dx, 1			; Head numbers start at 0 - add 1 for total
	mov [Sides], dx

no_change:;we already have the correct( or consistent ) info ( viz sectorspertrack,drive number,etc)
	mov eax, 0			; Needed for some older BIOSes

;FAT12 disk storage
;how a typical fat12 formatted disk looks like-
;http://www.brokenthorn.com/Resources/OSDev6.html
; First, we need to load the root directory from the disk. 
;Technical details:
; Start of root = ReservedForBoot + NumberOfFats * SectorsPerFat = logical 19
; Number of root = RootDirEntries * 32 bytes/entry / 512 bytes/sector = 14
; Start of user data = (start of root) + (number of root) = logical 33

floppy_ok:				; Ready to read first block of data
	mov ax, 19			; Root dir starts at logical sector 19
	call l2hts

	mov si, buffer			; Set ES:BX to point to our buffer (see end of code)
	mov bx, ds ; 07c0
	mov es, bx 
	mov bx, si ;es:bx=buffer

	mov ah, 2			; Params for int 13h: read floppy sectors
	mov al, 14			; And read 14 of them ie read whole of the RootDirectory!
	;entire root directory occupies 14sectors

	pusha				; Prepare to enter loop

;int 13h ah=2
;es:bx= buffer to read sectors into
;al=no of sectors to be read
;dh=head number
;dl=drive number
;ch=low 8buts for cylinder number
;cl= sector number(bits 0-5)
read_root_dir:
	popa				; In case registers are altered by int 13h
	pusha
	
	stc				; A few BIOSes do not set properly on error
	int 13h				; Read sectors using BIOS
	;If Carry Flag (CF) is set, there was an error.
	jnc search_dir			; If read went OK, skip ahead
	call reset_floppy		; Otherwise, reset floppy controller and try again
	jnc read_root_dir		; Floppy reset OK?

	jmp reboot			; If not, fatal double error


search_dir:
	popa;a=all so push all and pop all

	mov ax, ds			; Root dir is now in [buffer]
	mov es, ax			; Set DI to this info
	mov di, buffer

	mov cx, word [RootDirEntries]	; Search all (224) entries
	mov ax, 0			; Searching at offset 0


next_root_entry:
	;dx used as temp,since loop uses cx as counter
	xchg cx, dx			; We use CX in the inner loop...

	mov si, kern_filename		; Start searching for kernel filename
	mov cx, 11 ; 0-10 bytes file name+extension (11 bytes)
	rep cmpsb
	je found_file_to_load		; Pointer DI will be at offset 11

	add ax, 32			; Bump searched entries by 1 (32 bytes per entry)

	mov di, buffer			; Point to next entry
	add di, ax
	;reached the next entry. next entry address in es:di
	xchg dx, cx			; Get the original CX back
	loop next_root_entry
	;all 224 entries searched!
	mov si, file_not_found		; If kernel is not found, bail out
	call print_string
	jmp reboot


found_file_to_load:			; Fetch cluster and load FAT into RAM
	mov ax, word [es:di+0Fh]	; Offset 11 + 15 = 26, contains 1st cluster
	;cluster number is located at 26th byte see=
    ;http://www.brokenthorn.com/Resources/OSDev6.html
	mov word [cluster], ax;stores the cluster containing our file

	mov ax, 1			; Sector 1 = first sector of first FAT
	call l2hts

	mov di, buffer			; ES:BX points to our buffer
	mov bx, di

	mov ah, 2			; int 13h params: read (FAT) sectors
	mov al, 9			; All 9 sectors of 1st FAT

	pusha				; Prepare to enter loop


read_fat:
	popa				; In case registers are altered by int 13h
	pusha

	stc
	int 13h				; Read sectors using the BIOS

	jnc read_fat_ok			; If read went OK, skip ahead
	call reset_floppy		; Otherwise, reset floppy controller and try again
	jnc read_fat			; Floppy reset OK?

; ******************************************************************
fatal_disk_error:
; ******************************************************************
	mov si, disk_error		; If not, print error message and reboot, we point si to error message later used in instruction lodsb in print_string
	call print_string
	jmp reboot			; Fatal double error

;OUR BUFFER NOW CONTAINS FAT (we have loaded all 9sectors of FAT into buffer)
read_fat_ok:
	popa

	mov ax, 2000h			; Segment where we'll load the kernel
	mov es, ax
	mov bx, 0

	mov ah, 2			; int 13h floppy read params
	mov al, 1			; read 1 sector

	push ax				; Save in case we (or int calls) lose it

;From the organization of the disk, it is seen that the first 33 sectors are predefined.
;(0-boot sector,1-9 fat1,10-19 fat 2 + 14 sectors for root directory) so 0-32 sectors=33 sectors
;The actual data sector that holds user data does not exist in these first 33 sectors and starts at sector number 33 (remember we start with 0). 
;The entries in positions 0 and 1 of the FAT are reserved. Therefore, it is entry 2 of the FAT 
;that actually contains the description for physical sector number 33. 
;Therefore, physical sector number = 33 + FAT entry number - 2 
;For example, entry 5 of the FAT would actually refer to physical data sector number 36. 

; Now we must load the FAT from the disk. Here's how we find out where it starts:
; FAT cluster 0 = media descriptor = 0F0h
; FAT cluster 1 = filler cluster = 0FFh
; Cluster start = ((cluster number) - 2) * SectorsPerCluster + (start of user)
;               = (cluster number) + 31

;WE NOW LOAD EACH FILE CLUSTER INTO OUR NEW BUFFER 2000:pointer!
;Convert CHS to LBA
;root dir contains chs addressing scheme to store starting cluster number
load_file_sector:
	mov ax, word [cluster]
	add ax, 31
	;ax now contains the actual physical sector number!!
	call l2hts			; Make appropriate params for int 13h

	mov ax, 2000h			; Set buffer past what we've already read
	mov es, ax
	mov bx, word [pointer] ;pointer is each time incremented by 512 bytes upon loading a cluster
	;till end of file is reached
	;es:bx points to 2000:0 where we want to load our kernel (our buffer to store the read clusters)

	pop ax				; Save in case we (or int calls) lose it
	;upon popping al=1,ah=2 read 1 sector
	push ax

	stc
	int 13h

	jnc calculate_next_cluster	; If there's no error...

	call reset_floppy		; Otherwise, reset floppy and retry
	jmp load_file_sector


	; In the FAT, cluster values are stored in 12 bits, so we have to
	; do a bit of maths to work out whether we're dealing with a byte
	; and 4 bits of the next byte -- or the last 4 bits of one byte
	; and then the subsequent byte!

;Notice all even clusters accopy all of the first byte, but part of the second. Also notice that all odd clusters occopy a part of their first byte, but all of the second!
;Okay, so what we need to do is to read a 2byte (word) value from the FAT (This is our cluster).
;If the cluster is even, Mask out the top 4 bits, as it belongs to the next cluster.
;If it is odd, shift it down 4 bits (to discard the bits used by the first cluster.

;   01011101       0111010   01110101  00111101  0011101  0111010  0011110 0011110
;   |                |              |              |            |               |
;   |                |1st cluster   |              |3rd cluster-|               |
;   |-0 cluster  ----|              |2nd cluster---|            |4th cluster----|
;  0th byte       1st byte  2nd byte  3rd byte  4th byte (index acc to memory)
; 0th byte since start of our fat and so on..

;So suppose our cluster no=2
;our next cluster will be=fat[cluster]=fat[2]=1100 10111100(higher to lower bits!!)
;to get to that we must load, 3rd,4th byte(only lower 4bits) !
;VERY IMPORTANT
;so word to be loaded=cluster+cluster/2=cluster*3/2
;take example for current cluster=4, you need 6th and 7th bytes
;so word=4+4/2=6 followed by masking!
calculate_next_cluster:
	mov ax, [cluster]
	mov dx, 0
	mov bx, 3
	mul bx ;ax=ax*3
	mov bx, 2
	div bx				; DX = [cluster] mod 2
	mov si, buffer
	add si, ax			; AX = word in FAT for the 12 bit entry
	mov ax, word [ds:si] ;we load word into ax , word=16bits whereas cluster number=12bits

	or dx, dx			; If DX = 0 [cluster] is even; if DX = 1 then it's odd

	jz even				; If [cluster] is even, drop last 4 bits of word(word is 16bits)
					; with next cluster; if odd, drop first 4 bits

;higher byte address loaded into higher register
odd:
	shr ax, 4			; Shift out first 4 bits (they belong to another entry)
	jmp short next_cluster_cont


even:
	and ax, 0FFFh			; Mask out final 4 bits


next_cluster_cont:
	mov word [cluster], ax		; Store cluster

	cmp ax, 0FF8h			; FF8h = end of file marker in FAT12
	jae end

	add word [pointer], 512		; Increase buffer pointer 1 sector length
	jmp load_file_sector


end:					; We've got the file to load!
	pop ax				; Clean up the stack (AX was pushed earlier)
	mov dl, byte [bootdev]		; Provide kernel with boot device info

	jmp 2000h:0000h			; Jump to entry point of loaded kernel!


; ------------------------------------------------------------------
; BOOTLOADER SUBROUTINES

reboot:
	mov ax, 0
	int 16h				; Wait for keystroke
	mov ax, 0
	int 19h				; Call the bootstrap loader
	;This interrupt attempts to load the sector at head 0, track 0, sector
    ;1, on the first diskette into memory at 0:7C00h. If unable, it then
    ;attempts to load the sector at head 0, track 0, sector 1 of the first
    ;hard disk.
    ;If INT 19h is successful, control is transferred to the first byte of
    ;the sector, which has been read in at memory location 0:7C00h.
    ;That is, CS is set to 0 and IP is set to 7C00h.


print_string:				; Output string in SI to screen
	pusha

	mov ah, 0Eh			; int 10h teletype function

.repeat:
	lodsb				; Get char from string
	cmp al, 0
	je .done			; If char is zero, end of string
	int 10h				; Otherwise, print it
	jmp short .repeat

.done:
	popa
	ret


reset_floppy:		; IN: [bootdev] = boot device; OUT: carry set on error
	push ax
	push dx
	mov ax, 0
	mov dl, byte [bootdev]
	stc
	int 13h
	pop dx
	pop ax
	ret


;http://www.brokenthorn.com/Resources/OSDev6.html
;LOGICAL BLOCK ADDRESSING to CYLINDER HEAD SECTOR addressing
;http://en.wikipedia.org/wiki/Logical_block_addressing
l2hts:			; Calculate head, track and sector settings for int 13h
			; IN: logical sector in AX, OUT: correct registers for int 13h
;read sector into memory
;AH = 02h
;AL = number of sectors to read (must be nonzero)
;CH = low eight bits of cylinder number
;CL = sector number 1-63 (bits 0-5)
;high two bits of cylinder (bits 6-7, hard disk only)
;DH = head number
;DL = drive number (bit 7 set for hard disk)
;ES:BX -> data buffer

;absolute sector 	= 	(LBA % sectors per track) + 1
;absolute head   	= 	(LBA / sectors per track) % number of heads
;absolute track 	= 	 LBA / (sectors per track * number of heads)

	push bx
	push ax

	mov bx, ax			; Save logical sector

	mov dx, 0			; First the sector
	;32bit by 16bit division
	;dx contains remainder,ax contains quotient
	div word [SectorsPerTrack];18 sectors per track..19/18
	add dl, 01h			; Physical sectors start at 1
	mov cl, dl			; Sectors belong in CL for int 13h
	mov ax, bx

	mov dx, 0			; Now calculate the head
	div word [SectorsPerTrack]
	mov dx, 0
	div word [Sides]
	mov dh, dl			; Head/side
	mov ch, al			; Track

	pop ax
	pop bx

	mov dl, byte [bootdev]		; Set correct device

	ret


; ------------------------------------------------------------------
; STRINGS AND VARIABLES

	kern_filename	db "KERNEL  BIN"	; MikeOS kernel filename 0-7 bytes file name, 8-10 extension

	disk_error	db "Floppy error! Press any key...", 0
	file_not_found	db "KERNEL.BIN not found!", 0

	bootdev		db 0 	; Boot device number for floppy drive
	cluster		dw 0 	; Cluster of the file we want to load
	pointer		dw 0 	; Pointer into Buffer, for loading kernel


; ------------------------------------------------------------------
; END OF BOOT SECTOR AND BUFFER START

	times 510-($-$$) db 0	; Pad remainder of boot sector with zeros
	dw 0AA55h		; Boot signature (DO NOT CHANGE!) 511 and 512th byte


buffer:				; Disk buffer begins (8k after this, stack starts)


; ==================================================================