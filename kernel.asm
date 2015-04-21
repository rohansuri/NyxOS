BITS 16 ;The BITS directive specifies whether NASM should generate code designed to run on
;a processor operating in 16-bit mode, 32-bit mode or 64-bit mode. The syntax is BITS XX, 
;where XX is 16, 32 or 64.

;tasm vs nasm differs in procedure definitions
;a procedure is just another code label you CALL instead of JMP, NASM got it
;right.

start:
	cli				; Clear interrupts
	mov ax, 0
	mov ss, ax			; Set stack segment and pointer
	mov sp, 0FFFFh
	sti				; Restore interrupts

	cld				; The default direction for string operations
					; will be 'up' - incrementing address in RAM

	mov ax, 2000h			; Set all segments to match where kernel is loaded
	mov ds, ax			; After this, we don't need to bother with
	mov es, ax			; segments ever again, as MikeOS and its programs
	mov fs, ax			; live entirely in 64K
	mov gs, ax
	
	
	mov ah,0fh
	int 10h	
	mov [videomode],al
	mov [cols],ah
	
	
	call hide_cursor
	call draw ;bootscreen
	call clearscreen
	mov bl, 00011011b	;black on cyan
	mov dl, 0			; start col
	mov dh, 0			; start row
	mov si, 80			; Width
	mov di, 1			; end row
	call drawbox
	mov dl,36
	mov dh,0
	mov bh,0
	call move_cursor
	mov si,osname
	call printstr_tel
	
	;mov bl,40h
	mov bl, 01001111b	;white on red
	mov dl, 20			; start col
	mov dh, 5			; start row
	mov si, 40			; Width
	mov di, 15		; end row
	
	call drawbox
	
	
	
	mov dl,20
	mov dh,4
	mov bh,0
	call move_cursor

	mov si,select
	call printstr_tel
	
	
	call loadprograms
	mov dh,[mini]
	call drawpointer
	call keyboard
	


	jmp $
	
	bootmsg db 'Booting NyxOS'
	bootmsglen dw 13
	cmdline db 'Command Line',0
	about db 'About us',0
	shut db 'Shutdown (APM v1.2 needed)',0
	restart db 'Restart',0
	osname db 'NyxOS',0
	osnamelen dw 5
	empty db ' ',0
	point db '>',0
	aboutus db 'NyxOS is a minimalistic operating system,',0ah,'completely written in assembly language.',0ah,'It is a 16-bit OS that operates in Real Mode.',0AH,'Project by Rohan Suri, Ravish Thalesar and Rushabh Shah :)',0ah,'If you want to know how NyxOS was built, contact us at :',0ah,0ah,'rohan.a.suri@gmail.com',0ah,'ravishthalesar94@gmail.com',0ah,'shahrushabh2808@gmail.com',0Ah,0ah,'To return to main screen, press RETURN.',0
	commands db 'Commands supported: time, date, exit',0
	time db 'time',0
	date db 'date',0
	exit db 'exit',0
    buffer: resb 64
	notrecog db 0dh,0ah,'Command not recognised',0
	videomode db 0
	cols db 0
	select db 'Select program using up/down arrow keys:',0
	mini db 5
	maxi db 8


getcursor:
    mov ah,03h
   ;page no
    int 10h
	ret
hide_cursor:
	mov ch, 32
	mov ah, 1
	mov al, 3
	int 10h
	ret
move_cursor:
	mov ah,02h
		;mov bh,0;page no
	int 10h
	ret
header:
	mov dl, 0;column
	mov dh, 0;row
		mov bh,0
	call move_cursor

	mov ah, 09h	
	mov cx,80			
	mov bl,0b0h;F0 white E0h yellow B0 light blue	0A looked cool	40h red	23h dark green 10h dark blue
	mov bh, 0			
	mov al,' '
	int 10h
	
	mov dl,35;col
	mov dh,00;row
		mov bh,0
	call move_cursor
	
	mov bp,osname
	mov cx,[osnamelen]
	mov bl,0b0h
	call printstr
	
	ret
drawbox:
	.rep:
		mov bh,0
    call move_cursor
	mov ah, 09h			; Draw single bar
	mov bh, 0
	mov cx, si
	mov al, ' '
	int 10h

	inc dh				; next row
	mov ax,0
	mov al,dh
	cmp ax, di			;end row reached?
	jne .rep
	
	ret
drawpointer:
	mov si,point
	mov dl,22
		mov bh,0
	call move_cursor
	call printstr_tel
	ret
clearpointer:
	mov si,empty
	mov dl,22
		mov bh,0
	call move_cursor
	call printstr_tel
	ret
wait_for_key:
	mov ah,10h
	int 16h
	ret
keyboard:
	;get keystroke
	call wait_for_key
	.up:
		cmp ah,48h ;up 48, down 50
		jnz .down
		;go up ,check if mini
		mov bh,0
		call getcursor
		cmp dh,[mini]
		je keyboard
		;if not move up
		call clearpointer
		dec dh
		call drawpointer
		jmp keyboard
	.down:
		cmp ah,50h
		jnz .enter
		mov bh,0
		call getcursor
		cmp dh,[maxi]
		je keyboard
		call clearpointer
		inc dh
		call drawpointer
		jmp keyboard
	.enter
		cmp al,13 ; \n 
		jnz .rep
		;switch page or try refreshing
		mov bh,0
		call getcursor
		mov al,[mini] ;command line
		cmp dh,al
		jne .op2
		call commandline
		jmp .rep ;only for now!!
		.op2:
		inc al
		cmp dh,al
		jne .op3 
		call shutdown
		jmp .rep ; only for now!
		.op3:
		inc al
		cmp dh,al
		jne .op4
		call restartproc
		jmp .rep ;only for now!
		.op4:
		inc al
		cmp dh,al
		jne .rep 
		call abtus
		jmp .rep ;only for now
	.rep:
		jmp keyboard
restartproc:
	mov        al, 0feh  ; output port 8042 keyboard controller
	out        64h, al
	ret
abtus:
	;new page
	mov ah,05h
	mov al,01h
	int 10h
	
	mov bh,01h
	mov dh,00h
	mov dl,00h
	call move_cursor
	
	mov cx,1800;should be no of rows*no of columns in current video mode!
	mov bh,01h
	mov ah,0eh
	mov al,' '
	.cls:
	int 10h
	dec cx
	jnz .cls
	
	;start para
	mov dh,08
	mov dl,16
	mov bh,1
	call move_cursor

	mov si,aboutus
	.rep:
	mov ah,0eh
	lodsb
	cmp al,0
	je .done
	cmp al,0ah
	jnz .here

	;new line
	mov bh,1
	call getcursor
	inc dh
	mov dl,16
	mov bh,1
	call move_cursor
	
	.here:
	mov bh,01h
	int 10h
	mov ah,86h
	mov dx,0A000h
	mov cx,0000h
	int 15h
	jmp .rep
	.done:
	call wait_for_key
	cmp al,13
	jnz .done
	mov ax,0500h
	int 10h
	ret
printchar:
	;count in cx
	;pg no in bh
	mov ah,0eh
	;al contains char
	int 10h
	ret
commandline:
	mov ah,05h
	mov al,01h
	int 10h
	
	mov bh,01h
	mov dh,00h
	mov dl,00h
	call move_cursor
	
	mov cx,1950;should be no of rows*no of columns in current video mode!
	mov bh,01h
	mov ah,0eh
	mov al,' '
	.cls:
	int 10h
	dec cx
	jnz .cls
	
	mov dh,00
	mov dl,00
	call move_cursor
	mov si,commands
	call printstr_tel
	;move to next row
	.repOuter:
	mov bh,1
	call getcursor
	inc dh;next row
	mov dl,00
	call move_cursor
	mov si,point
	call printstr_tel
	mov di,buffer
	mov bp,0000h;use BP as char counter
	
	.rep:
	call wait_for_key
	cmp al,13 ;enter key
	jz .comp;start string compare
	cmp al,08 ;backspace
	jnz .store;not bs then store the char
	mov bh,1
	call getcursor
	dec dl
	cmp dl,0;at pos 0
	je .rep;> pos, skip
	call move_cursor
	dec bp
	dec di
	mov bh,1
	mov ah,0ah
	mov al,''
	mov cx,1
	int 10h
	jmp .rep
	
	
	.store:
	;mov byte [si],al
	;inc si
	stosb
	inc bp
	;print char
	mov ah,0ah
	mov bh,1
	mov cx,1
	int 10h
	call getcursor
	inc dl
	call move_cursor
	jmp .rep
	
	
	.comp:
	;start string compare
	cmp bp,0000h
	je .repOuter;empty command enter
	cmp bp,0004h
	jne .none
	mov cx,bp
	mov di,buffer
	mov si,exit
	repe cmpsb
	cmp cx,0000h
	jz  .eexit;we compare using zero flag! ie if last comparison resulted in setting of zero flag
	;very imp!! dont use cmp cx,0 problems when comparing strings of len1.
	mov cx,bp
	mov di,buffer
	mov si,date
	repe cmpsb
	cmp cx,0000h
	jz .date
	mov cx,bp
	mov di,buffer
	mov si,time
	repe cmpsb
	cmp cx,0000h
	jz .time
	.none:
	mov si,notrecog
	call printstr_tel
	jmp .repOuter
	
		.eexit:
	mov ah,05h;return to page 0
	mov al,00h
	int 10h
	ret
	
	.time:
	mov ah,02h
	int 1ah
	mov di,buffer
	mov al,0ah
	stosb
	mov al,0dh
	stosb
	mov ah,ch
	call .split
	mov al,':'
	stosb
	mov ah,cl
	call .split
	mov al,0
	stosb
	mov si,buffer
	call printstr_tel
	
	jmp .repOuter
	
	.date:
	
	;get date RTC
	mov ah,04h
	int 1ah
	mov di,buffer
	;new line
	mov al,0ah
	stosb
	mov al,0dh
	stosb
	;dl day,dh month,cl year,ch century(19or20)
	mov ah, dl
	call .split
	mov al, '/'
	stosb	
	
	mov ah, dh	
	call .split
	mov al, '/'
	stosb
	
	mov ah, ch
	call .split ;split 19 as 1   9 then we add + "0" to get ascii equivalent
	mov ah, cl
	call .split
	;terminal char
	mov ax,0
	stosb
	
	mov si,buffer
	call printstr_tel
	jmp .repOuter
	
	.split:
	mov al, ah
	shr al, 4;first digit (upper nibble)
	call .toascii
	mov al, ah
	and al, 0Fh;second digit(lower nibble)
	call .toascii
	ret

	.toascii:
	add al, '0'
	stosb		
	ret
	
	
shutdown:
	;Connect to APM API, real mode interface
	mov ax,5301h
	xor bx,bx
	int 15h

	;Try to set APM version (to 1.2)
	mov ax,5303h
	xor bx,bx
	mov cx,0102h
	int 15h
	jc .return ;failure
	;set power state
	mov ax,5307h
	mov bx,0001h;all devices for which bios manages power
	mov cx,0003h;off
	int 15h
	.return:
	ret
	
printstr_tel:
	mov ah,0eh
	.rep:
	lodsb
	cmp al,0
	je .done
	mov bh,0
	int 10h
	jmp .rep
	.done
		ret
printstr:
	mov ah, 13h 
	mov bh, 0	;page number
	;mov cx, 16	;string length
	;mov bl, 0eh	;color
	mov al, 01h	;sub service
	int 10h
	
	ret
	
draw:
	;set cursor pos
	mov dh,18;row
	mov dl,30;col
		mov bh,0
	call move_cursor
	
	mov bp,bootmsg
	mov cx,[bootmsglen]
	mov bl, 0eh	;yellow color 0eh
	;write character string
	call printstr

	
	mov si,0004h ;timer count
     mov dx,1432h
	.timer:
	mov ah,86h
	mov dx,4240h  ;8480 for 2 secs
	mov cx,000fh ;001e
	int 15h
    
    mov ah, 09h	
	mov cx,1			
	mov bl,0eh			
	mov bh, 0			
	mov al, '.'
	int 10h
	
	;get cursor pos
	mov ah,03h
	mov bh,0
	int 10h
	;set cursor to next column
	inc dl
		mov bh,0
	call move_cursor
	dec si
	jnz .timer
	
	ret
	
clearscreen:
	;switching to original video mode :p clears screen
	mov ah, 00h
	mov al, [videomode]
	int 10h
	call hide_cursor
	ret	

loadprograms: ; should use loop here
	mov dh,5
	mov dl,25
		mov bh,0
	call move_cursor
	mov si,cmdline
	call printstr_tel
	inc dh
		mov bh,0
	call move_cursor
	mov si,shut
	call printstr_tel
	inc dh
		mov bh,0
	call move_cursor
	mov si,restart
	call printstr_tel
	inc dh
		mov bh,0
	call move_cursor
	mov si,about
	call printstr_tel
	
	ret	

