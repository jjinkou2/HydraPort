''*************************************
''*  NES PPU emulator driver v1.0     *
''*************************************
'
' Copyright (C) 2009 Darryl Biggar
' Version of 11 January 2009
'
' Simulates the NES PPU. This version only simulates horizontal mirroring and vertical scrolling.
'
'
' The parameters to be passed to this driver must be in the following order:
' word  address of two pattern tables, each 4KB long
' word  address of two name tables and attribute tables, each is 1 KB long
' word  address of 32-byte pallette (must be on a long boundary)
' word  address of 256 byte sprite data memory (OAM) (must be on a long boundary)
' byte  cog number for this cog
' byte  total number of cogs
' byte  PPUctrl
' byte  PPUmask
' byte  PPU x-offset
' byte  PPU y-offset
' byte  PPUstatus
'
' PPUctrl ($2000) (this driver only reads this)
' 76543210
' ||||||||
' ||||||++- Base nametable address
' ||||||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
' |||||+--- VRAM address increment per CPU read/write of PPUDATA
' |||||     (0: increment by 1, going across; 1: increment by 32, going down)
' ||||+---- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000)
' |||+----- Background pattern table address (0: $0000; 1: $1000)
' ||+------ Sprite size (0: 8x8; 1: 8x16)
' |+------- PPU master/slave select (has no effect on the NES)
' +-------- Generate an NMI at the start of the
'           vertical blanking interval (0: off; 1: on)
'
' PPUmask ($2001) (this driver only reads this)
' 76543210
' ||||||||
' |||||||+- Grayscale (0: normal color; 1: AND all palette entries
' |||||||   with 0x30, effectively producing a monochrome display;
' |||||||   note that colour emphasis STILL works when this is on!)
' ||||||+-- Enable background in leftmost 8 pixels of screen (0: clip; 1: display)
' |||||+--- Enable sprite in leftmost 8 pixels of screen (0: clip; 1: display)
' ||||+---- Enable background rendering
' |||+----- Enable sprite rendering
' ||+------ Intensify reds (and darken other colors)
' |+------- Intensify greens (and darken other colors)
' +-------- Intensify blues (and darken other colors)
'
' PPUstatus ($2002) (driver writes this - main program must clear)
'
' 76543210
' ||||||||
' |||+++++- Unimplemented
' ||+------ Sprite overflow. The PPU can handle only eight sprites on one
' ||        scanline and sets this bit if it starts dropping sprites.
' ||        Normally, this triggers when there are 9 sprites on a scanline,
' ||        but the actual behavior is significantly more complicated.
' |+------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 'hits'
' |         a nonzero background pixel.  Used for raster timing.
' +-------- Vertical blank has started (0: not in VBLANK; 1: in VBLANK)
'
' Each 4-byte block for each sprite is organised as follows
' byte 0 - y-position of sprite (minus one) Values in the range 239-255 result in sprite no being displayed.
' byte 1 - tile index number. For 8x16 sprites, bit 0 is bank number, bits 7-1 are tile number for top half of sprite
' byte 2 - sprite attributes
' byte 3 - x position of sprite.
'
' Sprite attributes:
' 76543210
' ||||||||
' ||||||++- Palette (4 to 7) of sprite
' |||+++--- Unimplemented, reads back as 0
' ||+------ Priority (0: in front of background; 1: behind background)
' |+------- Flip sprite horizontally
' +-------- Flip sprite vertically
'

CON

  SCANLINE_BUFFER = $7F00
  num_vert_scanlines = 240  

PUB start_of_PPU_driver 
RETURN @_ppustart

PUB start(paramadr)

'' Start REM engine - starts a cog
  cognew(@entry, paramadr)

DAT

_ppustart
                        org

' Entry
'
entry                   mov     dira, #1 ' enable debug led port
                        ' fetch some parameter, starting at 'par':
                        mov     t2,par
                        rdlong  cognumber, t2 ' store cog number (0,1,...)

'                        mov     t2,par
                        add     t2, #4
                        rdlong  cogtotal, t2 ' store total number of cogs (1,2,...)

                        ' Syncronise all gfx cog with TV driver before starting
                        call    #waitsyncro_start
'                        mov     outa,#1
                                                
main_loop
                        call    #load_internal_buffers
                        call    #waitrequest_start


prepare_scanline
                        ' Take the currentscanline and add the vertical_scroll
                        mov     y_offset, currentscanline
                        sub     y_offset,#1
                        rdbyte  t1, PPUyoffset
                        add     y_offset, t1
                        cmp     y_offset,#num_vert_scanlines         wc,wz
        if_ae           add     y_offset,#16
'                        shl     t1,#4

                        rdbyte  x_offset, PPUxoffset
'                        mov     x_offset,#0
'                        mov     sprite_a_offset, x_offset
{                        rdlong  t2, horizontal_scroll
                        or      t1,t2
                        mov     x_offset,#0}
{                        rdlong  t1,gamepad
                        or      t1,#$8
                        shl     t1,#8
                        mov     t2,internal_pallette
                        andn    t2,mask_pixel2
                        or      t2,t1
                        mov     internal_pallette,t2}
              
                                                                     
                        ' safety check: if trying to draw a line off the tilemap, then output a black scanline
 '                       cmp y_offset, max_tile_y wc, wz
 '       if_a            jmp #blacktiles

                        mov     tilemap_offset, #0
                        mov     t1, y_offset
                        shr     t1, #3
                        shl     t1, #5
                        add     tilemap_offset, t1
                        mov     t1,PPUctrl
                        and     t1,#2           
                        shl     t1,#9           '  $000 or $400 maps to $000, $800 or $C00 maps to $400
                        add     tilemap_offset,t1
{                         and     t1,#3
                        shl     t1,#10          ' to give $000, $400, $800, $C00,                         
                        add     tilemap_offset,t1
                        test    tilemap_offset,H0400 wz               
        if_nz           add     tilemap_offset,H0400}
                        and     tilemap_offset,H07FF                                   ' ensure wrap-around}

{                        mov     att_table_offset,y_offset
                        shr     att_table_offset,#5
                        shl     att_table_offset,#3
                        and     att_table_offset,#63
                        add     att_table_offset,attributetable}
                                                
                        mov     t1,tilemap_offset
                        mov     att_table_offset,tilemap_offset
                        shr     att_table_offset,#4
                        and     att_table_offset,#$78
                        shr     t1,#2
                        and     t1,#$07
                        add     att_table_offset,t1
                        test    att_table_offset,#$40 wz
                        and     att_table_offset,#$3F
              if_nz     add     att_table_offset,H0400
                        add     att_table_offset,ninesixty
                        add     att_table_offset,tilemap                        
                        
                        mov     att_table_bitshift,#0
                        test    tilemap_offset,#$40 wz
              if_nz     add     att_table_bitshift,#4

{                        mov     att_table_offset,tilemap_offset
                        shr     att_table_offset,#6    ' divide by 64
                        shl     att_table_offset,#2
                        test    att_table_offset,$40 wz
                        and     att_table_offset,#$3F
        if_nz           add     att_table_offset,H0400
                        add     att_table_offset,ninesixty
                        add     att_table_offset,attributetable}


                        add     tilemap_offset, tilemap

                        ' reset pointer to start of scanbuffer
                        mov     t1,#scanbuffer_prev
                        test    x_offset,#4     wz
        if_z            add     t1,#1
                        movd    copyfourpixel, t1

                        ' number of tiles to output = 32
                        mov     l1, #33             ' 1 extra for horizontal scroll

                        ' Calculate things that won't change during a scanline:
                        mov lshift, x_offset
                        and lshift, #3
                        mov t1, lshift wz
        if_nz           mov shift_rpixel, shift_rpixel_instruction ' set self-modify code
        if_z            mov shift_rpixel, skip_rpixel_instruction ' to either shift or move
                        ' I have to do this because shr and shl can't shift by 32 bit :(
                        shl lshift, #3

                        mov rshift, #4
                        sub rshift, t1
                        shl rshift, #3

                        mov     y_tile_offset, y_offset
                        and     y_tile_offset, #7
                        add     y_tile_offset, tiles

{                        mov     att_table_offset,y_offset
                        shr     att_table_offset,#5
                        shl     att_table_offset,#3
                        add     att_table_offset,attributetable}

nexttile

' Fetch tile (32-bit) pixel into 'tilepixel'
fetchtile

' Compute attribute table offset to get high 2 bits of pallette table
'
                        ' Now add x offset / 8 : this will give us a full tile offset
                        mov     t1, x_offset
                        shr     t1, #3
                        and     t1, #31
                        mov     t2,t1
                        shr     t2, #2
                        add     t2,att_table_offset
                        mov     t5,att_table_bitshift
                        test    t1,#$02 wz
              if_nz     add     t5,#2
              
'                        mov     t2,att_table_offset
                        rdbyte  t6,t2
'                        mov     t6,#$00 ' #$AA ' ' #$00 ' #$FF ' #$55
                        shr     t6,t5
                        and     t6,#3
                        add     t6,#internal_pallette
                        movs    code_get_pallette,t6
                        mov     t2, tilemap_offset                     ' these instructions         
                        add     t2, t1                                 ' are here to give movs
                        mov     t1, y_tile_offset                      ' time to operate
code_get_pallette       mov     t6,internal_pallette                   ' t6=pallette colors
'                        mov     t6,internal_pallette+1
                                                                                              

                        ' Fetch the current tile in t2
                        rdbyte  t2, t2
                        shl     t2, #4              ' Each tile is 16 bytes
                        add     t2, t1


                        rdbyte  t3, t2           ' Pixel read into t1
                        shl     t3,#24
                        add     t2,#8
                        rdbyte  t4, t2
                        shl     t4,#24

                        mov     l3,#2
nextfourbits
                        mov     l2,#4
                        mov    tilepixel,#0
nextbit
                        mov     t5,t6
                        shl     t4,#1           wc
              if_nc     shl     t5,#16
                        shl     t3,#1           wc
              if_nc     shl     t5,#8
                        and     t5,mask_pixel4


                        shr     tilepixel,#8
                        or      tilepixel,t5
                        djnz    l2, #nextbit
                                                
                        mov     t1,tilepixel
                        
shift_rpixel            shl     tilepixel, rshift
                        or      tilepixel, nexttilepixel
                        
                        mov     nexttilepixel, t1
                        shr     nexttilepixel, lshift
                        
'                        mov     tilepixel,white ' debugging!
copyfourpixel           
                        mov scanbuffer, tilepixel
                        add copyfourpixel, destination_increment

                        djnz    l3,#nextfourbits

                        ' advance to next tile
                        add     x_offset, #8
                        and     x_offset, #$FF
                        djnz    l1, #nexttile

tilesfinished
'
' sprite processing
'
startsprite
                        mov     num_visible_sprites,#0
drawspriteproc
                        movs    code_sprite_ptr,#internal_sprite_buffer   ' +63
                        mov     l1,#64          ' number of sprites

nextsprite

code_sprite_ptr         mov     t8,internal_sprite_buffer  ' read in 4 bytes of sprite data
                        add     code_sprite_ptr,#1
'                        add     code_sprite_ptr1,#1

                        mov     t2,currentscanline

                        mov     t3,t8
                        add     t3,#1
                        and     t3,#$FF         ' isolate y_offset for sprite
'                        add     t3,sprite_a_offset
'                        cmp     t3,#$FF wz
'        if_z            jmp     #endspriteloop                        
                        sub     t2,t3           wc
        if_c            jmp     #endspriteloop   ' if t2<current_scanline skip sprite
                        cmp     t2,spritesize          wc,wz
        if_a            jmp     #endspriteloop
'                        cmp     t2,#8           wc
'        if_nc           jmp     #endspriteloop   ' if t2-current_scanline>8 skip sprite
'        if_nc           add     t8,#$100         ' add 1 to the tile for the bottom half of an 8x16 tile
        
                        add     num_visible_sprites,#1  ' sprite is visible
                        cmp     num_visible_sprites,#16  wc
        if_nc           jmp     #sprite_overflow

                        test    t8,flip_vert_bit        wz
'        if_nz           xor     t2,#7
         if_nz          mov     t3,spritesize
         if_nz          sub     t3,t2
         if_z           mov     t3,t2
                        cmp     t3,#8           wc
                        and     t3,#7

                        mov     y_tile_offset,t3
                        add     y_tile_offset, spritetiles
        if_nc           add     y_tile_offset,#16       ' if bottom half of 8x16 go to next tile in tilemap
        
'code_sprite_ptr1         mov     t2,internal_sprite_buffer  ' read in 4 bytes of sprite data
                        mov     t2,t8
                        shr     t2,#16          ' get byte 2 - attribute data for sprite
                        and     t2,#3           ' isolate index bits for pallette
'                        mov     t2,#1       ' used for debugging
                        add     t2,#(internal_pallette+4)
                        movs    code_get_spr_pallette,t2
                        mov     t2,t8
                        shr     t2,#24
                        and     t2,#$FF         ' isolate x_offset for sprite
code_get_spr_pallette   mov     t7,internal_pallette    ' t6 contains pallette colors

'                        mov     t6,white       ' used for debugging
'                        add     t2,sprite_a_offset
'                        mov     t2,#0
                        
                        mov     rshift,t2
                        and     rshift,#3       wz      ' lower 2 bits determine shift within 4-pixel frame
        if_nz           mov     shift_srpixel1, shift_rpixel_instruction
        if_z            mov     shift_srpixel1, skip_rpixel_instruction
        if_nz           mov     shift_srpixel2, shift_rpixel_instruction1
        if_z            mov     shift_srpixel2, skip_rpixel_instruction1
                        mov     t3,rshift
                        shl     rshift,#3
                        mov     lshift,#4
                        sub     lshift,t3
                        and     lshift,#3
                        shl     lshift,#3
                                                
                        shr     t2,#2           ' t2 is pointer to starting long in scanbuffer
                        add     t2,#scanbuffer
        if_z            sub     t2,#1
'                        mov     t2,#scanbuffer

                        movd    scopyfourpixel1,t2
                        movd    scopyfourpixel2,t2
                        movd    scopyfourpixel3,t2
                        movd    scopyfourpixel4,t2
                        
                        mov     t2,t8
                        and     t2,mask_pixel2  ' isolate the tile index for sprite
                        shr     t2,#4           ' divide by 16 to find location in tilemap
                        test    spritesize,#8 wz
        if_z            jmp     #:skip
                        test    t2,#$10            wz
                        andn    t2,#$10
        if_nz           add     t2,H1000
:skip                   add     t2,y_tile_offset

                        rdbyte  t3,t2
                        test    t8,flip_horiz_bit       wz
                        add     t2,#8
                        rdbyte  t4,t2
              if_nz      rev     t3,#24
              if_nz      rev     t4,#24

                        shl     t3,#24
                        shl     t4,#24

                        mov     nexttilepixel,#0
                        mov     nextmask,#0
                        
                        mov     l3,#2
nextfoursbits
                        mov     l2,#4
                        mov     tilepixel,#0
                        mov     mask,#0
nextsbit
                        shr     mask,#8
                        mov     t2,t4
                        or      t2,t3
                        shl     t2,#1           wc
              if_c      or      mask,mask_pixel4        ' if non-zero set mask
              
                        mov     t5,t7           ' make a copy of pallette colors
                        shl     t4,#1           wc
              if_nc     shl     t5,#16
                        shl     t3,#1           wc
              if_nc     shl     t5,#8
                        and     t5,mask_pixel4

                        shr     tilepixel,#8
                        or      tilepixel,t5
                        djnz    l2, #nextsbit
                                                
                        and     tilepixel,mask
                        mov     t1,tilepixel
                        mov     t2,mask
                        
shift_srpixel1          shl     tilepixel, rshift
                        or      tilepixel, nexttilepixel
shift_srpixel2          shl     mask,rshift
                        or      mask,nextmask
                        
                        mov     nexttilepixel, t1
                        shr     nexttilepixel, lshift
                        mov     nextmask,t2
                        shr     nextmask, lshift
                        
scopyfourpixel1         andn    scanbuffer,mask
scopyfourpixel2         or      scanbuffer,tilepixel

                        add     scopyfourpixel1, destination_increment
                        add     scopyfourpixel2, destination_increment
                        add     scopyfourpixel3, destination_increment
                        add     scopyfourpixel4, destination_increment

                        djnz    l3,#nextfoursbits

scopyfourpixel3         andn    scanbuffer,nextmask
scopyfourpixel4         or      scanbuffer,nexttilepixel

endspriteloop
'                        call    #checktv
                        rdword currentrequest, request_scanline
                        cmp currentrequest, currentscanline wz, wc
        if_ae           jmp #start_tv_copy

                        djnz    l1, #nextsprite


scanlinefinished        ' Check status of TV: warning: this is a pseudo-call: it might not return!
                        call #checktv           

:wait                   ' Wait here until the TV request exactly the scanline that THIS cog prepared
                        ' Other cog will wait here a bit
                        rdword currentrequest, request_scanline
                        cmps currentrequest, currentscanline wz, wc
        if_b            jmp #:wait

start_tv_copy
                        movs :nextcopy, #scanbuffer
                        mov t1, display_base
                        mov l1, #64

:nextcopy               mov t3,scanbuffer
                        add :nextcopy, #1
                        wrlong t3, t1
                        add t1, #4
                        djnz l1, #:nextcopy
                        
scanlinedone            
                        ' Line is done, increment to the next one this cog will handle
                        
                        add currentscanline, cogtotal
                        cmp currentscanline, #239 wc, wz
        if_be           jmp #prepare_scanline
                        
                        ' The screen is completed, jump back to main loop a wait for next frame
                        jmp #main_loop


'
' More than 8 sprites
'
sprite_overflow         rdbyte  t1,ppustatus
                        or      t1,#$20
                        wrbyte  t1,ppustatus
                        jmp     #scanlinefinished


' Instruction that will get copied over at line 'shift_rpixel'
shift_rpixel_instruction shl tilepixel, rshift
skip_rpixel_instruction mov tilepixel, #0
shift_rpixel_instruction1 shl mask, rshift
skip_rpixel_instruction1 mov mask, #0

spriteoutput_instruction or scanbuffer, tilepixel
shadowoutput_instruction nop

' Verification: if the TV is already asking for this scanline or more
' (except scanline 0), then lit the debug led: we have failed preparing this
' scanline in time: we need to optimize or use more cog
' if we failed, then skip straight to TV output: This means that WE WONT RETURN TO THE CALLER.
checktv
                        cmp currentrequest, #0 wz
        if_z            jmp #checktv_ret
                        rdword currentrequest, request_scanline
                        cmp currentrequest, currentscanline wz, wc
                        ' At this point, we could skip drawing sprites or something like that and go
                        ' straight to image output and still stay synced with the tv driver
                        ' We must be quick though!
        if_b            mov outa, #0
        if_ae           mov outa, #1
        if_ae           jmp #start_tv_copy
checktv_ret             ret
        

' Output all black when drawing off-map stuff
{blacktiles
                        call #debugclear
                        jmp #scanlinefinished

' Helper proc to clear the scanbuffer
debugclear
                        movd :debugclear, #scanbuffer
                        mov l1, #64
:debugclear             mov scanbuffer, #0
                        add :debugclear, destination_increment
                        djnz l1, #:debugclear
debugclear_ret          ret}

' Wait until the tv driver request scanline 0
waitrequest_start
                        mov currentscanline, cognumber
:waitloop               rdword currentrequest, request_scanline wz
        if_nz           jmp #:waitloop
'                        cmp     cognumber,#0 wz
'        if_nz           jmp     #:waitloop
waitrequest_start_ret   ret

' Wait until the tv driver request last scanline
waitsyncro_start
:waitloop               rdword currentrequest, request_scanline
                        cmp currentrequest, #num_vert_scanlines wz
        if_ne           jmp #:waitloop
waitsyncro_start_ret    ret

' Add scroll offset

load_internal_buffers


                        rdword  tilemap, tilemap_adr
                        rdword  t1, tiles_adr
                        rdword  pallette, pallettes_adr
                        rdword  sprites, sprites_adr

                        rdbyte  PPUctrl, PPUctrl_adr
                        rdbyte  PPUmask, PPUmask_adr

{                        mov     t2,PPUctrl
                        and     t2,#3
                        shl     t2,#10          ' to give $000, $400, $800, $C00,                         
                        add     tilemap,t2}

                        mov     tiles,t1
                        test    PPUctrl,#$10 wz
             if_nz      add     tiles,H1000
'                        mov     t2,PPUctrl
'                        and     t2,#$10
'                        shl     t2,#8           ' to give $0000 or $1000
'                        mov     tiles,t2
'                        add     tiles,t1
                        
                        mov     spritetiles,t1
                        mov     spritesize,#15
                        test    PPUctrl,#$20 wz
              if_nz     jmp     #:skip
                        mov     spritesize,#7
                        test    PPUctrl,#$8 wz
              if_nz     add     spritetiles,H1000
'                         mov     t2,PPUctrl
'                        and     t2,#$8
'                        shl     t2,#9           ' to give $0000 or $1000
'                        mov     spritetiles,#0  ' t2
'                        add     spritetiles,t1
:skip
                        mov     attributetable,tilemap
                        add     attributetable,ninesixty
' load pallette information
                        movd    code_pallette_load,#internal_pallette
                        mov     l1,#8           ' 8 longs of pallette data
                        mov     t2,pallette
pallette_load_loop
code_pallette_load      rdlong  internal_pallette,t2
                        add     t2,#4
                        add     code_pallette_load,destination_increment
                        djnz    l1,#pallette_load_loop
'                        rdlong  internal_pallette,pallette
'                        wrlong  (internal_pallette),#$C

' load sprite information
                        movd    code_sprite_load,#internal_sprite_buffer
                        mov     l1,#64           ' 64 longs of sprite data
                        mov     t2,sprites
sprite_load_loop
code_sprite_load        rdlong  internal_sprite_buffer,t2
                        add     t2,#4
                        add     code_sprite_load,destination_increment
                        djnz    l1,#sprite_load_loop
'                        rdlong  internal_sprite_buffer,sprites
load_internal_buffers_ret
                        ret




PPUctrl_adr             long    SCANLINE_BUFFER-16
PPUmask_adr             long    SCANLINE_BUFFER-15
PPUstatus               long    SCANLINE_BUFFER-14
' OAM address is not used in this COG
PPUxoffset              long    SCANLINE_BUFFER-11
PPUyoffset              long    SCANLINE_BUFFER-12
sprites_adr             long    SCANLINE_BUFFER-10
tilemap_adr             long    SCANLINE_BUFFER-8       ' Nametable
pallettes_adr           long    SCANLINE_BUFFER-6
tiles_adr               long    SCANLINE_BUFFER-4       ' Patterntable
request_scanline        long    SCANLINE_BUFFER-2
display_base            long    SCANLINE_BUFFER

' t1-8: temporary registers
t1 long                 $0
t2 long                 $0
t3 long                 $0
t4 long                 $0
t5 long                 $0
t6 long                 $0
t7 long                 $0
t8 long                 $0
' l1-5: loop registers
l1 long                 $0
l2 long                 $0
l3 long                 $0
'l4 long                 $0
'l5 long                 $0
' p1-7: parameter registers
{p1 long                 $0
p2 long                 $0
p3 long                 $0
p4 long                 $0}
'p5 long                 $0
'p6 long                 $0
'p7 long                 $0

'empty                   long    $0
'full                    long    $ffffffff
'black                   long    $0                      ' was $02020202 but now TV adds black
white                   long    $05050505               ' was $07070707 but now TV adds black
destination_increment   long    512
H0BFF                   long    $0BFF
H07FF                   long    $07FF
H0400                   long    $0400
H0800                   long    $0800
H1000                   long    $1000

'time long               $0
'period long             220000
'framecount long         $0
tilemap                 long            $0
tiles                   long              $0
spritetiles             long    $0
pallette                long    $0
sprites                 long    $0
cognumber               long          $0
currentscanline         long    $0
currentrequest          long     $0
cogtotal                long           $0
'debug_v1 long           $0
'debug_v2 long           $0
'wasteclock long         400
'mask_pixel1             long        $000000FF
mask_pixel2             long        $0000FF00
'mask_pixel3             long        $00FF0000
mask_pixel4             long        $FF000000
'patterntablelength      long    $1000           ' length of a pattern table
ninesixty               long    960
'onekilobyte             long    $400
attributetable          long    0
flip_vert_bit           long    $800000
flip_horiz_bit          long    $400000
PPUctrl                 long    $0
PPUmask                 long    $0
spritesize              long    0
tilemap_offset          long     $0
att_table_offset        long    $0
att_table_bitshift      long    $0
tilepixel               long          $0
nexttilepixel           long      $0
mask                    long    $0
nextmask                long    $0
num_visible_sprites     long    $0
lshift                  long             $0
rshift                  long             $0
y_offset                long           $0
x_offset                long           $0
y_tile_offset           long      $0
'max_tile_y long         240

internal_pallette       long    $19191900     ' background pallette colors
                        long    $19191900
                        long    $19191900
                        long    $19191900
                        long    $19191900 ' $9905053A ' $99BAC93A
                        long    $19191900
                        long    $19191900
                        long    $19191900

DAT

internal_sprite_buffer  res     64

{                        long    $000000F8, $444001B0, $4C4000B0, $44404DB8, $4C404CB8
                        long    $44004AC0, $4C404AC0, $44004BC8, $4C404BC8

                        long    $4640FC6A, $4E40FC6A, $46400172
                        long    $4E400072, $46404D7A, $4E404C7A, $46004E82
                        long    $4E404E82, $000000F8, $3A0276F8, $420277F8
                        long    $3C02F65D, $4402505D, $5823FF18}

scanbuffer_prev         res     2                       ' Two extra longs on the left
scanbuffer              res     66                      ' Two extra longs on the right

                        fit     $1F0
                        