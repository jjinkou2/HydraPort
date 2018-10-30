{*************************************
 *   Hydra NES Emulator Version  1.2 *
 *************************************

Copyright (C) 2010 Darryl Biggar

Version of 13 March 2010

Changes from the previous version:
13 March 2010 - added sound driver; single spin file for all the ROM data; constants added to spin
                ROM data file.

}

CON
  _clkmode  = xtal1 + pll16x
  _xinfreq  = 5_000_000
  _free = 160

  SCANLINE_BUFFER = $7F00

  spin_ppuctrl_adr = SCANLINE_BUFFER-16                 ' $7EF0
  spin_ppumask_adr = SCANLINE_BUFFER-15                 ' $7EF1
  spin_ppustatus_adr=SCANLINE_BUFFER-14                 ' $7EF2
  spin_OAMaddr_adr = SCANLINE_BUFFER-13                 ' $7EF3
  spin_horizontal_scroll = SCANLINE_BUFFER-11           ' $7EF5
  spin_vertical_scroll = SCANLINE_BUFFER-12             ' $7EF4
  spin_spritetable_adr = SCANLINE_BUFFER-10             ' $7EF6
  spin_nametables_adr = SCANLINE_BUFFER-8               ' $7EF8  
  spin_pallettes_adr = SCANLINE_BUFFER-6                ' $7EFA
  spin_patterntables_adr = SCANLINE_BUFFER-4            ' $7EFC
  spin_request_scanline = SCANLINE_BUFFER-2             ' $7EFE
  spin_nmi_request_adr = SCANLINE_BUFFER-17             ' $7EEF
  spin_apu_dma_adr = SCANLINE_BUFFER-36                 ' $7EDC
  spin_apu_status_adr = SCANLINE_BUFFER-18              ' $7EEE

' Gamepad constant declaration
  NES_RIGHT  = %00000001
  NES_LEFT   = %00000010
  NES_DOWN   = %00000100
  NES_UP     = %00001000
  NES_START  = %00010000
  NES_SELECT = %00100000
  NES_B      = %01000000
  NES_A      = %10000000

  #0, COMMAND_NONE, COMMAND_READ, COMMAND_WRITE

  debug_mode = false
  
VAR

  long tv_status      '0/1/2 = off/visible/invisible           read-only

  long  emu_mode, emu_status, reg6502_acc, reg6502_xreg, reg6502_yreg, reg6502_status
  long  reg6502_stackptr, reg6502_pc, memory_offset, memory_start
  
  long  nt,ms,dnt           ' name table to use for display
  long  button_pressed,bp_count
  long  letter_offset,number_offset,dash_char
  long  sprite_data[64]
  
  long  varaddress,eeprom_cog
                                                                                                         
OBJ


  emu   : "NES_emulator_core_010.spin"
  apu   : "NES_APU_driver_001.spin"
  eeprom: "NS_eeprom_drv_010.spin"
  chooser: "NES_Game_Chooser.spin"


PUB start | ptr, EEPROM_address, ROM_length, temp1, temp2, rd, Disp_ctr

  eeprom.start(28,29,0)
  varaddress := eeprom.variables
  eeprom_cog := long[varaddress-4]
  ptr := chooser.Select_Game(varaddress)

  ms := long[ptr]
  EEPROM_address := long[ptr+4]
  ROM_length := long[ms]
  nt := ms + $0800
  dnt := nt
{  letter_offset := byte[ms+18]
  number_offset := byte[ms+19]
  dash_char     := byte[ms+22]
  Show_NES_RAM_Content($0,ms)    ' Shows zero page of NES RAM
  repeat}
'  ms := emu.start_of_emulator
'  long[ms] := ms
'  long[ms+4] := EEPROM_address
'  long[ms+8] := ROM_length
  
'  Hex($348,0,ms,6)
  

' emu_mode = 0 - do nothing
' emu_mode = 1 - step-by-step mode
' emu_mode = 2 - run full speed
' emu_mode = 3 - run continuously, writing out each step

'  Show_NES_RAM_Content($0,ms)    ' Shows zero page of NES RAM
'  repeat
  ms := emu.start_of_emulator
  rd := ms+$1000
  eeprom_Read(rd, EEPROM_address, ROM_length)
  repeat until eeprom_IsDone
  cogstop(eeprom_cog)

  reg6502_stackptr := $FF
  memory_offset := word[rd+4]+rd-word[rd+12]   ' amount to add to get ROM data
  memory_start := ms                            ' amount to add to get zero page addresses
  reg6502_pc := word[word[rd+14]+memory_offset]       ' set to RESET address in PPU rom
  word[emu.NMI_ptr] := word[rd+16]
  word[emu.breakpoint_ptr] :=  $0  ' sets a breakpoint

  Setup_PPU(rd)

  if debug_mode
    emu_mode := 0
    emu.start(@emu_mode)
    bytefill(nt,byte[rd+21],2048)          ' clear name tables to blank screen
    bytefill(nt+960,0,64)                                 ' clear attrib`ute table 1
    bytefill(nt+960+$0400,0,64)                           ' clear attribute table 2
  else
    emu_mode := 2
    apu.start(7)
    emu.start_and_replace(@emu_mode)


  
  bp_count := 0                                    ' stop after n breakpoints reached
  button_pressed := False
  Disp_ctr := word[rd+12]
    
  repeat
    ' Wait for a VBL using the legacy TV variable 'tv_status'
    repeat while tv_status == 1
    repeat while tv_status == 2
'{
    ' Read both gamepad
    if emu_mode==0
       if bp_count>0
          bp_count--
          emu_mode := 2
       temp1 := NES_Read_Gamepad
'       byte[spin_ppuctrl_adr]    := ((temp1 >> 4)& 3) | (char_pattern_table << 4 )
'       word[spin_vertical_scroll] :=0

       if (temp1 & NES_LEFT)         ' Left button - run while button is held
'        byte[spin_ppuctrl_adr] := $90
'        byte[spin_vblank_adr] := 0
'        emu_mode :=2
         emu_mode := 1

       if button_pressed
         if (temp1 == 0)
           button_pressed := False
       if not button_pressed
         if (temp1 & NES_RIGHT)
           button_pressed := True     ' Right button - run one step at a time
           emu_mode :=1
'        long[spin_apu_dma_adr+12] := $400308CF ' (17 << 27) | (366 << 16) | 5
'        byte[spin_apu_status_adr] := 15
         if (temp1 & NES_LEFT)
'        button_pressed := True     ' Left button
           emu_mode :=1
'        long[spin_apu_dma_adr+12] := $400C08CF ' (31 << 27) | (285 << 16) | 3
'        byte[spin_apu_status_adr] := 15
         if (temp1 & NES_A)
           button_pressed := True
'           emu_mode :=3
           long[spin_apu_dma_adr] :=  $0C74081F ' $0903081F ' $0C740870
           byte[spin_apu_status_adr] :=15
'        running_to_breakpoint := True
         if (temp1 & NES_B)
           button_pressed := True     ' B Button - run at full speed
'        byte[spin_ppuctrl_adr] := $90
'        byte[spin_vblank_adr] := 0
           emu_mode :=2
         if (temp1 & NES_UP)
           Disp_ctr -= 8
'           button_pressed := True
         if (temp1 & NES_DOWN)
           Disp_ctr += 8
'           button_pressed := True

    if emu_mode<>2 and bp_count==0
       Show_Emulation_Info
       Hex(0,0,$2000,4)
'       Hex($400,0,$2800,4)
'       Hex($800,0,$2800,4)
'       Hex($C00,0,$2C00,4)
       Show_NES_RAM_Content(Disp_ctr,memory_offset)    ' Shows zero page of NES RAM
'       Show_NES_RAM_Content($FF00,memory_offset)    ' Shows NES ROM                                             E
'       Show_NES_RAM_Content($2080,nt-$2000)' Shows PPU name table RAM
'       Show_NES_RAM_Content(spin_apu_dma_adr-8,0) ' data.pallettes,0)                  ' Shows Hydra HUB memory
'    Show_NES_RAM_Content(spin_apu_dma_adr,0) ' data.pallettes,0)                  ' Shows Hydra HUB memory
'       Show_NES_RAM_Content($0000,data.pattern_tables)
'        Show_NES_RAM_Content($3F00,data.pallettes-$3F00) ' Shows PPU pallette memory

'      

PUB Show_Emulation_Info | temp

    Hex(112,24,reg6502_xreg,2)  ' X-xx
    Hex(117,25,reg6502_yreg,2)  ' Y-xx
    Hex(122,19,reg6502_stackptr,2)  ' S-xx
    byte[dnt+136] := letter_offset+16 ' place "P"
'    Hex(169,1,byte[$430+memory_start],2)     ' PC-xxxx
'    Hex(169,1,$430+memory_start,4)     ' PC-xxxx
    byte[dnt+144] := letter_offset+14 ' "N"
    byte[dnt+145] := letter_offset+22 ' "V"
    byte[dnt+146] := letter_offset+21 ' "unused"
    byte[dnt+147] := letter_offset+2  ' "B"
    byte[dnt+148] := letter_offset+4  ' "D"
    byte[dnt+149] := letter_offset+9  ' "I"
    byte[dnt+150] := letter_offset+26 ' "Z"
    byte[dnt+151] := letter_offset+3  ' "C"
    
    Binary(176,0,reg6502_status,8)
    Hex(137,3,reg6502_pc,4)     ' PC-xxxx
    Hex(105,1,reg6502_acc,2)    ' A-xx
    Hex(186,5,emu_status,1)
 
    if  (reg6502_pc & $8000)
      temp := reg6502_pc+memory_offset
    else
      temp := reg6502_pc+memory_start
      
    Hex(96,0,reg6502_pc,4)
    Hex(101,0,byte[temp],2)
    Hex(128,0,reg6502_pc+1,4)
    Hex(133,0,byte[temp+1],2)
    Hex(160,0,reg6502_pc+2,4)
    Hex(165,0,byte[temp+2],2)
                        
PUB Show_NES_RAM_Content(mem_loc,offset) | temp

    temp :=$0
    repeat 16 ' 8 ' 16
      Hex(224+((temp)>>3)<<5,0,mem_loc,4)       ' was 224+
      repeat 8
        Hex(229+(((temp)>>3)<<5)+(temp&7)*3,0,byte[offset+mem_loc],2)
        mem_loc++
        temp++
        
 
        
'---------------------------------------------

' General read gamepad spin function                       
PUB NES_Read_Gamepad : nes_bits   |       i
  DIRA [3] := 1 ' output
  DIRA [4] := 1 ' output
  DIRA [5] := 0 ' input
  DIRA [6] := 0 ' input

  OUTA [3] := 0 ' JOY_CLK = 0
  OUTA [4] := 0 ' JOY_SH/LDn = 0
  OUTA [4] := 1 ' JOY_SH/LDn = 1
  OUTA [4] := 0 ' JOY_SH/LDn = 0
  nes_bits := 0
  nes_bits := INA[5] | (INA[6] << 8)

  repeat i from 0 to 6
    OUTA [3] := 1 ' JOY_CLK = 1
    OUTA [3] := 0 ' JOY_CLK = 0
    nes_bits := (nes_bits << 1)
    nes_bits := nes_bits | INA[5] | (INA[6] << 8)

  nes_bits := (!nes_bits & $FFFF)

PUB Hex(loc,letter,amount,digits) | temp

  if letter>0
    byte[dnt+loc] := letter_offset+letter
    byte[dnt+loc+1] := dash_char ' $27 is the "-" character in the Galaga nametable
    loc+=2
  loc := loc+digits-1
  repeat while digits>0
    temp := (amount & 15)
    if temp>9
      byte[dnt+loc] := letter_offset+temp-9
    else
      byte[dnt+loc] := number_offset+temp
    amount >>= 4
    loc--
    digits--
    

PUB Binary(loc,letter,amount,digits) | temp

  if letter>0
    byte[dnt+loc] := letter_offset+letter
    byte[dnt+loc+1] := dash_char ' "-"
    loc+=2
  loc := loc+digits-1
  repeat while digits>0
    temp := (amount & 1)
    byte[dnt+loc] := number_offset+temp
    amount >>= 1
    loc--
    digits--

      
PUB  eeprom_Read(arg_buffer_address, arg_eeprom_address, arg_byte_count)

  long[varaddress+24] := arg_buffer_address
  long[varaddress+20] := arg_eeprom_address
  long[varaddress+16] := arg_byte_count
  long[varaddress+12] := COMMAND_READ

PUB eeprom_GetBytesRemaining

  return long[varaddress+16]

PUB eeprom_IsDone

  return (long[varaddress+12] == COMMAND_NONE)
  
PUB Setup_PPU(rom_data)

  nt := ms + $0800
  dnt := nt

  letter_offset := byte[rom_data+18] ' $40 ' byte[rom_data+18]
  number_offset := byte[rom_data+19] ' $30 ' byte[rom_data+19]
  dash_char     := byte[rom_data+22] ' 58 ' byte[rom_data+22]

  word[spin_pallettes_adr] := word[rom_data+8]+rom_data
  word[spin_nametables_adr] := nt
  word[spin_patterntables_adr] := word[rom_data+6]+rom_data
  word[spin_spritetable_adr] := @sprite_data
    
  ' Set up PPU initial conditions
  byte[spin_ppuctrl_adr]    := (byte[rom_data+23] << 4 ) ' 0 ' (byte[rom_data+23] << 4 )
  byte[spin_ppumask_adr]    := %0001_1010
  byte[spin_vertical_scroll] :=0
  byte[spin_horizontal_scroll] := 0
  byte[spin_nmi_request_adr] := 0

  longfill(@sprite_data,$FF,64)

  