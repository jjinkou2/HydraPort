{*************************************
 *   EEPROM loader version 0.01      *
 *************************************

Copyright (C) 2010 Darryl Biggar

This program loads the PPU and ROM image files into the EEPROM
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

' Gamepad constant declaration
  NES_RIGHT  = %00000001
  NES_LEFT   = %00000010
  NES_DOWN   = %00000100
  NES_UP     = %00001000
  NES_START  = %00010000
  NES_SELECT = %00100000
  NES_B      = %01000000
  NES_A      = %10000000

  EEPROM_start =  $8000
  EEPROM_end   = $20000
  
VAR

  long tv_status      '0/1/2 = off/visible/invisible           read-only

  long  temp1,temp2

  long  cog_number     ' Current rendering cog ID
  long  cog_total      ' Total number of required rendering cog
  long  horiz_offset
  long  vert_offset

  long  sprite_data[64]         ' NES memory and name tables
  
  long  emu_mode                ' these variables have to be in this order
  long  emu_status
  long  reg6502_acc
  long  reg6502_xreg
  long  reg6502_yreg
  long  reg6502_status
  long  reg6502_stackptr
  long  reg6502_pc
  long  memory_offset
  long  memory_start
  
  long  nt,ms,dnt
  long  button_pressed, Number_ROMs, ROM_selected, ROM_append_address, counter  
                                                                                                       
OBJ


'  tv    : "NES_tv_017.spin"
  tv    : "FakeTV"
  ppu   : "NES_PPU_driver_011.spin"
  eeprom: "NS_eeprom_drv_010.spin"

'
'   Uncomment the line below with the ROM you wish to load into the EEPROM
'
'

  rom  : "NES_Pacman_ROM.spin"
'  rom  : "NES_DonkeyKong_ROM.spin"'
'  rom  : "NES_Galaga_ROM.spin"
'  rom  : "NES_SoundDemo_ROM"


PUB start

  ' Set up pointers to data tables:
'  ms := @NES_mem_name_tables
  ms := tv.start_of_tv_driver   
  nt := ms+$0040 
  dnt :=nt+rom#nametable_offset

  word[spin_pallettes_adr] := rom.pallettes
  word[spin_nametables_adr] := nt
  word[spin_patterntables_adr] := rom.pattern_tables
  word[spin_spritetable_adr] := @sprite_data
    
  ' Set up PPU initial conditions
  byte[spin_ppuctrl_adr]    := (rom#char_pattern_table << 4 )
  byte[spin_ppumask_adr]    := %0001_1010
  byte[spin_vertical_scroll] :=0
  byte[spin_horizontal_scroll] := 0
  byte[spin_nmi_request_adr] := 0
  

  ' Boot rendering cogs:

  cog_total := 5
  cog_number := 0
  repeat
    ppu.start(@cog_number)
    repeat 10000 ' Allow some time for previous cog to boot up before setting 'cog_number' again
    cog_number++
  until cog_number == cog_total


  tv.start(@tv_status)
  eeprom.start(28,29,0)
'  emu.start(@emu_mode)

  bytefill(nt,rom#blank_char,2048)                ' clear name tables to blank screen
  bytefill(nt+960,0,64)                                 ' clear attribute table 1
  bytefill(nt+960+$0400,0,64)                           ' clear attribute table 2
  
        
  button_pressed := False


  Number_ROMs := 0
  ROM_selected := 1
  ROM_append_address := EEPROM_start
  counter := 0
  LoadROMnames

  repeat
    ' Wait for a VBL using the legacy TV variable 'tv_status'
    repeat while tv_status == 1
    repeat while tv_status == 2

    DrawString($20,@TopBars)
    DrawString($40,@Title)
    DrawString($60,@BottomBars)
    DrawString($A0,@Instructions)
    
'    Hex($300,0,Number_ROMs,4)
    DrawNames(Number_ROMs,@ROM_Names)
    DrawString($320,@NextAvail)
    Hex($338,0,ROM_Append_Address,6)
    DrawString($340,@Current_ROM)
    DrawString($34D,rom.ROM_data+24)
       
    '    SelectName(ROM_selected)

    temp1 := NES_Read_Gamepad
    Hex($360,0,counter>>8,2)
    counter++
    if not button_pressed
'      Hex($300,0,temp1,4)
      if (temp1 & NES_A)
        button_pressed := True
        AppendCurrentROM
        LoadROMnames
      if (temp1 & NES_B)
        button_pressed := True
        EraseAll
        LoadROMnames
    if temp1==0
      button_pressed := False

PUB AppendCurrentROM | ROM_data_length,t1,t2
    ROM_data_length := long[rom.ROM_data]
'    Hex($300,0,ROM_append_address+ROM_data_length,6)
    if ROM_append_address+ROM_data_length => EEPROM_end      ' exceeds 128K of EEPROM space?
      ClearLine($360)
      DrawString($360,@EEPROM_full)
    else
      eeprom.Write(rom.ROM_data, ROM_append_address, ROM_data_length)
      repeat until eeprom.IsDone
        Hex($360,0,eeprom.GetBytesRemaining,4)

      ROM_append_address += ROM_data_length
      ROM_data_length := 0
      eeprom.Write(@ROM_data_length, ROM_append_address, 4)
      repeat until eeprom.IsDone
        Hex($360,0,eeprom.GetBytesRemaining,4)
'      DrawString($360,@Done)
    
PUB EraseAll | ROM_data_length
   ROM_data_length := 0
   ROM_append_address := EEPROM_start
   eeprom.Write(@ROM_data_length, ROM_append_address, 4)
   repeat until eeprom.IsDone

PUB LoadROMnames | ROM_length
  ROM_append_address := EEPROM_start
  Number_ROMs := 0
  repeat ' 1
'    Hex($308,0,ROM_append_address,6)
    eeprom.Read(ms,ROM_append_address,40)
    repeat until eeprom.IsDone
'    Show_NES_RAM_Content($0,ms)
    ROM_length := long[ms]
    if ||ROM_length>$8000
      ROM_length :=0
    if ROM_append_address>$20000
      ROM_length := 0
    if ROM_length>0
      bytemove(@ROM_names+(Number_ROMs<<4),ms+24,16)
      Number_ROMs++
      ROM_append_address += ROM_length
'    Hex($368,0,Number_ROMs,4)
'    Hex($370,0,ROM_append_address,4)
  until ROM_length==0
'  DrawString($100,@Done)
      
  
  
  
PUB DrawNames(number,pointer) | loc, i

  loc := $E0
  repeat i from 0 to 8
    ClearLine(loc+i*64)
  if number>0
    repeat i from 1 to number
       DrawString(loc,@ROM_names+(i-1)*16)
       loc := loc +64
       if loc>$300
          loc := $F0
  else
    DrawString(loc,@No_ROMs)
           
 
PUB DrawString(loc,text) | i,letter
    repeat i from 0 to strsize(text)-1
        letter := byte[text][i]
        case letter
          "A".."Z": letter := letter - $40 + rom#letter_offset
          "0".."9": letter := letter - $30 + rom#number_offset
          " ": letter := rom#blank_char
'          $10..$1F :            ' do nothing to allow border around title to appear in case of Pacman font
          other: letter := rom#dash_char
        byte[dnt+loc] := letter 
        loc++
        
PUB ClearLine(loc)

  bytefill(nt+loc,rom#blank_char,32)

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

PUB Show_NES_RAM_Content(mem_loc,offset) | loc

    loc :=$0
    repeat 8 ' 8 ' 16
      Hex(256+(loc>>3)<<5,0,mem_loc,4)       ' was 224+
      repeat 8
        Hex(261+((loc>>3)<<5)+(loc&7)*3,0,byte[offset+mem_loc],2)
        mem_loc++
        loc++

PUB Hex(loc,letter,amount,digits) | digit

  if letter>0
    byte[dnt+loc] := rom#letter_offset+letter
    byte[dnt+loc+1] := rom#dash_char
    loc+=2
  loc := loc+digits-1
  repeat while digits>0
    digit := (amount & 15)
    if digit>9
      byte[dnt+loc] := rom#letter_offset+digit-9
    else
      byte[dnt+loc] := rom#number_offset+digit
    amount >>= 4
    loc--
    digits--
    

PUB Binary(loc,letter,amount,digits)

  if letter>0
    byte[dnt+loc] := rom#letter_offset+letter
    byte[dnt+loc+1] := rom#dash_char ' "-"
    loc+=2
  loc := loc+digits-1
  repeat while digits>0
    temp1 := (amount & 1)
    byte[dnt+loc] := rom#number_offset+temp1
    amount >>= 1
    loc--
    digits--

DAT

TopBars       byte      $1F,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
              byte      $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$1D,0
Title         byte      $11,"   NES-HYDRA EEPROM LOADER    ",$11,0
BottomBars    byte      $1E,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
              byte      $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$1C,0
Instructions  byte      "A. PGM EEPROM   B. ERASE ALL",0
No_ROMs       byte      "NO ROMS LOADED",0
Current_ROM   byte      "CURRENT ROM.",0
EEPROM_full   byte      "NOT ENOUGH SPACE ON EEPROM",0
NextAvail     byte      "NEXT AVAIL EEPROM ADDR.",0
ROM_names     byte      "1. PACMAN",0,0,0,0,0,0,0
              byte      "2. GALAGA",0,0,0,0,0,0,0
              byte      "3. DONKEY KONG",0,0
              byte      "4. PACMAN",0,0,0,0,0,0,0
              byte      "5. GALAGA",0,0,0,0,0,0,0
              byte      "6. DONKEY KONG",0,0
              byte      "7. PACMAN",0,0,0,0,0,0,0
              byte      "8. GALAGA",0,0,0,0,0,0,0
              byte      "9. DONKEY KONG",0,0
              byte      "10. PACMAN",0,0,0,0,0,0
              byte      "11. GALAGA",0,0,0,0,0,0
              byte      "12. DONKEY KONG",0
              