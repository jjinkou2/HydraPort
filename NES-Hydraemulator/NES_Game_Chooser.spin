{*************************************
 *   EEPROM loader version 0.01      *
 *************************************

Copyright (C) 2010 Darryl Biggar

}
CON

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

  NES_RIGHT  = %00000001
  NES_LEFT   = %00000010
  NES_DOWN   = %00000100
  NES_UP     = %00001000
  NES_START  = %00010000
  NES_SELECT = %00100000
  NES_B      = %01000000
  NES_A      = %10000000

  #0, COMMAND_NONE, COMMAND_READ, COMMAND_WRITE

  EEPROM_start = $8000
  debug_mode = false
  
VAR
  long  varaddress,Number_ROMs,ms,ROM_address,dnt,nt
  long  tv_status,cog_number,cog_total

  
OBJ
  tv    : "NES_tv_017.spin"
  ppu   : "NES_PPU_driver_011.spin"
  empty_space: "Space_filler.spin"
  rom   : "NES_Pacman_tilemap.spin"

PUB Select_Game(eeprom_variables) | button_pressed,ROM_selected, ROM_read_address,counter,temp1, done

  button_pressed := False
  varaddress := eeprom_variables
  ms := tv.start_of_tv_driver
  nt := ms + $0800
  dnt := nt
  
  Setup_PPU(rom.pallettes,nt,rom.pattern_tables,rom.sprite_data)
  
  ROM_selected := 1
  counter := 0
  LoadROMnames
  done := false
      
  repeat until done
    ' Wait for a VBL using the legacy TV variable 'tv_status'
    repeat while tv_status == 1
    repeat while tv_status == 2

    DrawString($20,@TopBars)
    DrawString($40,@Title)
    DrawString($60,@BottomBars)
    DrawString($A0,@Instructions)
    
'    Hex($300,0,Number_ROMs,4)
    DrawNames(Number_ROMs,@ROM_Names)
    SelectROM(ROM_selected)
    Hex($340,0,ROM_selected,2)
       
    '    SelectName(ROM_selected)

    temp1 := NES_Read_Gamepad
    Hex($360,0,counter>>8,2)
    counter++
    if not button_pressed
      if (temp1 & NES_A)
        button_pressed := True
        done := true
      if (temp1 & NES_UP)
        button_pressed := True
        ROM_selected := (ROM_selected-1) #> 1
      if (temp1 & NES_DOWN)
        button_pressed := True
        ROM_selected := (ROM_selected+1) <# Number_ROMs
        
'        AppendCurrentROM
'        LoadROMnames
'      if (temp1 & NES_B)
'        button_pressed := True
'        EraseAll
'        LoadROMnames
    if temp1==0
      button_pressed := False

  ROM_Address := LoadROMdata(ROM_selected)
'  Show_NES_RAM_Content($0,ms)    ' Shows zero page of NES RAM
    
  return @ms 


PUB Setup_PPU(pl_addr,nt_addr,pt_addr,sd_addr)

  word[spin_pallettes_adr] := pl_addr
  word[spin_nametables_adr] := nt_addr
  word[spin_patterntables_adr] := pt_addr
  word[spin_spritetable_adr] := sd_addr
    
  ' Set up PPU initial conditions
  byte[spin_ppuctrl_adr]    := (rom#char_pattern_table << 4 )
  byte[spin_ppumask_adr]    := %0001_1010
  byte[spin_vertical_scroll] :=0
  byte[spin_horizontal_scroll] := 0
  byte[spin_nmi_request_adr] := 0

  if debug_mode
     cog_total := 4
  else
     cog_total :=5
    
  cog_number := 0
  repeat
    ppu.start(@cog_number)
    repeat 10000 ' Allow some time for previous cog to boot up before setting 'cog_number' again
    cog_number++
  until cog_number == cog_total

  tv.start(@tv_status)

  bytefill(nt,rom#blank_char,2048)          ' clear name tables to blank screen
  bytefill(nt+960,0,64)                                 ' clear attribute table 1
  bytefill(nt+960+$0400,0,64)                           ' clear attribute table 2

PUB LoadROMnames | ROM_length, ROM_append_address
  ROM_append_address := EEPROM_start
  Number_ROMs := 0
  repeat
'    Hex($308,0,ROM_append_address,6)
    eeprom_Read(ms,ROM_append_address,40)
    repeat until eeprom_IsDone
'    Show_NES_RAM_Content($0,ms)
    ROM_length := long[ms]
    if ||ROM_length>$8000
      ROM_length :=0
    if ROM_length>0
      bytemove(@ROM_names+(Number_ROMs<<4),ms+24,16)
      Number_ROMs++
      ROM_append_address += ROM_length
'    Hex($368,0,Number_ROMs,4)
'    Hex($370,0,ROM_append_address,4)
  until ROM_length==0
'  DrawString($100,@Done)
  
PUB LoadROMdata(ROM_selected) | ROM_length, ROM_append_address
  ROM_append_address := EEPROM_start
  ROM_length := 0
  repeat ROM_selected
    if ROM_length>0
      ROM_append_address += ROM_length
    Hex($368,0,ROM_append_address,6)
    eeprom_Read(ms,ROM_append_address,40)
    repeat until eeprom_IsDone
    ROM_length := long[ms]
    if ||ROM_length>$8000
      ROM_length :=0
  return ROM_append_address
  
PUB DrawNames(number,pointer) | loc, i

  loc := $100
  repeat i from 0 to 8
    ClearLine(loc+i<<6)
  if number>0
    repeat i from 1 to number
       DrawString(loc,@ROM_names+(i-1)<<4)
       loc := loc +64
       if loc>$300
          loc := $F0
  else
    DrawString(loc,@No_ROMs)
           
 
PUB SelectROM(ROM_selected) | loc,i
  bytefill(nt+960,0,64)
  loc := (($100+(ROM_selected-1)<<6)>>7)<<3
  if ROM_selected&1 ==0
    bytefill(nt+$3C0+loc,$A0,4)
  else
    bytefill(nt+$3C0+loc,$0A,4)
  
'  bytefill(nt+960,$AA,64)
    


PUB DrawString(loc,text) | i,letter
    repeat i from 0 to strsize(text)-1
        letter := byte[text][i]
        case letter
          "A".."Z": letter := letter - $40 + rom#letter_offset
          "0".."9": letter := letter - $30 + rom#number_offset
          " ": letter := rom#blank_char
          $10..$1F :            ' do nothing to allow border around title to appear in case of Pacman font
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
    

PUB Binary(loc,letter,amount,digits) | temp1

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

PUB  eeprom_Read(arg_buffer_address, arg_eeprom_address, arg_byte_count)

  long[varaddress+24] := arg_buffer_address
  long[varaddress+20] := arg_eeprom_address
  long[varaddress+16] := arg_byte_count
  long[varaddress+12] := COMMAND_READ

PUB eeprom_GetBytesRemaining

  return long[varaddress+16]

PUB eeprom_IsDone

  return (long[varaddress+12] == COMMAND_NONE)


DAT

TopBars       byte      $1F,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
              byte      $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$1D,0
Title         byte      $11,"   NES-HYDRA GAME CHOOSER     ",$11,0
BottomBars    byte      $1E,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
              byte      $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$1C,0
Instructions  byte      "USE ARROWS TO SELECT - PRESS A",0
No_ROMs       byte      "NO ROMS LOADED",0
'Current_ROM   byte      "CURRENT ROM.",0
'EEPROM_full   byte      "NOT ENOUGH SPACE ON EEPROM",0
'NextAvail     byte      "NEXT AVAIL EEPROM ADDR.",0
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
              