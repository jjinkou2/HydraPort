'
'  6502 Emulator - Derived from an original file provided by Eric Ball
' Copyright (C) Eric Ball and Darryl Biggar (See copyright notice at the end)
'
'  This cog emulates instruction set of the 6502 and the relevant NES memory I/O operations
'
'
'
CON SCANLINE_BUFFER = $7F00
'
PUB start_of_emulator
RETURN @emustartvar

PUB NMI_ptr
RETURN @_NMI

PUB breakpoint_ptr
RETURN @breakpoint

PUB start(paramadr)
  opcodeptr := @decode6502
  cognew( @cogstart, paramadr )

PUB start_and_replace(paramadr)
  opcodeptr := @decode6502
  coginit(cogid, @cogstart, paramadr )

DAT

' This is the opcode table, stored in HUB RAM.
' Currently it only implements the "official" 6502 opcodes.
' Known "unofficial" opcodes are included in the comments.
' Note: many "unofficial" opcodes have non-intuitive
' behaviours and side effects.

                        org     0
decode6502              jmp     #_BRK           nr     ' 00 Implied BRK $00 1 7
                        jmpret  _ORA, #_IND_X   nr     ' 01 Indirect,X ORA ($44,X) $01 2 6
                        nop                            ' 02
                        nop                            ' 03 Indirect,X ASL+ORA
                        nop                            ' 04 Zero Page NOP
                        jmpret  _ORA, #_ZPAGE   nr     ' 05 Zero Page ORA $44 $05 2 2
                        jmpret  _ASLM, #_ZPAGE  nr     ' 06 Zero Page ASL $44 $06 2 5
                        nop                            ' 07 Zero Page ASL+ORA
                        jmp     #_PHP           nr     ' 08 PHP (PusH Processor status) $08 3 
                        jmpret  _ORA, #_IMMED   nr     ' 09 Immediate ORA #$44 $09 2 2
                        jmp     #_ASLA          nr     ' 0A Accumulator ASL A $0A 1 2
                        nop                            ' 0B Immediate ASL+AND
                        nop                            ' 0C Absolute NOP
                        jmpret  _ORA, #_ABSOL   nr     ' 0D Absolute ORA $4400 $0D 3 4
                        jmpret  _ASLM, #_ABSOL  nr     ' 0E Absolute ASL $4400 $0E 3 6
                        nop                            ' 0F Absolute ASL + ORA 
                        jmpret  _BPL, #_IMMED   nr     ' 10 BPL (Branch on PLus) $10
                        jmpret  _ORA, #_IND_Y   nr     ' 11 Indirect,Y ORA ($44),Y $11 2 5+
                        nop                            ' 12
                        nop                            ' 13 Indirect,Y ASL+ORA
                        nop                            ' 14 Zero Page,X NOP
                        jmpret  _ORA, #_ZP_X    nr     ' 15 Zero Page,X ORA $44,X $15 2 3
                        jmpret  _ASLM, #_ZP_X   nr     ' 16 Zero Page,X ASL $44,X $16 2 6
                        nop                            ' 17 Zero Page,X ASL+ORA
                        jmp     #_CLC           nr     ' 18 CLC (CLear Carry) $18
                        jmpret  _ORA, #_ABS_Y   nr     ' 19 Absolute,Y ORA $4400,Y $19 3 4+
                        nop                            ' 1A NOP
                        nop                            ' 1B Absolute,Y ASL+ORA
                        nop                            ' 1C Absolute,X NOP
                        jmpret  _ORA, #_ABS_X   nr     ' 1D Absolute,X ORA $4400,X $1D 3 4+
                        jmpret  _ASLM, #_ABS_X  nr     ' 1E Absolute,X ASL $4400,X $1E 3 7
                        nop                            ' 1F Absolute,X ASL + ORA
                        jmp     #_JSR           nr     ' 20 Absolute JSR $5597 $20 3 6
                        jmpret  _ANDA, #_IND_X  nr     ' 21 Indirect,X AND ($44,X) $21 2 6
                        nop                            ' 22
                        nop                            ' 23 Indirect,X ROL+AND
                        jmpret  _BIT, #_ZPAGE   nr     ' 24 Zero Page BIT $44 $24 2 3
                        jmpret  _ANDA, #_ZPAGE  nr     ' 25 Zero Page AND $44 $25 2 2
                        jmpret  _ROLM, #_ZPAGE  nr     ' 26 Zero Page ROL $44 $26 2 5
                        nop                            ' 27 Zero Page ROL+AND
                        jmp     #_PLP           nr     ' 28 PLP (PuLl Processor status) $28 4
                        jmpret  _ANDA, #_IMMED  nr     ' 29 Immediate AND #$44 $29 2 2
                        jmp     #_ROLA          nr     ' 2A Accumulator ROL A $2A 1 2
                        nop                            ' 2B Immediate ROL+AND 
                        jmpret  _BIT, #_ABSOL   nr     ' 2C Absolute BIT $4400 $2C 3 4
                        jmpret  _ANDA, #_ABSOL  nr     ' 2D Absolute AND $4400 $2D 3 4
                        jmpret  _ROLM, #_ABSOL  nr     ' 2E Absolute ROL $4400 $2E 3 6
                        nop                            ' 2F Absolute ROL+AND
                        jmpret  _BMI, #_IMMED   nr     ' 30 BMI (Branch on MInus) $30
                        jmpret  _ANDA, #_IND_Y  nr     ' 31 Indirect,Y AND ($44),Y $31 2 5+
                        nop                            ' 32
                        nop                            ' 33 Indirect,Y ROL+AND
                        nop                            ' 34 Zero Page,X NOP
                        jmpret  _ANDA, #_ZP_X   nr     ' 35 Zero Page,X AND $44,X $35 2 3
                        jmpret  _ROLM, #_ZP_X   nr     ' 36 Zero Page,X ROL $44,X $36 2 6
                        nop                            ' 37 Zero Page,X ROL+AND
                        jmp     #_SEC           nr     ' 38 SEC (SEt Carry) $38
                        jmpret  _ANDA, #_ABS_Y  nr     ' 39 Absolute,Y AND $4400,Y $39 3 4+
                        nop                            ' 3A NOP
                        nop                            ' 3B Absolute,Y ROL+AND
                        nop                            ' 3C Absolute,X NOP
                        jmpret  _ANDA, #_ABS_X  nr     ' 3D Absolute,X AND $4400,X $3D 3 4+
                        jmpret  _ROLM, #_ABS_X  nr     ' 3E Absolute,X ROL $4400,X $3E 3 7
                        nop                            ' 3F Absolute,X ROL+AND
                        jmp     #_RTI           nr     ' 40 Implied RTI $40 1 6
                        jmpret  _EORA, #_IND_X  nr     ' 41 Indirect,X EOR ($44,X) $41 2 6
                        nop                            ' 42
                        nop                            ' 43 Indirect,X LSR+EOR
                        nop                            ' 44 Zero Page NOP
                        jmpret  _EORA, #_ZPAGE  nr     ' 45 Zero Page EOR $44 $45 2 3
                        jmpret  _LSRM, #_ZPAGE  nr     ' 46 Zero Page LSR $44 $46 2 5
                        nop                            ' 47 Zero Page LSR+EOR
                        jmp     #_PHA           nr     ' 48 PHA (PusH Accumulator) $48 3
                        jmpret  _EORA, #_IMMED  nr     ' 49 Immediate EOR #$44 $49 2 2
                        jmp     #_LSRA          nr     ' 4A Accumulator LSR A $4A 1 2
                        nop                            ' 4B Immediate AND+LSR
                        jmpret  _JMPA, #_ABSOL  nr     ' 4C Absolute JMP $5597 $4C 3 3
                        jmpret  _EORA, #_ABSOL  nr     ' 4D Absolute EOR $4400 $4D 3 4
                        jmpret  _LSRM, #_ABSOL  nr     ' 4E Absolute LSR $4400 $4E 3 6
                        nop                            ' 4F Absolute LSR+EOR
                        jmpret  _BVC, #_IMMED   nr     ' 50 BVC (Branch on oVerflow Clear) $50
                        jmpret  _EORA, #_IND_Y  nr     ' 51 Indirect,Y EOR ($44),Y $51 2 5+
                        nop                            ' 52
                        nop                            ' 53 Indirect,Y LSR+EOR
                        nop                            ' 54 Zero Page,X NOP
                        jmpret  _EORA, #_ZP_X   nr     ' 55 Zero Page,X EOR $44,X $55 2 4
                        jmpret  _LSRM, #_ZP_X   nr     ' 56 Zero Page,X LSR $44,X $56 2 6
                        nop                            ' 57 Zero Page,X LSR+EOR
                        jmp     #_CLI           nr     ' 58 CLI (CLear Interrupt) $58
                        jmpret  _EORA, #_ABS_Y  nr     ' 59 Absolute,Y EOR $4400,Y $59 3 4+
                        nop                            ' 5A NOP
                        nop                            ' 5B Absolute,Y LSR+EOR
                        nop                            ' 5C Absolute,X NOP
                        jmpret  _EORA, #_ABS_X  nr     ' 5D Absolute,X EOR $4400,X $5D 3 4+
                        jmpret  _LSRM, #_ABS_X  nr     ' 5E Absolute,X LSR $4400,X $5E 3 7
                        nop                            ' 5F Absolute,X LSR+EOR
                        jmp     #_RTS           nr     ' 60 Implied RTS $60 1 6
                        jmpret  _ADC, #_IND_X   nr     ' 61 Indirect,X ADC ($44,X) $61 2 6
                        nop                            ' 62
                        nop                            ' 63 Indirect,X ROR+ADC
                        nop                            ' 64 Zero Page NOP
                        jmpret  _ADC, #_ZPAGE   nr     ' 65 Zero Page ADC $44 $65 2 3
                        jmpret  _RORM, #_ZPAGE  nr     ' 66 Zero Page ROR $44 $66 2 5
                        nop                            ' 67 Zero Page ROR+ADC
                        jmp     #_PLA           nr     ' 68 PLA (PuLl Accumulator) $68 4
                        jmpret  _ADC, #_IMMED   nr     ' 69 Immediate ADC #$44 $69 2 2
                        jmp     #_RORA          nr     ' 6A Accumulator ROR A $6A 1 2
                        nop                            ' 6B Immediate AND+ROR
                        jmpret  _JMPI, #_ABSOL  nr     ' 6C Indirect JMP ($5597) $6C 3 5
                        jmpret  _ADC, #_ABSOL   nr     ' 6D Absolute ADC $4400 $6D 3 4
                        jmpret  _RORM, #_ABSOL  nr     ' 6E Absolute ROR $4400 $6E 3 6
                        nop                            ' 6F Absolute ROR+ADC
                        jmpret  _BVS, #_IMMED   nr     ' 70 BVS (Branch on oVerflow Set) $70
                        jmpret  _ADC, #_IND_Y   nr     ' 71 Indirect,Y ADC ($44),Y $71 2 5+
                        nop                            ' 72
                        nop                            ' 73 Indirect,Y ROR+ADC
                        nop                            ' 74 Zero Page,X NOP
                        jmpret  _ADC, #_ZP_X    nr     ' 75 Zero Page,X ADC $44,X $75 2 4
                        jmpret  _RORM, #_ZP_X   nr     ' 76 Zero Page,X ROR $44,X $76 2 6
                        nop                            ' 77 Zero Page,X ROR+ADC
                        jmp     #_SEI           nr     ' 78 SEI (SEt Interrupt) $78
                        jmpret  _ADC, #_ABS_Y   nr     ' 79 Absolute,Y ADC $4400,Y $79 3 4+
                        nop                            ' 7A NOP
                        nop                            ' 7B Absolute,Y ROR+ADC
                        nop                            ' 7C Absolute,X NOP
                        jmpret  _ADC, #_ABS_X   nr     ' 7D Absolute,X ADC $4400,X $7D 3 4+
                        jmpret  _RORM, #_ABS_X  nr     ' 7E Absolute,X ROR $4400,X $7E 3 7
                        nop                            ' 7F Absolute,X ROR+ADC
                        nop                            ' 80 Indirect,X NOP
                        jmpret  _STA, #_IND_X   nr     ' 81 Indirect,X STA ($44,X) $81 2 6
                        nop                            ' 82 Indirect,X NOP
                        nop                            ' 83 Indirect,X Store A & X
                        jmpret  _STY, #_ZPAGE   nr     ' 84 Zero Page STY $44 $84 2 3
                        jmpret  _STA, #_ZPAGE   nr     ' 85 Zero Page STA $44 $85 2 3
                        jmpret  _STX, #_ZPAGE   nr     ' 86 Zero Page STX $44 $86 2 3
                        nop                            ' 87 Zero Page Store A & X
                        jmp     #_DEY           nr     ' 88 DEY (DEcrement Y) $88
                        nop                            ' 89       
                        jmp     #_TXA           nr     ' 8A TXA (Transfer X to A) $8A
                        nop                            ' 8B Immediate TXA+AND
                        jmpret  _STY, #_ABSOL   nr     ' 8C Absolute STY $4400 $8C 3 4
                        jmpret  _STA, #_ABSOL   nr     ' 8D Absolute STA $4400 $8D 3 4
                        jmpret  _STX, #_ABSOL   nr     ' 8E Absolute STX $4400 $8E 3 4
                        nop                            ' 8F Absolute Store A & X
                        jmpret  _BCC, #_IMMED   nr     ' 90 BCC (Branch on Carry Clear) $90
                        jmpret  _STA, #_IND_Y   nr     ' 91 Indirect,Y STA ($44),Y $91 2 6
                        nop                            ' 92
                        nop                            ' 93 Indirect,Y A & X & mem -> mem
                        jmpret  _STY, #_ZP_X    nr     ' 94 Zero Page,X STY $44,X $94 2 4
                        jmpret  _STA, #_ZP_X    nr     ' 95 Zero Page,X STA $44,X $95 2 4
                        jmpret  _STX, #_ZP_Y    nr     ' 96 Zero Page,Y STX $44,Y $96 2 4
                        nop                            ' 97 Zero Page,Y Store A & X
                        jmp     #_TYA           nr     ' 98 TYA (Transfer Y to A) $98
                        jmpret  _STA, #_ABS_Y   nr     ' 99 Absolute,Y STA $4400,Y $99 3 5
                        jmp     #_TXS           nr     ' 9A TXS (Transfer X to Stack ptr) $9A 2    
                        nop                            ' 9B Absolute,Y A & B -> S & mem -> mem
                        nop                            ' 9C Absolute,X Y & mem -> mem
                        jmpret  _STA, #_ABS_X   nr     ' 9D Absolute,X STA $4400,X $9D 3 5
                        nop                            ' 9E Absolute,Y X & mem -> mem 
                        nop                            ' 9F Absolute,Y A & X & mem -> mem
                        jmpret  _LDY, #_IMMED   nr     ' A0 Immediate LDY #$44 $A0 2 2
                        jmpret  _LDA, #_IND_X   nr     ' A1 Indirect,X LDA ($44,X) $A1 2 6
                        jmpret  _LDX, #_IMMED   nr     ' A2 Immediate LDX #$44 $A2 2 2
                        nop                            ' A3 Indirect,X Load A and X
                        jmpret  _LDY, #_ZPAGE   nr     ' A4 Zero Page LDY $44 $A4 2 3
                        jmpret  _LDA, #_ZPAGE   nr     ' A5 Zero Page LDA $44 $A5 2 3
                        jmpret  _LDX, #_ZPAGE   nr     ' A6 Zero Page LDX $44 $A6 2 3
                        nop                            ' A7 Zero Page Load A and X
                        jmp     #_TAY           nr     ' A8 TAY (Transfer A to Y) $A8
                        jmpret  _LDA, #_IMMED   nr     ' A9 Immediate LDA #$44 $A9 2 2
                        jmp     #_TAX           nr     ' AA TAX (Transfer A to X) $AA
                        nop                            ' AB Immediate ORA #$EE+AND+TAX
                        jmpret  _LDY, #_ABSOL   nr     ' AC Absolute LDY $4400 $AC 3 4
                        jmpret  _LDA, #_ABSOL   nr     ' AD Absolute LDA $4400 $AD 3 4
                        jmpret  _LDX, #_ABSOL   nr     ' AE Absolute LDX $4400 $AE 3 4
                        nop                            ' AF Absolute Load A and X
                        jmpret  _BCS, #_IMMED   nr     ' B0 BCS (Branch on Carry Set) $B0
                        jmpret  _LDA, #_IND_Y   nr     ' B1 Indirect,Y LDA ($44),Y $B1 2 5+
                        nop                            ' B2
                        nop                            ' B3 Indirect,Y Load A and X
                        jmpret  _LDY, #_ZP_X    nr     ' B4 Zero Page,X LDY $44,X $B4 2 4   (#_ZPAGE)
                        jmpret  _LDA, #_ZP_X    nr     ' B5 Zero Page,X LDA $44,X $B5 2 4
                        jmpret  _LDX, #_ZP_Y    nr     ' B6 Zero Page,Y LDX $44,Y $B6 2 4
                        nop                            ' B7 Zero Page,Y Load A and X
                        jmp     #_CLV           nr     ' B8 CLV (CLear oVerflow) $B8
                        jmpret  _LDA, #_ABS_Y   nr     ' B9 Absolute,Y LDA $4400,Y $B9 3 4+
                        jmp     #_TSX           nr     ' BA TSX (Transfer Stack ptr to X) $BA 2
                        nop                            ' BB Absolute,Y mem & S -> A,X,S
                        jmpret  _LDY, #_ABS_X   nr     ' BC Absolute,X LDY $4400,X $BC 3 4+
                        jmpret  _LDA, #_ABS_X   nr     ' BD Absolute,X LDA $4400,X $BD 3 4+
                        jmpret  _LDX, #_ABS_Y   nr     ' BE Absolute,Y LDX $4400,Y $BE 3 4+
                        nop                            ' BF Absolute,Y Load A and X
                        jmpret  _CPY, #_IMMED   nr     ' C0 Immediate CPY #$44 $C0 2 2
                        jmpret  _CMPA, #_IND_X  nr     ' C1 Indirect,X CMP ($44,X) $C1 2 6
                        nop                            ' C2 Indirect,X NOP
                        nop                            ' C3 Indirect,X DEC+CMP
                        jmpret  _CPY, #_ZPAGE   nr     ' C4 Zero Page CPY $44 $C4 2 3
                        jmpret  _CMPA, #_ZPAGE  nr     ' C5 Zero Page CMP $44 $C5 2 3
                        jmpret  _DEC, #_ZPAGE   nr     ' C6 Zero Page DEC $44 $C6 2 5
                        nop                            ' C7 Zero Page DEC+CMP
                        jmp     #_INY           nr     ' C8 INY (INcrement Y) $C8
                        jmpret  _CMPA, #_IMMED  nr     ' C9 Immediate CMP #$44 $C9 2 2
                        jmp     #_DEX           nr     ' CA DEX (DEcrement X) $CA
                        nop                            ' CB Immediate A & X - # -> X 
                        jmpret  _CPY, #_ABSOL   nr     ' CC Absolute CPY $4400 $CC 3 4
                        jmpret  _CMPA, #_ABSOL  nr     ' CD Absolute CMP $4400 $CD 3 4
                        jmpret  _DEC, #_ABSOL   nr     ' CE Absolute DEC $4400 $CE 3 6
                        nop                            ' CF Absolute DEC+CMP
                        jmpret  _BNE, #_IMMED   nr     ' D0 BNE (Branch on Not Equal) $D0
                        jmpret  _CMPA, #_IND_Y  nr     ' D1 Indirect,Y CMP ($44),Y $D1 2 5+
                        nop                            ' D2
                        nop                            ' D3 Indirect,Y DEC+CMP
                        nop                            ' D4 Zero Page,X NOP
                        jmpret  _CMPA, #_ZP_X   nr     ' D5 Zero Page,X CMP $44,X $D5 2 4
                        jmpret  _DEC, #_ZP_X    nr     ' D6 Zero Page,X DEC $44,X $D6 2 6
                        nop                            ' D7 Zero Page,X DEC+CMP
                        jmp     #_CLD           nr     ' D8 CLD (CLear Decimal) $D8
                        jmpret  _CMPA, #_ABS_Y  nr     ' D9 Absolute,Y CMP $4400,Y $D9 3 4+
                        nop                            ' DA NOP
                        nop                            ' DB Absolute,Y DEC+CMP
                        nop                            ' DC Absolute,X NOP
                        jmpret  _CMPA, #_ABS_X  nr     ' DD Absolute,X CMP $4400,X $DD 3 4+
                        jmpret  _DEC, #_ABS_X   nr     ' DE Absolute,X DEC $4400,X $DE 3 7
                        nop                            ' DF Absolute,X DEC+CMP
                        jmpret  _CPX, #_IMMED   nr     ' E0 Immediate CPX #$44 $E0 2 2
                        jmpret  _SBC, #_IND_X   nr     ' E1 Indirect,X SBC ($44,X) $E1 2 6
                        nop                            ' E2 Indirect,X NOP
                        nop                            ' E3 Indirect,X INC+SBC
                        jmpret  _CPX, #_ZPAGE   nr     ' E4 Zero Page CPX $44 $E4 2 3
                        jmpret  _SBC, #_ZPAGE   nr     ' E5 Zero Page SBC $44 $E5 2 3
                        jmpret  _INC, #_ZPAGE   nr     ' E6 Zero Page INC $44 $E6 2 5
                        nop                            ' E7 Zero Page INC+SBC
                        jmp     #_INX           nr     ' E8 INX (INcrement X) $E8
                        jmpret  _SBC, #_IMMED   nr     ' E9 Immediate SBC #$44 $E9 2 2
                        jmp     #_NOOP          nr     ' EA Implied NOP $EA 1 2
                        nop                            ' EB Immediate SBC
                        jmpret  _CPX, #_ABSOL   nr     ' EC Absolute CPX $4400 $EC 3 4
                        jmpret  _SBC, #_ABSOL   nr     ' ED Absolute SBC $4400 $ED 3 4
                        jmpret  _INC, #_ABSOL   nr     ' EE Absolute INC $4400 $EE 3 6
                        nop                            ' EF Absolute INC+SBC
                        jmpret  _BEQ, #_IMMED   nr     ' F0 BEQ (Branch on EQual) $F0
                        jmpret  _SBC, #_IND_Y   nr     ' F1 Indirect,Y SBC ($44),Y $F1 2 5+
                        nop                            ' F2
                        nop                            ' F3 Indirect,Y INC+SBC
                        nop                            ' F4 Zero Page,X NOP
                        jmpret  _SBC, #_ZP_X    nr     ' F5 Zero Page,X SBC $44,X $F5 2 4
                        jmpret  _INC, #_ZP_X    nr     ' F6 Zero Page,X INC $44,X $F6 2 6
                        nop                            ' F7 Zero Page,X INC+SBC
                        jmp     #_SED           nr     ' F8 SED (SEt Decimal) $F8
                        jmpret  _SBC, #_ABS_Y   nr     ' F9 Absolute,Y SBC $4400,Y $F9 3 4+
                        nop                            ' FA NOP
                        nop                            ' FB Absolute,Y INC+SBC
                        nop                            ' FC Absolute,X NOP
                        jmpret  _SBC, #_ABS_X   nr     ' FD Absolute,X SBC $4400,X $FD 3 4+
                        jmpret  _INC, #_ABS_X   nr     ' FE Absolute,X INC $4400,X $FE 3 7
                        nop                            ' FF Absolute,X INC+SBC

emustartvar
                        org     0
cogstart
' TODO Initialization
' - copy input parameters
 
entry                   mov     temp,par
                        rdlong  emu_mode,temp    wz
              if_z      jmp     #entry
                        
' load all relevant parameters
                        mov     ctr,#9
                        movd    code_entry,#emu_status
read_param_loop         add     temp,#4
code_entry              rdlong  0,temp
                        add     code_entry,destination_increment
                        djnz    ctr,#read_param_loop

' main emulation loop

fetch                   {rdbyte  temp,PPUctrl
                        test    temp,#$80 wz
        if_z            jmp     #not_nmi}                        
                        rdbyte  temp,nmi_request_ptr
                        cmp     temp,#1 wz
        if_nz           jmp     #not_nmi                ' 1=invisible; 2=visible
'                        test     emu_mode,#2 wz
'        if_z            jmp     #not_nmi                ' only trigger NMI for mode 2 or 3
        
                        wrbyte  H4000,nmi_request_ptr  ' get rid of signal that brought us here
'                        mov     ptr,#0                  ' process NMI - opcode #0 is BRK
'                        mov     emu_mode,#0
'                        jmp     #fetch4
                        jmp     #_BRK_no_pc_inc
                        
not_nmi                 mov     ptr, _PC
                        add     ptr, ROMbase
                        rdbyte  ptr, ptr
fetch4                  shl     ptr, #2
                        add     ptr, opcodeptr
                        rdlong  fetch2, ptr
                        add     _PC, #1
fetch2                  jmpret  _ADC, #_IMMED   nr     ' execute addressing mode
fetch3                  shr     fetch2, #9 wz          ' shift opcode to lsb
        if_nz           jmp     fetch2                 ' execute opcode


nextopcode              cmp     _PC,breakpoint wz
              if_z      mov     emu_mode,#0
                        cmp     emu_mode,#2 wz
              if_z      jmp     #fetch
              
'                        mov     emu_status,#1           ' 1 = code executed OK

exit_emulation          {cmp     emu_mode,#3     wz
        if_nz}           mov     emu_mode,#0

                        movd    code_wpl,#emu_mode
                        mov     temp,par
                        mov     ctr,#8
write_param_loop
code_wpl                wrlong  0,temp
                        add     temp,#4
                        add     code_wpl,destination_increment
                        djnz    ctr,#write_param_loop
'                        wrlong  emu_mode,par
                        jmp     #entry


'invalid_opcode          ' mov     emu_status,#2           ' 2 = opcode not recognised
'                        jmp     #exit_emulation

'
rdmempc                 mov     _AB, _PC
                        add     _PC, #1
rdmem                   mov     ptr, _AB
                        testn    ptr, ROMmask wz
        if_nz           add     ptr,ROMbase
        if_nz           jmp     #readret
                        testn   ptr, RAMmask wz
        if_z            add     ptr,RAMbase
        if_z            jmp     #readret
                        and     ptr,#$1F
                        cmp     ptr,#2 wz          ' deal with $2002 read
        if_nz           jmp     #rdrem1
                        add     ptr,PPUctrl
                        rdbyte  temp,ptr
                        mov     _DB,temp
                        and     temp,#$7F
                        wrbyte  temp,ptr
                        jmp     #rdmem_ret                        
rdrem1                  cmp     ptr,#7 wz
        if_nz           jmp     #Joystick_read
                        mov     temp,VRAMaddress
                        test    temp,H0800 wz
                        and     temp,H03FF
        if_nz           add     temp,H0400
                        rdword  ptr,name_table_ptr
                        add     ptr,temp           ' $2007 PatternTableMem[VRAMaddress] -> _DB
                        add     VRAMaddress,#1
readret                 rdbyte  _DB, ptr
rdmempc_ret
rdmem_ret               ret

Joystick_read           cmp     ptr, #$16 wz          ' $4016 read
        if_nz           jmp     #rdmem_ret
                        mov     _DB, INA           ' read all 32-bits of input including gamepads
                        or      OUTA, #%000001000 ' JOY_CLK = 1
                        shr     _DB, #5                 ' shift joystick data to bit 1
                        xor     _DB, #1
                        and     _DB, #1
                        and     OUTA,#%111110111 ' JOY_CLK = 0
                        jmp     #rdmem_ret

'
wrmem                   mov     ptr, _AB               '
'                        cmp     ptr,breakpoint wz
'        if_z            mov     emu_mode,#0
                        testn    ptr, RAMmask wz
        if_z            add     ptr,RAMbase
        if_z            jmp     #write_ret
                        cmp     ptr,H4000 wc,wz
        if_ae           jmp     #H4000_write
                        and     ptr,#7
                        cmp     ptr,#3 wz,wc
        if_b            add     ptr,PPUctrl
        if_b            jmp     #write_ret
        if_z            mov     OAMaddr,_DB             ' $2003 _DB -> OAMoffset
        if_z            jmp     #wrmem_ret
                        cmp     ptr,#4 wz
        if_z            rdword  ptr,sprite_data_ptr     ' $2004 _DB -> SpriteMem[OAMoffset]
        if_z            add     ptr,OAMaddr
        if_z            jmp     #write_ret
                        cmp     ptr,#5 wz
        if_z            rdword  ptr,xscroll
        if_z            shl     ptr,#8
        if_z            add     ptr,_DB
        if_z            wrword  ptr,xscroll
        if_z            jmp     #wrmem_ret              ' 2005 - not used so save the space
                        cmp     ptr,#6 wz
        if_z            shl     VRAMaddress,#8          ' $2006 _DB -> upper or lower byte of VRAMaddress
        if_z            add     VRAMaddress,_DB
        if_z            and     VRAMaddress,OOOOFFFF
        if_z            jmp     #wrmem_ret
                        mov     temp,VRAMaddress         ' $2007 _DB -> PatternTableMem[VRAMaddress]
                        cmp     temp,H3F00 wc,wz
        if_ae           jmp     #H3F00_write
'                        test    temp,H2000 wz
'        if_z            rdword  ptr,pattern_table_ptr
'        if_z            jmp     #write1
                        test    temp,H0800 wz
                        and     temp,H03FF
        if_nz           add     temp,H0400
                        rdword  ptr,name_table_ptr
write1                  add     ptr,temp
                        rdbyte  temp,PPUctrl
                        test    temp,#4 wz
        if_z            add     VRAMaddress,#1
        if_nz           add     VRAMaddress,#32
write_ret               wrbyte  _DB, ptr
wrmem_ret               ret

H3F00_write             and     temp,#31
                        rdword  ptr,pallette_ptr
'                        mov     emu_mode,#0
                        jmp     #write1

H4000_write             and     ptr,#$1F
                        cmp     ptr,#$10        wc,wz
'                        and     ptr,#$F
        if_b            jmp     #apu_dma_write
                        sub     ptr,#$13                ' $4014 - Sprite DMA access
                        djnz    ptr,#apu_write
'                        mov     ptr,_DB                 ' Sprite DMAaccess write
'                        shl     ptr,#8
'                        add     ptr,RAMbase
'                        wrword  ptr,sprite_data_ptr
'                        mov     ptr,_DB                 ' Sprite DMAaccess write
                        shl     _DB,#8
                        add     _DB,RAMbase
                        wrword  _DB,sprite_data_ptr
                        jmp     #wrmem_ret

apu_write               djnz    ptr,#Joystick_write     ' $4015 - APU status
                        mov     ptr,#(36-18)            ' APU status is 18 above APU dma
apu_dma_write           add     ptr,apu_dma_ptr
                        jmp     #write_ret

Joystick_write          djnz    ptr,#invalid_write         ' $4016 - Joystick write

                        or      DIRA, #%000011000          ' JOY_CLK and JOY_SH/LDn to outputs
                        and     DIRA, #%110011111          ' JOY_DATAOUT0 and JOY_DATAOUT1 to inputs
                        ror     _DB,#1 wc
         if_c           or      OUTA, #%000010000         ' JOY_SH/LDn = 1
         if_nc          and     OUTA, #%111101111          ' JOY_SH/LDn = 0
                        jmp     #wrmem_ret        

invalid_write           'mov     emu_status,#3           ' 3 = Attempt to write to an unsupported area
                        jmp     #wrmem_ret
'

' Addressing modes
_IMMED                  mov     _AB, _PC               ' Immediate 
                        add     _PC, #1
                        jmp     #fetch3

_ZPAGE                  call    #rdmempc               ' ZeroPage 
                        mov     _AB, _DB
                        jmp     #fetch3

_ZP_X                   call    #rdmempc               ' ZP,X  
                        add     _DB, _X
                        jmp     #zp_xy

_ZP_Y                   call    #rdmempc               ' ZP,Y
                        add     _DB, _Y
zp_xy                   and     _DB, #$FF
                        mov     _AB, _DB
                        jmp     #fetch3

_ABSOL                  call    #rdmempc               ' Absolute
                        jmp     #abs_xy

_ABS_X                  call    #rdmempc               ' Absolute,X
                        add     _DB, _X
                        jmp     #abs_xy

_ABS_Y                  call    #rdmempc               ' Absolute,Y
                        add     _DB, _Y
abs_xy                  movs    :src, _DB
                        call    #rdmempc
                        mov     _AB, _DB
                        shl     _AB, #8
:src                    add     _AB, #0-0
                        jmp     #fetch3

_IND_X                  call    #rdmempc               ' (ZP,X)
                        add     _DB, _X
                        and     _DB, #$FF
                        mov     _AB, _DB
                        call    #rdmem
indirect_xy             movs    :src, _DB
                        add     _AB, #1
                        and     _AB, #$FF
                        call    #rdmem
                        mov     _AB, _DB
                        shl     _AB, #8
:src                    add     _AB, #0-0
                        jmp     #fetch3

_IND_Y                  call    #rdmempc               ' (ZP),Y
                        mov     _AB, _DB
                        call    #rdmem
                        add     _DB, _Y
                        jmp     #indirect_xy

' Instructions

' TODO decimal mode
_ADC                    call    #rdmem                  ' N, V, Z, C
                        mov     temp, _A                ' save for oVerflow
                        test    _SR, #_CF wc
                        addx    _A, _DB
                        xor     _DB, temp
                        xor     temp, _A
                        andn    temp, _DB               ' sum different sign than old A and old A same sign as operand
                        test    temp, #$80 wc
                        muxc    _SR, #_VF
                        test    _A, #$100 wc
a_to_sr1                muxc    _SR, #_CF
a_to_sr                 and     _A, #$FF wz
a_to_sr0                muxz    _SR, #_ZF
                        test    _A, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode

_ANDA                   call    #rdmem                  ' N, Z
                        and     _A, _DB wz
                        jmp     #a_to_sr0
           
_ASLM                   call    #rdmem                  ' N, Z, C
                        shl     _DB, #1
                        test    _DB, #$100 wc
rmw1                    muxc    _SR, #_CF
rmw                     and     _DB, #$FF wz
rmw0                    muxz    _SR, #_ZF
                        test    _DB, #$80 wc
                        muxc    _SR, #_NF
                        call    #wrmem
'                        call    #wrmem
                        jmp     #nextopcode
                        
_BIT                    'jmp     #exit_emulation
                        call     #rdmem                  ' N, V, Z
                        test    _DB, _A wz
                        muxz    _SR, #_ZF
                        test    _DB, #$80 wc
                        muxc    _SR, #_NF
'                        test    _DB, #$40 wc
'                        muxc    _SR, #_VF
                        jmp     #nextopcode
                        
_BCC                    call    #rdmem
                        test    _SR, #_CF wc
branch_no_carry
              if_c      jmp     #nextopcode
branch                  test    _DB, #$80 wc
                        muxc    _DB, FFFFFFOO
                        add     _PC, _DB
                        and     _PC, OOOOFFFF
                        jmp     #nextopcode
                                      
_BCS                    call    #rdmem
                        test    _SR, #_CF wc
branch_on_carry
              if_nc     jmp     #nextopcode
                        jmp     #branch
                        
_BEQ                    call    #rdmem
                        test    _SR, #_ZF wc
                        jmp     #branch_on_carry

_BMI                    call    #rdmem
                        test    _SR, #_NF wc
                        jmp     #branch_on_carry

_BNE                    call    #rdmem
                        test    _SR, #_ZF wc
                        jmp     #branch_no_carry
                                      
_BPL                    call    #rdmem
                        test    _SR, #_NF wc
                        jmp     #branch_no_carry
                                      
_BVC                    call    #rdmem
                        test    _SR, #_VF wc
                        jmp     #branch_no_carry
                                      
_BVS                    call    #rdmem
                        test    _SR, #_VF wc
                        jmp     #branch_on_carry

_CMPA                   mov     temp, _A
compare                 call    #rdmem                  ' N, Z, C (sub w/o C)
                        sub     temp, _DB wc, wz
                        muxz    _SR, #_ZF
                        muxnc   _SR, #_CF               ' should this be muxc?
                        and     temp, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode

_CPX                    mov     temp, _X                ' N, Z, C (sub w/o C)
                        jmp     #compare
                        
_CPY                    mov     temp, _Y                ' N, Z, C (sub w/o C)
                        jmp     #compare

_DEC                    call    #rdmem                  ' N, Z
                        sub     _DB, #1
                        jmp     #rmw

_EORA                   call    #rdmem                  ' N, Z
                        xor     _A, _DB wz
                        jmp     #a_to_sr0

_INC                    call    #rdmem                  ' N, Z
                        add     _DB, #1
                        jmp     #rmw

_JMPI                   call    #rdmem
                        movs    :src, _DB
                        add     _AB, #1                 ' TODO handle JMP (xxFF)
                        call    #rdmem
                        mov     _AB, _DB
                        shl     _AB, #8
:src                    add     _AB, #0-0
_JMPA                   mov     _PC, _AB
                        jmp     #nextopcode

_LDA                    call    #rdmem                  ' N, Z
                        mov     _A, _DB wz
db_to_sr0               muxz    _SR, #_ZF
                        test    _DB, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode
                        
_LDX                    call    #rdmem                  ' N, Z
                        mov     _X, _DB wz
                        jmp     #db_to_sr0
                        
_LDY                    call    #rdmem                  ' N, Z
                        mov     _Y, _DB wz
                        jmp     #db_to_sr0
                        
_LSRM                   call    #rdmem                  ' N, Z, C
                        shr     _DB, #1 wc
                        jmp     #rmw1

_ORA                    call    #rdmem                  ' N, Z
                        or      _A, _DB wz
                        jmp     #a_to_sr0

_ROLM                   call    #rdmem                  ' N, Z, C
                        test    _SR, #_CF wc
                        rcl     _DB, #1
                        test    _DB, #$100 wc
                        jmp     #rmw1

_RORM                   call    #rdmem                  ' N, Z, C
                        test    _SR, #_CF wc
                        muxc    _DB, #$100
                        shr     _DB, #1 wc
                        jmp     #rmw1
                        
' TODO decimal mode
_SBC                    call    #rdmem                  ' N, V, Z, C
                        mov     temp, _A
                        test    _SR, #_CF+_FF wc        ' invert carry using future flag
                        subx    _A, _DB
                        xor     _DB, temp
                        xor     temp, _A
                        and     temp, _DB
                        test    temp, #$80 wc
                        muxc    _SR, #_VF
                        test    _A, #$100 wc
                        muxnc   _SR, #_CF               ' inverse carry
                        jmp     #a_to_sr

_STA                    mov     _DB, _A
                        call    #wrmem
                        jmp     #nextopcode
                        
_STX                    mov     _DB, _X
                        call    #wrmem
                        jmp     #nextopcode
                        
_STY                    mov     _DB, _Y
                        call    #wrmem
                        jmp     #nextopcode

' instructions with implicit addressing

_ASLA                   shl     _A, #1                  ' N, Z, C
                        test    _A, #$100 wc
a0_to_sr1               muxc    _SR, #_CF
a0_to_sr                and     _A, #$FF wz
a0_to_sr0               muxz    _SR, #_ZF
                        test    _A, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode
                        
_BRK                    add     _PC,#1                  ' 2 null := mem[PC++]
_BRK_no_pc_inc          mov     _DB, _PC
                        shr     _DB, #8
                        call    #push                  ' 3 mem[SP--] := PC[15..8]
                        mov     _DB, _PC
                        call    #push                  ' 4 mem[SP--] := PC[7..0]
                        mov     _DB, _SR
                        or      _DB, _BF
                        call    #push                  ' 5 mem[SP--] := SR or $10
                        mov     _AB, _NMI
                        jmp     #_JMPI                   ' 6,7 PC := mem[$FFFA]

_CLC                    andn    _SR, #_CF
                        jmp     #nextopcode
                        
_CLD                    andn    _SR, #_DF
                        jmp     #nextopcode
                        
_CLI                    andn    _SR, #_IM
                        jmp     #nextopcode
                        
_CLV                    andn    _SR, #_VF
                        jmp     #nextopcode
                        
_DEX                    sub     _X, #1                  ' N, Z
x_to_sr                 and     _X, #$FF wz
x_to_sr0                muxz    _SR, #_ZF
                        test    _X, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode
                        
_DEY                    sub     _Y, #1                  ' N, Z
y_to_sr                 and     _Y, #$FF wz
y_to_sr0                muxz    _SR, #_ZF
                        test    _Y, #$80 wc
                        muxc    _SR, #_NF
                        jmp     #nextopcode

_INX                    add     _X, #1                  ' N, Z
                        jmp     #x_to_sr

_INY                    add     _Y, #1                  ' N, Z
                        jmp     #y_to_sr

_JSR                    call    #rdmempc                ' 2 temp := mem[PC++]
                        movs    :src, _DB
                        mov     _DB, _PC
                        shr     _DB, #8
                        call    #push                  ' 4 mem[SP--] := PC[15..8]
                        mov     _DB, _PC
                        call    #push                  ' 5 mem[SP--] := PC[7..0]
                        call    #rdmempc                ' 6 PC[15..8] := mem[PC]
                        mov     _PC, _DB                
                        shl     _PC, #8
:src                    add     _PC, #0-0               '   PC[7..0]  := temp
                        jmp     #nextopcode

_LSRA                   shr     _A, #1 wc               ' N, Z, C
                        jmp     #a0_to_sr1

_NOOP                   jmp     #nextopcode

_PHA                    mov     _DB, _A
push_db                 call    #push
                        jmp     #nextopcode
                        
push                    mov     _AB, _SP
                        or      _AB, #$100
                        call    #wrmem                  ' 3 mem[SP--] := A/SR
                        sub     _SP, #1
                        and     _SP, #$FF
push_ret                ret
                        
_PHP                    mov     _DB, _SR
                        call    #push
                        jmp     #nextopcode

pull                    add     _SP, #1
                        and     _SP, #$FF
                        mov     _AB, _SP
                        or      _AB, #$100
                        call    #rdmem                  ' 4 A := mem[SP]
pull_ret                ret

_PLA                    call    #pull
                        mov     _A, _DB wz
                        jmp     #a0_to_sr0

_PLP                    call    #pull                  ' 4 SR := mem[SP]
                        mov     _SR, _DB
                        or      _SR, #_FF               ' set future flag
                        jmp     #nextopcode
                        
_RTI                    call    #pull                  ' 4 SR := mem[SP++] and $EF
                        mov     _SR, _DB
                        andn    _SR, _BF                ' clear break flag
                        or      _SR, #_FF               ' set future flag
                        call    #subreturn
                        rdbyte  temp,PPUctrl
                        or      temp,#$80
                        wrbyte  temp,PPUctrl
                        jmp     #nextopcode

subreturn               call    #pull                  ' 5 PC[7..0] := mem[SP++]
                        movs    :src, _DB
                        call    #pull                  ' 6 PC[15..8] := mem[SP]
                        mov     _PC, _DB
                        shl     _PC, #8
:src                    add     _PC, #0-0
subreturn_ret           ret

_RTS                    call    #subreturn
                        {call    #pull                  ' 4 PC[7..0] := mem[SP++]
                        movs    :src, _DB
                        call    #pull                  ' 5 PC[15..8] := mem[SP]
                        mov     _PC, _DB
                        shl     _PC, #8
:src                    add     _PC, #0-0}
                        add     _PC,#1
'                        call    #rdmempc                ' 6 null := mem[PC++]
                        jmp     #nextopcode

_ROLA                   test    _SR, #_CF wc            ' N, Z, C
                        rcl     _A, #1
                        test    _A, #$100 wc
                        jmp     #a0_to_sr1

_RORA                   test    _SR, #_CF wc            ' N, Z, C
                        muxc    _A, #$100
                        shr     _A, #1 wc
                        jmp     #a0_to_sr1

_SEC                    or      _SR, #_CF
                        jmp     #nextopcode
                        
_SED                    or      _SR, #_DF
                        jmp     #nextopcode
                        
_SEI                    or      _SR, #_IM
                        jmp     #nextopcode

_TAX                    mov     _X, _A wz               ' N, Z
                        jmp     #x_to_sr0

_TAY                    mov     _Y, _A wz               ' N, Z
                        jmp     #y_to_sr0

_TSX                    mov     _X, _SP wz              ' N, Z
                        jmp     #x_to_sr0

_TXA                    mov     _A, _X wz               ' N, Z
                        jmp     #a0_to_sr0

_TXS                    mov     _SP, _X
                        jmp     #nextopcode

_TYA                    mov     _A, _Y wz               ' N, Z
                        jmp     #a0_to_sr0

_NMI                    long    $C77A ' $FFFA                  ' Where to go when an NMI occurs
'_RESET                  long    $FFFC                  ' reset vector
'_IRQBRK                 long    $FFFE                  ' IRQ/BRK vector}

OOOOFFFF                long    $0000_FFFF             ' 16 bit mask
FFFFFFOO                long    $FFFF_FF00             ' sign extend

H4000                   long    $4000
H2000                   long    $2000
H03FF                   long    $03FF
H0800                   long    $0800
H0400                   long    $0400
H3F00                   long    $3F00
H80000000               long    $80000000
'
'

breakpoint              long    $0 ' $94E2 ' $8067 ' $807D ' $BAE9 ' $8092

RAMmask                 long    $1FFF                   ' lower 2K of internal NES memory map is RAM
ROMmask                 long    $7FFF                   ' top 32K of NES memory map is ROM           
'
destination_increment   long    512
PPUctrl                 long    SCANLINE_BUFFER-16               ' pointer to PPU i/o controls
sprite_data_ptr         long    SCANLINE_BUFFER-10               ' pointer to sprite memory location (word)
xscroll                 long    SCANLINE_BUFFER-12               ' pointer to x,y offset location
'pattern_table_ptr       long    SCANLINE_BUFFER-4                ' pointer to start of pattern tables
pallette_ptr            long    SCANLINE_BUFFER-6
name_table_ptr          long    SCANLINE_BUFFER-8                ' pointer to start of name tables
nmi_request_ptr         long    SCANLINE_BUFFER-17               ' pointer to signal of VBLANK 1=showing data 2 = vertical blank
apu_dma_ptr             long    SCANLINE_BUFFER-36      ' 4 channels of APU DMA
' apu_status_ptr          long    SCANLINE_BUFFER-18

'DMAaccess               long    $4014
'JoystickDMA             long    $4016


opcodeptr               long    1                      ' pointer to opcode table

emu_mode                long    $0
emu_status              long    $0
_A                      long     1                      ' A register
_X                      long     1                      ' X register
_Y                      long     1                      ' Y register
_SR                     long     1                      ' status register
_SP                     long     1                      ' stack pointer
_PC                     long     1                      ' program counter
ROMbase                 long    1                      ' pointer to ROM (high memory)
RAMbase                 long    1                      ' pointer to RAM (low memory)

_AB                     res     1                      ' address bus
_DB                     res     1                      ' data bus 
ptr                     res     1                      ' memory pointer
ctr                     long    0                      ' counter
temp                    long    1                      ' temporary
OAMaddr                 long    $0                      ' records offset to sprite memory

VRAMaddress             long    $0                      ' records offset location for read/write in VRAM

                        fit     $1F0
CON
_NF                     =       1<<7                   ' Negative flag
_VF                     =       1<<6                   ' Overflow flag
_FF                     =       1<<5                   ' Future flag
_BF                     =       1<<4                   ' Break flag
_DF                     =       1<<3                   ' Decimal flag
_IM                     =       1<<2                   ' Interrupt mask
_ZF                     =       1<<1                   ' Zero flag
_CF                     =       1<<0                   ' Carry flag

' 2009-10-08 moved opcode decode table to HUB RAM
' Cycle usage taken from Synertek SY6500/MCS6500 Microcomputer Family Programming Manual
' http://archive.6502.org/datasheets/synertek_programming_manual.pdf

{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                          6502 CORE (C) 2009-10-07 Eric Ball                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                    TERMS OF USE: Parallax Object Exchange License                                            │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}