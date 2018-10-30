{{//////////////////////////////////////////////////////////////////////

NES Audio Processing Unit Driver for the Hydra

by Darryl Biggar
Based on the sound driver by Nick Sabalausky (version 5.2 dated 21 July 06)
Information on the NES sound driver taken from Brad Taylor 2A03 sound channel hardware
documentation dated February 19th, 2003
}}

'///////////////////////////////////////////////////////////////////////
' CONSTANTS ////////////////////////////////////////////////////////////
'///////////////////////////////////////////////////////////////////////
CON

  SCANLINE_BUFFER = $7F00

  APU_DMA_Addr_Ptr = SCANLINE_BUFFER-36                 ' $7EDC
  APU_DMA_Status_Ptr = SCANLINE_BUFFER-18               ' $7EEE

  ' Sample rate has been set to 60*512 = 30720
  
  SAMPLE_RATE = 30720 ' 30720

  #0, SHAPE_IGNORE, SHAPE_SILENT, SHAPE_SINE, SHAPE_SAWTOOTH, SHAPE_SQUARE, SHAPE_TRIANGLE, SHAPE_NOISE, SHAPE_PCM_8BIT_11KHZ, SHAPE_RELEASE
  DURATION_INFINITE = $8000_0000
  NOTHING_PENDING = $0

  'Channel Data Field Offsets
  #0, CHDAT_THETA, CHDAT_THETA_DELTA, CHDAT_THETA_CYCLED, CHDAT_SHAPE, CHDAT_STOP_TIME, CHDAT_VOLUME, CHDAT_AMP_ENV, CHDAT_ENV_SEG_DURATION, CHDAT_LFSR, CHDAT_PCM_START, CHDAT_PCM_END, CHDAT_PCM_CURR, SIZE_OF_CHDAT
'  SIZE_OF_PARAM_CHDAT      = 7 'in longs: pending_shape, pending_freq, pending_duration, pending_volume, pending_amp_env, pending_pcm_start and pending_pcm_end
  INDEX_OF_CHANNEL0_PARAMS = 4 'in bytes: audio_pin 

'  AMP_ENV_SUSTAIN_SEG = 3  'The # of the amplitude segment to "sustain" for infinite duration sounds
                           '(Use anything from 1 to 6, inclusive)

'  NO_ENVELOPE = $FFFF_FFFF

  VOLUME_MIN = 1
  VOLUME_MAX = 255


'///////////////////////////////////////////////////////////////////////
' VARIABLES ////////////////////////////////////////////////////////////
'///////////////////////////////////////////////////////////////////////
VAR
  long cogon, cog

  'Communication paramaters. See above for explanation of protocol.
  long audio_pin

  long pending_shape
  long pending_freq
  long pending_duration
  long pending_volume
  long pending_amp_env
  long pending_pcm_start
  long pending_pcm_end

  long pending_shape_ch1
  long pending_freq_ch1
  long pending_duration_ch1
  long pending_volume_ch1
  long pending_amp_env_ch1
  long pending_pcm_start_ch1
  long pending_pcm_end_ch1

  long pending_shape_ch2
  long pending_freq_ch2
  long pending_duration_ch2
  long pending_volume_ch2
  long pending_amp_env_ch2
  long pending_pcm_start_ch2
  long pending_pcm_end_ch2

  long pending_shape_ch3
  long pending_freq_ch3
  long pending_duration_ch3
  long pending_volume_ch3
  long pending_amp_env_ch3
  long pending_pcm_start_ch3
  long pending_pcm_end_ch3


'///////////////////////////////////////////////////////////////////////
' OBJECTS //////////////////////////////////////////////////////////////
'///////////////////////////////////////////////////////////////////////
OBJ

'///////////////////////////////////////////////////////////////////////
' FUNCTIONS ////////////////////////////////////////////////////////////
'///////////////////////////////////////////////////////////////////////

'// PUB start //////////////////////////////////////////////////////////

PUB start(pin) : okay
{{
Starts the sound driver on a new cog.

    pin:      The PChip I/O pin to send audio to (always 7 on the Hydra)
    returns:  false if no cog available
}}

  audio_pin := pin
  okay := cogon := (cog := cognew(@entry,@audio_pin)) > 0


'///////////////////////////////////////////////////////////////////////

PUB replace_start(pin)
{{
Starts the sound driver on the current cog
}}

  audio_pin := pin
  coginit(cogid,@entry,@audio_pin)

'///////////////////////////////////////////////////////////////////////


'///////////////////////////////////////////////////////////////////////
' DATA /////////////////////////////////////////////////////////////////
'///////////////////////////////////////////////////////////////////////

DAT

'// Assembly language sound driver /////////////////////////////////////

                    org
'---- Entry
entry
                    '---- Initialization ----
                    rdlong  temp,par                         'Get audio pin
                    mov     dira_init,#1
                    shl     dira_init,temp
                    or      dira,dira_init                   'Set audio pin's direction to output

                    mov     time_to_resume,cnt               'Setup delay
                    add     time_to_resume,delay_amount

                    mov     frqa,long_half                   'Setup counter A
                    add     ctra_init,temp
                    mov     ctra,ctra_init

                    '- Start of main processing loop -
loop
                    '---- Process Pending API Requests ----
                    mov     temp,sound_clock
                    and     temp,#%11                  'Look at the low 2-bits of sound_clock
                    cmp     temp,curr_channel   wz      'Only process requests for one channel each 22KHz sound tick
        if_ne       jmp     #skip_pending_requests

        
                        mov     temp,curr_channel
                        shl     temp,#2                 ' Multiply by 4
                        add     temp,APU_DMA_Addr       ' Add pointer to APU DMA memory

                        rdbyte  temp2,temp      wz              ' Get first byte of channel information
        if_z            jmp     #second_byte
'
' get volume
'
'                        mov     temp2,temp
                        wrbyte  zero,temp
                        test    temp2,#16       wz      ' Check for envelope counter disable
                        and     temp2,#$F                ' isolate volume
                        movd    set_new_volume,volume_ptr
        if_nz           jmp     #set_new_volume
                        
'                        mov     temp2,#1                ' for debugging
                        movd    set_new_env_duration,env_seg_duration_ptr
                        add     temp2,#1
                        shl     temp2,#7 '                    'multiply by 128 to get duration in clock ticks
set_new_env_duration    mov     0,temp2
                        mov     temp2,#$1F               ' set maximum volume plus signal for decay

set_new_volume          mov     0,temp2

second_byte             add     temp,#1

third_byte              add     temp,#1
                        rdbyte  temp2,temp       wz
        if_z            jmp     #skip_setting_wavelength

'
' get length counter and set duration values
'
                        rdword  temp2,temp
                        mov     temp1,temp2
                        wrbyte  zero,temp
                                
                        shr     temp2,#11                ' isolate 5 bits length counter

'                        mov     temp,APU_DMA_addr       ' for debugging
'                        sub     temp,#8
'                        wrlong  temp2,temp

                        and     temp2,#31
                        ror     temp2,#1        wc
        if_nc           jmp     #bit3_one
                        and     temp2,#$F               ' convert length using delay table
                        add     temp2,#delay_table
                        movs    get_delay_table,temp2
                        nop
get_delay_table         mov     temp2,0
                        jmp     #set_delay        
bit3_one
                        and     temp2,#$F       wz
        if_z            mov     temp2,#$7F              ' 0 maps to $7F, otherwise maps to same number
        
set_delay               movs    get_volume2,volume_ptr
                        movs    get_decay_time,env_seg_duration_ptr
                        movd    set_new_stop_time,stop_time_ptr
                        shl     temp2,#9 '                    'multiply by 512 to get duration in clock ticks
                        andn    temp2,bit_28
get_volume2             mov     curr_vol,0
                        test    curr_vol,#16    wz
get_decay_time
        if_nz           mov     temp2,0
                   
                        add     temp2,sound_clock      wz  'Set new stop_time
        if_z            add     temp2,#1                   'Ensure _pending_duration+sound_clock doesn't become "never stop" (ie. 0)
set_new_stop_time       mov     0,temp2

'                        mov     temp,APU_DMA_addr       ' for debugging
'                        sub     temp,#4
'                        wrlong  sound_clock,temp
'                        sub     temp,#4
'                        wrlong  curr_channel,temp
'
' get wavelength and convert it to theta_delta
'
                        and     temp1,H07FF     wz             ' isolate 11 bits - wavelength
              if_z      jmp     #skip_setting_wavelength
                        cmp     curr_channel,#3 wz
              if_nz     jmp     #no_wl_convert
                        and     temp1,#$F
                        add     temp1,#noise_wl_convert
                        movs    get_wavelength,temp1
                        nop
get_wavelength          mov     temp1,0
                                      
no_wl_convert
                        movd    set_new_theta_delta,theta_delta_ptr

                        mov     t1,theta_delta_const
                        shl     temp1,#4                ' multiply by 16
                        cmp     curr_channel,#2 wz      ' if triangle channel multiply by 2
              if_z      shl     temp1,#1
                        cmp     curr_channel,#3 wz
              if_z      shr     temp1,#1
                        mov     m1,temp1
                        call    #divide                 ' t2 = $2000*1.79MHz/Sample_Rate/Wavelength

'                        mov     temp,APU_DMA_addr       ' for debugging
'                        add     temp,#8
'                        wrlong  temp1,temp
'                        add     temp,#4
'                        wrlong  t2,temp

set_new_theta_delta     mov   0,t2                  'Set new theta_delta

' set channel shape
'
                        cmp     curr_channel,#1 wc,wz
        if_be           mov     temp2,#SHAPE_SQUARE
                        cmp     curr_channel,#2 wz
        if_z            mov     temp2,#SHAPE_SINE
                        cmp     curr_channel,#3 wz
        if_z            mov     temp2,#SHAPE_NOISE
        
                        movd    set_new_shape,shape_ptr
                        sub     temp2,#1
set_new_shape           mov     0,temp2                 'Set new shape (unless it's SHAPE_RELEASE)

'                        mov     temp,APU_DMA_addr       ' for debugging
'                        sub     temp,#8
'                        wrlong  sound_clock,temp
'                        sub     temp,#4
'                        wrlong  temp2,temp


skip_setting_wavelength

skip_pending_requests

channel_loop

                        rdbyte  temp2,APU_Channel_Status_Addr
                        shr     temp2,curr_channel
                        and     temp2,#1        wz
        if_z            jmp     #next_channel


                    '---- Get State Data ----
                    movs  get_theta,theta_ptr
                    movs  get_theta_cycled,theta_cycled_ptr
                    movs  get_lfsr,lfsr_ptr

get_theta           mov   theta_temp,0                       'Get theta
get_theta_cycled    mov   theta_cycled_temp,0                'Get theta_cycled
get_lfsr            mov   lfsr_temp,0                        'Get lfsr

                    '---- Generate Sample ----
                    movs    jump_table_indexer,shape_ptr
                    mov     shape_jmp_ptr,#shape_jmp_table
jump_table_indexer  add     shape_jmp_ptr,0                  'Compute offset into shape_jmp_table
                    movs    shape_jmp,shape_jmp_ptr
                    nop                                      'Wait-out the pipelining
shape_jmp           jmp     0                                'Call shape routine to generate and output sample
return_from_shape

                    '---- Advance Theta ----
                    movs    advance_theta,theta_delta_ptr
                    nop
advance_theta       add     theta_temp,0                     'Advance theta
                    cmp     theta_temp,sin_360   wc          'Wrap from 360 degrees to 0 degrees
        if_ae       sub     theta_temp,sin_360

                    mov     theta_cycled_temp,#0             'Update theta_cycled
        if_ae       mov     theta_cycled_temp,#1

                    '---- Store State Data ----
                    movd    store_theta,theta_ptr
                    movd    store_theta_cycled,theta_cycled_ptr
                    movd    store_lfsr,lfsr_ptr

store_theta         mov     0,theta_temp                     'Store theta
store_theta_cycled  mov     0,theta_cycled_temp              'Store theta_cycled
store_lfsr          mov     0,lfsr_temp                      'Store lfsr

                    '---- Get/Update Envelope ----
                    'check if done with amplitude envelope segment
                        movs    get_volume1,volume_ptr
                        movs    check_at_stop_time,stop_time_ptr
'                        movs    check_env_duration,env_seg_duration_ptr
get_volume1             mov     curr_vol,0
check_at_stop_time      mov     temp1,0

                        test    curr_vol,#16    wz
'check_env_duration
'              {if_nz}     mov     temp1,0

'                        cmp     curr_channel,#0 wz
'        if_nz           jmp     #:skip

:skip                   cmp     temp1,sound_clock         wz         'Check if at end of envelope segment
'                        sub     temp1,sound_clock
'                        mov     temp,APU_DMA_addr       ' for debugging
'                        sub     temp,#4
'                        wrlong  sound_clock,temp
'                        sub     temp,#4
'                        wrlong  temp1,temp
'                        cmp     temp1,#0        wz
        if_ne           jmp     #skip_update_env                 'If not at end of segment, skip the rest of this section
'
' check to see if sound is decayinng

                        test    curr_vol,#16    wz
        if_z            jmp     #end_sound1             ' not decaying - stop sound

                        movd    update_vol_env,volume_ptr
                        sub     curr_vol,#1             ' decrease the volume by one
                        and     curr_vol,#15    wz
                        or      curr_vol,#16
update_vol_env          mov     0,curr_vol
        if_nz           jmp     #skip_stop_sound        ' if current volume not zero continue

'                        sub     temp,#4
'                        mov     temp1,#$111
'                        wrlong  temp1,temp

                    'sound is done, stop sound
end_sound1
                    movd    end_sound,shape_ptr              'Get address of current channel's shape
                    nop                                      'Stall for the pipelining
end_sound           mov     0,#SHAPE_SILENT-1                'Stop sound by setting shape to "silent"

                    jmp     #skip_reset_stop_time
skip_stop_sound
'
' get the duration until the next drop in volume
'
                        movs    get_duration,env_seg_duration_ptr
                        movd    reset_stop_time,stop_time_ptr
get_duration            mov    temp,0
                        add     temp,sound_clock        wz
        if_nz           add     temp,#1
reset_stop_time         mov     0,temp

skip_reset_stop_time
skip_update_env

                    '---- Adjust Volume of Sample ----
                    movs    get_volume,volume_ptr
                    nop
get_volume          mov     mult_y,0
'                        mov     temp,APU_DMA_addr       ' for debugging
'                        sub     temp,#8
'                        wrlong  mult_y,temp

                        shl     mult_y,#4

                    mov     mult_x,sample_temp

                    test    mult_x,bit_31   wz                  'Z=0 if the multiplicand mult_y is negative
                    abs     mult_x,mult_x                       'Do the multiplication with positive multiplicands
                    call    #multiply_16_by_12_bit
        if_nz       neg     mult_y,mult_y                       'If multiplicand was negative, negate the result

                    '---- Add Into Mixer ----
                    adds    mixed_sample,mult_y                 'Add sample into mix (no need to load/store sample?)

                    '---- Next Channel ----
next_channel
                    add     theta_ptr,           #SIZE_OF_CHDAT 'Update channel data pointers
                    cmp     theta_ptr,#end_of_channel_data  wz  'Was this the last channel?
        if_e        jmp     #mixer                              'If yes, jump to mixer
                    add     theta_delta_ptr,     #SIZE_OF_CHDAT 'Continue updating channel data pointers
                    add     theta_cycled_ptr,    #SIZE_OF_CHDAT
                    add     shape_ptr,           #SIZE_OF_CHDAT
                    add     stop_time_ptr,       #SIZE_OF_CHDAT
                    add     volume_ptr,          #SIZE_OF_CHDAT
                    add     amp_env_ptr,         #SIZE_OF_CHDAT
                    add     env_seg_duration_ptr,#SIZE_OF_CHDAT
                    add     lfsr_ptr,            #SIZE_OF_CHDAT
'                    add     curr_ch_param_index,#(SIZE_OF_PARAM_CHDAT*4)
                    add     curr_channel,#1
                    jmp     #loop                               'Goto next channel

                    '---- Average and Output Mixed Sample ----
mixer
                    shl     mixed_sample,#1           'Crank volume high as possible for 6 channels without clipping
                    add     mixed_sample,bit_31       'Adjust from signed [-max_amplitude, +max_amplitude] to unsigned [0,+2*max_amplitude]
                    mov     frqa,mixed_sample         'Output Mixed Sample

                    mov     mixed_sample,#0           'Clear mixed_sample
                    mov     active_channels,#0        'Clear active_channels

                    '---- Prepare For Next Iteration ----
                    mov     theta_ptr,           #channel_data+CHDAT_THETA
                    mov     theta_delta_ptr,     #channel_data+CHDAT_THETA_DELTA
                    mov     theta_cycled_ptr,    #channel_data+CHDAT_THETA_CYCLED
                    mov     shape_ptr,           #channel_data+CHDAT_SHAPE
                    mov     stop_time_ptr,       #channel_data+CHDAT_STOP_TIME
                    mov     volume_ptr,          #channel_data+CHDAT_VOLUME
                    mov     amp_env_ptr,         #channel_data+CHDAT_AMP_ENV
                    mov     env_seg_duration_ptr,#channel_data+CHDAT_ENV_SEG_DURATION
                    mov     lfsr_ptr,            #channel_data+CHDAT_LFSR
'                    mov     curr_ch_param_index,#INDEX_OF_CHANNEL0_PARAMS
                    mov     curr_channel,#0

                    add     sound_clock,#1           wz      'Increment Sound Clock
        if_z        add     sound_clock,#1                   'Skip zero, stop_time uses it to mean "never stop"

                    mov     temp,time_to_resume              'If time_to_resume-cnt is negative (ie. > $7FFF_FFFF),
                    sub     temp,#16                         'that means the loop took too long and cnt has
                    sub     temp,cnt                         'passed time_to_resume, so waitcnt should be skipped.
                    cmp     temp,long_half           wc      '(but subtract a few clocks from time_to_resume
                                                             'in case cnt passes it during this computation)

        if_b        waitcnt time_to_resume,delay_amount      'Delay
        if_ae       mov     time_to_resume,delay_amount      'Reset time_to_resume
        if_ae       add     time_to_resume,cnt
                    jmp     #loop                            'Loop


' // Shape Generation Routines ////////////////////////////////////////
' Note: These use a JMP/JMP protocol instead of JMPRET/JMP or CALL/RET
'       because they are only called from one line of code
' Return: Returns the sample in sample_temp

generate_shape_silent
                    mov     sample_temp,#0                        'Add nothing to mixed_sample
                    jmp     #return_from_shape

generate_shape_sine
                    add     active_channels,#1                    'Increment number of active channels
                    mov     sin,theta_temp                        'Compute sample from sine wave
                    call    #getsin
                    mov     sample_temp,sin
                    shl     sample_temp,#15                       'Correctly place the sign bit by shifting to the high word
                    sar     sample_temp,#16                       'Shift down to 16-bit
                    jmp     #return_from_shape

generate_shape_sawtooth
                    add     active_channels,#1                    'Increment number of active channels
                    mov     sample_temp,theta_temp

                    shl     sample_temp,#19                       'Start with a triangle wave ranged [$0,$FFF8_0000]
                    add     sample_temp,theta_temp                'Adjust range to [$0,$FFF8_1FFF]
                    abs     sample_temp,sample_temp               'Turn triangle into sawtooth [0,$7FFF_FFFF]
                    shr     sample_temp,#15                       'Adjust from 31-bit to 16-bit
                    sub     sample_temp,bit_15                    'Convert to signed sample

                    jmp     #return_from_shape

generate_shape_square
                    add     active_channels,#1                    'Increment number of active channels
                    cmp     theta_temp,sin_90     wc             'Compute sample from square wave
                    negc    sample_temp,word_half                 'Negate amplitude for half of each cycle
                    jmp     #return_from_shape

generate_shape_triangle
                    add     active_channels,#1                    'Increment number of active channels
                    mov     sample_temp,theta_temp                'Compute sample from trangular wave
                    sub     sample_temp,bit_12                    'Convert to signed sample (ie, adjust range from [$0,$1FFF] to [-$1000,$0FFF])
                    shl     sample_temp,#3                        'Adjust from 13-bit to 16-bit (including sign bit)
                    jmp     #return_from_shape

generate_shape_noise
                    add     active_channels,#1                    'Increment number of active channels
                    mov     sample_temp,lfsr_temp
                    tjz     theta_cycled_temp,#return_from_shape  'Only generate a sample once per cycle

'                    add     sample_temp,#1         '(lfsr + 1)
                    mov     temp,lfsr_temp
'                        mov     temp,cnt
'
' this code is intended to emulate the NES LFSR with taps at 15 and 14, for a 15 bit sequence
'
                        shr     temp,#1                 ' tap at 14
                        xor     temp,sample_temp        ' xor with tap at 15
                        and     temp,#1         
                        shl     temp,#15
                        shr     lfsr_temp,#1
                        or      temp,lfsr_temp
                        mov     sample_temp,temp
{
                    rol     temp,#2
                    xor     sample_temp,temp       '^(lfsr << 2)
                    rol     temp,#4
                    xor     sample_temp,temp       '^(lfsr << 6)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 7)
}
{
                    rol     temp,#15
                    xor     sample_temp,temp       '^(lfsr << 22)
                    rol     temp,#6
                    xor     sample_temp,temp       '^(lfsr << 28)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 29)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 30)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 31)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 32)
}

{                    rol     temp,#6
                    xor     sample_temp,cnt   ' temp       '^(lfsr << 6)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 7)
                    rol     temp,#22
                    xor     sample_temp,temp       '^(lfsr << 29)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 30)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 31)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 32)
}
{
                    rol     temp,#2
                    xor     sample_temp,temp       '^(lfsr << 2)
                    rol     temp,#2
                    xor     sample_temp,temp       '^(lfsr << 4)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 5)
                    rol     temp,#9
                    xor     sample_temp,temp       '^(lfsr << 14)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 15)
                    rol     temp,#1
                    xor     sample_temp,temp       '^(lfsr << 16)
}
                    mov     lfsr_temp,sample_temp  'store new lfsr state
                    shr     sample_temp,#16        'return high 16-bits
'                    and     sample_temp,word_max   'return low 16-bits
                    jmp     #return_from_shape

generate_shape_pcm_8bit_11khz
                    add     active_channels,#1              'Increment number of active channels

                    movs    pcm_load_pcm_curr,pcm_curr_ptr
                    movs    pcm_cmp_end,pcm_end_ptr         'Do something useful instead of nop
pcm_load_pcm_curr   mov     temp,0

                    'I could just "rdlong" once every 4 samples, but that
                    'would require an extra long of storage per channel.
                    rdbyte  sample_temp,temp

                    'Setup the sample
                    shl     sample_temp,#24                 'Correctly place the sign bit by shifting to the high byte
                    sar     sample_temp,#16                 'Shift down to 16-bit

                    'Advance pointer
                    movd    pcm_store_pcm_curr,pcm_curr_ptr
                    xor     temp,bit_31                     'Increment 1-bit counter
                    test    temp,bit_31               wz
           if_z     add     temp,#1                         'Only increment ptr every other iteration (ie: 22KHz -> 11KHz)
pcm_cmp_end         cmp     temp,0                    wz    'Compare pcm_curr with pcm_end
           if_e     movd    pcm_stop,shape_ptr
pcm_store_pcm_curr  mov     0,temp
pcm_stop   if_e     mov     0,#SHAPE_SILENT-1

                    jmp     #return_from_shape

'// Sine/Cosine Lookup Function ///////////////////////////////////////
'// from Hydra Programmer's Manual
'
' Get sine/cosine
'
'      quadrant:  1             2             3             4
'         angle:  $0000..$07FF  $0800..$0FFF  $1000..$17FF  $1800..$1FFF
'   table index:  $0000..$07FF  $0800..$0001  $0000..$07FF  $0800..$0001
'        mirror:  +offset       -offset       +offset       -offset
'          flip:  +sample       +sample       -sample       -sample
'
' on entry: sin[12..0] holds angle (0° to just under 360°)
' on exit:  sin holds signed value ranging from $0000FFFF ('1') to $FFFF0001 ('-1')
'
getcos          add     sin,sin_90              'for cosine, add 90°
getsin          test    sin,sin_90      wc      'get quadrant 2|4 into c
                test    sin,sin_180     wz      'get quadrant 3|4 into nz
                negc    sin,sin                 'if quadrant 2|4, negate offset
                or      sin,sin_table           'or in sin table address >> 1
                shl     sin,#1                  'shift left to get final word address
                rdword  sin,sin                 'read word sample from $E000 to $F000
                negnz   sin,sin                 'if quadrant 3|4, negate sample
getsin_ret
getcos_ret      ret                             '39..54 clocks
                                                '(variance is due to HUB sync on RDWORD)


sin_90          long    $0800   
sin_180         long    $1000
sin_360         long    $2000
sin_table       long    $E000 >> 1              'sine table base shifted right

sin             long    0


'// Multiplication Functions //////////////////////////////////////
'// adapted from Hydra Programmer's Manual
'
' Multiply 8-bit mult_x[7..0] by 4-bit mult_y[3..0] (mult_y[31..4] must be 0)
' on exit, product in mult_y[11..0] (mult_y[31..12] are zeros)
'
multiply_8_by_4_bit      shl     mult_x,#24                   'get multiplicand into mult_x[31..24]
                         shr     mult_y,#1         wc         'get initial multiplier bit into c

                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product

                         shr     mult_y,#20                   'shift product from mult_y[31..20] to mult_y[11..0]
multiply_8_by_4_bit_ret  ret                                  'return with product in mult_y[11..0] (mult_y[31..12] are zeros)

'
' Multiply 16-bit mult_x[15..0] by 12-bit mult_y[11..0] (mult_y[31..12] must be 0)
' on exit, product in mult_y[27..0] (mult_y[31..28] are zeros)
'
multiply_16_by_12_bit    shl     mult_x,#16                   'get multiplicand into mult_x[31..16]
                         shr     mult_y,#1         wc         'get initial multiplier bit into c

                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product
                 if_c    add     mult_y,mult_x     wc         'if c set, add multiplicand into product
                         rcr     mult_y,#1         wc         'get next multiplier bit into c, shift product

                         shr     mult_y,#4                    'shift product from mult_y[31..4] to mult_y[27..0]
multiply_16_by_12_bit_ret ret                                 'return with product in mult_y[27..0] (mult_y[31..28] are zeros)


mult_x                   long    0
mult_y                   long    0

'
'
' Divide t1 (18 bits)/m1 (18 bits), result in t2, remainder in t1
'
divide                  mov     t2,#0                   '
                        mov     m2,#1
prepare_divisor                                         ' more MSB of m1 to bit 12 place
                        test    m1,bit_18 wz
              if_z      shl     m1,#1
              if_z      add     m2,#1
              if_z      jmp     #prepare_divisor        ' this could loop indefinitely if m1 = 0
                        
:loop                   cmpsub  t1,m1           wc
                        rcl     t2,#1
                        shr     m1,#1
                        djnz    m2,#:loop

divide_ret              ret                             '+140

t2                      long    0
t1                      long    0
m1                      long    0
m2                      long    0'
'
'
'// Data ///////////////////////////////////////////////////////////////

delay_amount            long    80_000_000/SAMPLE_RATE
theta_delta_const       long    $07_48_59       ' $2000*1789773/Sample_Rate

dira_init               long    0
ctra_init               long    6<<26   'mode = duty single

active_channels         long    0       'The number of channels outputting a sound
mixed_sample            long    0       'The sum of samples from each channel

theta_ptr               long    channel_data+CHDAT_THETA        '$0000 = 0 degrees, $2000 = 360 degrees
theta_delta_ptr         long    channel_data+CHDAT_THETA_DELTA  'Formula: ($2000 * frequency) / SAMPLE_RATE
theta_cycled_ptr        long    channel_data+CHDAT_THETA_CYCLED '1 if theta has just completed a cycle, 0 otherwise
shape_ptr               long    channel_data+CHDAT_SHAPE        'Shape of the sound
stop_time_ptr           long    channel_data+CHDAT_STOP_TIME    'Stop the sound when sound_clock reaches this value, or 0 to play forever
volume_ptr              long    channel_data+CHDAT_VOLUME       'Volume in bits 0..7
amp_env_ptr             long    channel_data+CHDAT_AMP_ENV
env_seg_duration_ptr    long    channel_data+CHDAT_ENV_SEG_DURATION  'In sound ticks
lfsr_ptr                long    channel_data+CHDAT_LFSR         'Linear-Feedback Shift Register: Used to generate white noise
pcm_start_ptr           long    channel_data+CHDAT_PCM_START    'Address the PCM data starts at
pcm_end_ptr             long    channel_data+CHDAT_PCM_END      'Address the PCM data ends at (exclusive)
pcm_curr_ptr            long    channel_data+CHDAT_PCM_CURR     'Address of the current PCM sample

channel_data
                        long    0,0,0,0,0,0,0,0,21,0,0,0  'Channel 0
                        long    0,0,0,0,0,0,0,0,21,0,0,0  'Channel 1
                        long    0,0,0,0,0,0,0,0,21,0,0,0  'Channel 2
                        long    0,0,0,0,0,0,0,0,21,0,0,0  'Channel 3
end_of_channel_data

shape_jmp_ptr           long    0
shape_jmp_table         long    generate_shape_silent
                        long    generate_shape_sine
                        long    generate_shape_sawtooth
                        long    generate_shape_square
                        long    generate_shape_triangle
                        long    generate_shape_noise
                        long    generate_shape_pcm_8bit_11khz
                        
sound_clock             long    1                  'Increments at approx 22KHz (ie. once per driver iteration)
time_to_resume          long    0                  'Used with WAITCNT to synchronize iterations of main loop to 22KHz

curr_ch_param_index     long    INDEX_OF_CHANNEL0_PARAMS  'param_index of current channel

{_pending_shape          long    0
_pending_freq           long    0
_pending_duration       long    0
_pending_volume         long    0
_pending_amp_env        long    0
_pending_pcm_start      long    0
_pending_pcm_end        long    0
_nothing_pending        long    0}

'Literal Pool -
'A few commonly-needed values that are too big to use as an inline constant (ie. > 511)
long_max                long    $FFFF_FFFF   'The maximum value a long can hold
long_half               long    $7FFF_FFFF   'Half of the maximum value a long can hold
word_max                long    $0000_FFFF   'The maximum value a word can hold
word_half               long    $0000_7FFF   'Half of the maximum value a word can hold
bit_31                  long    1<<31        'Bit 31 = 1, the rest = 0
bit_28                  long    1<<28        'Bit 28 = 1, the rest = 0
bit_15                  long    1<<15        'Bit 15 = 1, the rest = 0
bit_12                  long    1<<12        'Bit 12 = 1, the rest = 0
bit_18                  long    1<<18
H07FF                   long    $07FF           ' mask for lower 11 bits
zero                    long    $0

delay_table             long    $05,$0A,$14,$28,$50,$1E,$07,$0D,$06,$0C,$18,$30,$60,$24,$08,$10
'
' noise channel wavelength conversion
noise_wl_convert        long    $2,$4,$8,$10,$20,$30,$40,$50,$65,$7F,$BE,$FE,$17D,$1FC,$3F9,$7F2

duration_mask           long    $1FFF_FFFF   'Masks off the "current envelope segment" bits. Only needed when duration is infinite.
'amp_env_sustain_set     long    AMP_ENV_SUSTAIN_SEG<<29

curr_vol                long    0           'The current 4-bit volume scaler from the envelope
curr_channel            long    0           'The channel currently being processed

temp                    long    0           'Just a scratchpad for calculations
temp1                   long    0
temp2                   long    0           'Just a scratchpad for calculations

sample_temp             long    0
theta_temp              long    0
theta_cycled_temp       long    0
lfsr_temp               long    0

APU_Channel_Status_Addr long    APU_DMA_Status_Ptr
APU_DMA_Addr            long    APU_DMA_Addr_Ptr