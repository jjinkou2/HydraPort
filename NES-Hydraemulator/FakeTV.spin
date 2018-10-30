VAR

  long  vga_status      'status: off/visible/invisible  read-only       (21 contiguous longs)
  long  vga_enable      'enable: off/on                 write-only
  long  vga_pins        'pins: byte(2),topbit(3)        write-only
  long  vga_mode        'mode: interlace,hpol,vpol      write-only
  long  vga_videobase   'video base @word               write-only
  long  vga_colorbase   'color base @long               write-only              
  long  vga_hc          'horizontal cells               write-only
  long  vga_vc          'vertical cells                 write-only
  long  vga_hx          'horizontal cell expansion      write-only
  long  vga_vx          'vertical cell expansion        write-only
  long  vga_ho          'horizontal offset              write-only
  long  vga_vo          'vertical offset                write-only
  long  vga_hd          'horizontal display pixels      write-only
  long  vga_hf          'horizontal front-porch pixels  write-only
  long  vga_hs          'horizontal sync pixels         write-only
  long  vga_hb          'horizontal back-porch pixels   write-only
  long  vga_vd          'vertical display lines         write-only
  long  vga_vf          'vertical front-porch lines     write-only
  long  vga_vs          'vertical sync lines            write-only
  long  vga_vb          'vertical back-porch lines      write-only
  long  vga_rate        'pixel rate (Hz)                write-only

  long  tv_status     '0/1/2 = off/visible/invisible           read-only       (14 contiguous longs)
  long  tv_enable     '0/? = off/on                            write-only
  long  tv_pins       '%ppmmm = pins                           write-only
  long  tv_mode       '%ccinp = chroma,interlace,ntsc/pal,swap write-only
  long  tv_screen     'pointer to screen (words)               write-only
  long  tv_colors     'pointer to colors (longs)               write-only               
  long  tv_hc         'horizontal cells                        write-only
  long  tv_vc         'vertical cells                          write-only
  long  tv_hx         'horizontal cell expansion               write-only
  long  tv_vx         'vertical cell expansion                 write-only
  long  tv_ho         'horizontal offset                       write-only
  long  tv_vo         'vertical offset                         write-only
  long  tv_broadcast  'broadcast frequency (Hz)                write-only
  long  tv_auralcog   'aural fm cog                            write-only

  long  ex

OBJ

  vga:"VGA"

PUB start_of_tv_driver 
RETURN vga.start_of_vga_driver

PUB start(tvptr):okay

  longmove(@tv_status,tvptr,14)
  'longmove(@vga_status,@vgaparams,21)
  
  'if tv_hc<16 and tv_vc<12    'didn't figure out the max size for 3 ex yet
  '  ex:=3
  'elseif tv_hc<16 and tv_vc<12
  if tv_hc<16 and tv_vc<12
    ex:=2
  else
    ex:=1
  
  vga_status:=tv_status
  vga_enable:=tv_enable
  vga_pins:=%001_111
  vga_mode:=tv_mode&8|%0011
  vga_videobase:=tv_screen
  vga_colorbase:=tv_colors
  vga_hc:=tv_hc       
  vga_vc:=tv_vc       
  vga_hx:=ex'tv_hx*ex/10       
  vga_vx:=ex'tv_vx*ex       
  vga_ho:=tv_ho       
  vga_vo:=tv_vo
  vga_hd:=512       
  vga_hf:=16       
  vga_hs:=96       
  vga_hb:=48       
  vga_vd:=380       
  vga_vf:=11       
  vga_vs:=2       
  vga_vb:=31       
  vga_rate:=20_000_000
  okay:=vga.start(@vga_status)

PUB stop

  vga.stop       

{  
DAT
  
vgaparams               long    0               'status
                        long    1               'enable
                        long    %001_111        'pins
                        long    %0011           'mode
                        long    0               'videobase
                        long    0               'colorbase
                        long    16              'hc
                        long    12              'vc
                        long    1               'hx
                        long    1               'vx
                        long    0               'ho
                        long    0               'vo
                        long    512             'hd
                        long    16              'hf
                        long    96              'hs
                        long    48              'hb
                        long    380             'vd
                        long    11              'vf
                        long    2               'vs
                        long    31              'vb
                        long    20_000_000      'rate
}                     