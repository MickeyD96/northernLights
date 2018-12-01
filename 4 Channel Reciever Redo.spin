CON
  _clkmode = xtal1 + pll16x                           
  _xinfreq = 5_000_000
    
CON

  _SpiIrq = 5
  _SpiMiso = 4
  _SpiMosi = 6
  _SpiSck = 3 
  _SpiCsn = 7     
  _SpiCe = 2

   _LargestPayloadSize = 34
  _DefaultPayloadSize = _LargestPayloadSize

  _AddressSizeOffset = 3
  _StatusOffset = 7
  _PayloadOffset = 8
  _RxAddressOffset = 9

VAR


  byte Light1,Light2,Light3,Light4
  byte CountX,ZeroC,Select,Channel1,Channel2,Channel3,Channel4,Channel5,Channel6,Channel7,Channel8 
  byte payloadSize
  LONG STACKSPACE[25],STACKSPACE2[25]
    
  word sumOfBytes 
  byte globalpayload[_LargestPayloadSize + 1]  
  byte bytesToAdd[2]

  
OBJ   
   
  Nordic : "NordicBeta110718a"                          'Nordic nRF24L01
  
PUB Demo | localIndex, sendWithOutRequestCount, sendRequestFlag 

COGNEW(PWM_LIGHTS(1),@STACKSPACE)
COGNEW(ZEROCROSSING(1),@STACKSPACE2)

  waitcnt(clkfreq * 2 + cnt)

  
  dira[0]~~
  dira[12..15]~
  
  Select:=ina[12..15]
  
  if (select == 0)
    Channel1:=25
    Channel2:=26   
    Channel3:=27
    Channel4:=28
    
  elseif (select == 1)
    Channel1:=29
    Channel2:=30   
    Channel3:=31
    Channel4:=32
  else
    Channel5:=((select*4)-7)
    Channel6:=((select*4)-6)   
    Channel7:=((select*4)-5)
    Channel8:=((select*4)-4)
  
  Nordic.Start(_SpiSck, _SpiMiso, _SpiMosi, _SpiCsn, _SpiCe, @configNew)
  payloadSize := byte[@configNew + _PayloadOffset] ' change whenever Nordic is configured. 
                         


  dira[_SpiIrq]~
 
  sendWithOutRequestCount := 0
  sendRequestFlag := 0
  
 repeat
  
    if ina[_SpiIrq]  == 0 'Wait for incoming data

      localIndex := Nordic.Status

      Nordic.ReadPayload(@globalpayload)     

      sendRequestFlag := 1
      
      if (select < 2)
        if (globalpayload[0] == 255)
          light1:=(globalpayload[Channel1])
          light2:=(globalpayload[Channel2])
          light3:=(globalpayload[Channel3])
          light4:=(globalpayload[Channel4])
      else
        if (globalpayload[0] == 253)
          light1:=(globalpayload[Channel5])
          light2:=(globalpayload[Channel6])
          light3:=(globalpayload[Channel7])
          light4:=(globalpayload[Channel8])     
                                      
 
PUB PWM_LIGHTS(TEMP1)
 
COUNTX:=81
DIRA[8..11]~~
OUTA[8..11]~~


REPEAT
 
   COUNTX--

   IF (LIGHT1)<COUNTX 
     OUTA[8]~~
   ELSE      
     OUTA[8]~

   IF (LIGHT2)<COUNTX 
     OUTA[9]~~
   ELSE
     OUTA[9]~

   IF (LIGHT3)<COUNTX 
     OUTA[10]~~
   ELSE
     OUTA[10]~

   IF (LIGHT4)<COUNTX 
     OUTA[11]~~
   ELSE
     OUTA[11]~
       
  IF ZEROC==1
     COUNTX:=81
     ZEROC:=0 

PUB ZEROCROSSING(TEMP)

REPEAT
 
     WAITPEQ(|<1,|<1,0)   
     ZEROC:=1
      
     WAITPNE(|<1,|<1,0)
     ZEROC:=1
 

     
DAT
                                       '1         2         3
                             '01234567890123456789012345678901
packetTemplate          byte "$00 + $00 = $0000   ", 0
whatsUpMessage          byte "Wake up over there! ", 0

testAddress   byte $AA, $BB, $CC, $DD, $EE

''------------------ Cofig settings for new nRF24L01+ modules ----------------------------------------
'' ----------------- Keep below together and in order ------------------------------------------------
configNew     byte %00111111 '%00111011  Register $00
              ' use %00111111($3F)(2 bytes CRC) or %00111011($3B)(1 byte CRC)
              ' when communicating with older nRF2401A modules
              ' make sure this matches nRF2401A setting
              ' %00001111($0F) future with more irq options
              ' 7 only zero allowed, 6 Mask irq by RX_DR (1 = no irq), 5 Mask TX_DS, 4 Mask MAX_RT,
              ' 3 Enable CRC, 2 CRC size (1 = 2 bytes), 1 1 = power up, 0 1 = PRX 0 = PTX
              ' RX_DR Receive Data Ready
              ' TX_DS Transmit Data Sent
              ' MAX_RT Maximum Retries
              byte $00 '$03 ' Register $01 Auto Acknowledge
              ' use $00 when communicating with older nRF2401A modules
              ' %00111111 future
              ' 7:6 only %00, enable pipe 5 through 0
              byte %00000011 ' Register $02
              ' 7:6 only zero allowed, 5:0 enable rx addresses pipe 5:0
              byte $03 ' Register $03 SETUP_AW setup Address Widths 
              ' 7:2 only %000000, 1:0 %01 = 3 %11 = 5
              byte $00 ' Register $04 SETUP_RETR Setup Auto Retransmission
              ' use $00 when communicating with older nRF2401A modules     
              ' %00101111 = 750us delay to retx, tx 15 tries
              ' %01001111 = 1250us delay to retx, tx 15 tries
              ' 7:4 ARD delay 1111 = 4000us, 3:0 retry count 0 - 15 (1111)
              byte $02 ' Register $05 RF_CH RF Channel
              ' 7 leave 0, 6:0 frequency channel
              ' F = (2400 + RF_CH)MHz (both nRF24L01+ and nRF2401A modules)
              byte %00000110 ' Register $06 RF_SETUP RF Setup
              ' don't use 2Mbps when communicating with older nRF2401A modules
              ' $06 = 1Mbps and full power
              ' 7:6 %00 only, 5 RF_DR_LOW 1 = 250kbps use 0 for higher speeds, 4 leave %0
              ' 3 1 = 2Mbps 0 = 1Mbps, 2:1 RF_POWER %11 = 0dBm (highest power), 0 don't care
              ' Register $07 STATUS Status Register used below.
              ' Register $08 OBSERVE_TX 7:4 lost packet count (read only),
              ' 3:0 retransmitted packets count (read only).  I plan to use this
              ' register's infomation in the future.
              ' Register $09 RPD 7:1 reserved (read only), 0 Received Power Detector (read only)
              ' Register $0A RX_ADDR_P0 39:0 Receive address pipe 0, 5 bytes maximum LSByte written first
              ' used below rxAddress0 and rxAddress1
              ' Register $0B RX_ADDR_P1 39:0 Receive address pipe 1
              ' Register $0C RX_ADDR_P2 7:0 Receive address pipe 2
              ' Register $0D RX_ADDR_P3 7:0 Receive address pipe 3
              ' Register $0E RX_ADDR_P4 7:0 Receive address pipe 4
              ' Register $0F RX_ADDR_P5 7:0 Receive address pipe 5
              ' Register $10 TX_ADDR 39:0 Transmit address
'configNew[7]
nordicStatus  byte %01000000 ' $40   %01110000   '$70
              ' only zero allowed, Data Ready RX, TX ACK received, MAX_RT failed,
              ' 3:1 Data pipe with payload %111 if empty (read only), 0 1= TX FIFO full (read only)
rxPayload     byte 32 '20 '25 '32   ' Register $11 RX_PW_P0
              ' This will need to be reduced to 25 (assuming 2 bytes CRC and 5 bytes address)
              ' to work with older nRF2401 modules.
              ' Newer nRF24L01+ modules can have payloads as large as 32 bytes.
              ' 7:6 only zero allowed, 5:0 Number of bytes in RX payload in data pipe 0
              ' (1 to 32)(0 = pipe not used)
              ' Register $12 RX_PW_P1 7:6 only zero allowed,
              ' 5:0 Number of bytes in RX payload in data pipe 1
              ' (1 to 32)(0 = pipe not used)
              ' Register $13 RX_PW_P2 7:6 only zero allowed,
              ' 5:0 Number of bytes in RX payload in data pipe 2
              ' Register $14 RX_PW_P3 7:6 only zero allowed,
              ' 5:0 Number of bytes in RX payload in data pipe 3
              ' Register $15 RX_PW_P4 7:6 only zero allowed,
              ' 5:0 Number of bytes in RX payload in data pipe 4
              ' Register $16 RX_PW_P5 7:6 only zero allowed,
              ' 5:0 Number of bytes in RX payload in data pipe 5
              ' Register $17 FIFO_STATUS 7 only zero allowed, 6 TX_REUSE (read only), 
              ' 5 TX_FULL %1 = full (read only), 4 TX_EMPTY %1 = empty (read only),
              ' 3:2 only zero allowed, 1 RX_FULL %1 = full (read only),
              ' 0 RX_EMPTY %1 = empty (read only)
              ' Register $1C DYNPD Enable dynamic payload length, 7:6 only zero allowed,
              ' 5 DPL_P5 Enable dynamic payload length data pipe 5 (Requires EN_DPL and ENAA_5)
              ' 4 DPL_P4 Enable dynamic payload length data pipe 4 (Requires EN_DPL and ENAA_4)
              ' 3 DPL_P3 Enable dynamic payload length data pipe 3 (Requires EN_DPL and ENAA_3)
              ' 2 DPL_P2 Enable dynamic payload length data pipe 2 (Requires EN_DPL and ENAA_2)
              ' 1 DPL_P1 Enable dynamic payload length data pipe 1 (Requires EN_DPL and ENAA_1)
              ' 0 DPL_P0 Enable dynamic payload length data pipe 0 (Requires EN_DPL and ENAA_0)
              ' Register $1D FEATURE Feature Register 7:3 only zero allowed,
              ' 2 EN_DPL Enables Dynamic Payload Length, 1 EN_ACK_PAY Enables Payload with ACK,
              ' 0 EN_DYN_ACK Enables the W_TX_PAYLOAD_NOACK command

rxAddress     byte $E7[5] ' Register $0A Receive address data pipe 0, LSByte first
txAddress     byte $E6[5] ' Register $10 Transmit address data pipe 0, LSByte first
'' ----------------- Keep above together and in order ------------------------------------------------

              