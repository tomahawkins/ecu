#include <canlib.h>

void initCAN() { canInitializeLibrary(); }

/* canOpenExt_
 * open channel for extended CAN frames
 * set baud rate to 250 Kbit/s
 * typically used for refuse (J1939)
 */
int canOpenExt_ (int channel) {
  int h;
  h = canOpenChannel(channel, canWANT_EXTENDED);
  canSetBusParams(h, BAUD_250K, 4, 3, 1, 1, 0);
  canSetBusOutputControl(h, canDRIVER_NORMAL);
  canBusOn(h);
  return h;
}

/* canOpenStd_
 * open channel for standard CAN frames
 * set baud rate to 500 Kbit/s
 * typically used for shuttle
 */
int canOpenStd_ (int channel) {
  int h;
  h = canOpenChannel(channel, 0);
  canSetBusParams(h, BAUD_500K, 4, 3, 1, 1, 0);
  canSetBusOutputControl(h, canDRIVER_NORMAL);
  canBusOn(h);
  return h;
}

/*
int canOpenVirtual_ (int channel) {
  int h;
  //h = canOpenChannel(channel, canOPEN_NO_INIT_ACCESS);
  h = canOpenChannel(channel, canOPEN_ACCEPT_VIRTUAL);
  //h = canOpenChannel(channel, 0);
  canSetBusParams(h, BAUD_250K, 6, 2, 1, 0, 0);
  //canSetBusOutputControl(h, canDRIVER_NORMAL);
  canBusOn(h);
  return h;
}
*/

void canClose_ (int h) {
  //canBusOff(h); 
  canClose(h); 
}

long id;
unsigned long time;
unsigned int dlc;
unsigned int flags;
unsigned char msg[8];

int canRead_     (int handle)             { return (int) canRead     (handle, &id, msg, &dlc, &flags, &time);    }
int canReadWait_ (int handle, long int w) { return (int) canReadWait (handle, &id, msg, &dlc, &flags, &time, w); }

int           canReadId_()       { return id;        }
int           canReadTime_()     { return time;      }
int           canReadMsgLen_()   { return (int) dlc; }
unsigned char canReadMsg_(int i) { return msg[i];    }

void canWriteByte_(char byte, int i) { msg[i] = byte; }

void canWrite_(int h, int id, int dlc) { canWrite(h, (long) id, msg, (unsigned int) dlc, canMSG_EXT); }

int canReadTimer_(int handle) {
#ifdef _WIN32
  time = canReadTimer(handle);
  return 0;
#else
  return canReadTimer(handle, &time);
#endif
}

int canFlushReceiveQueue_(int handle) { return (int) canIoCtl(handle, canIOCTL_FLUSH_RX_BUFFER, NULL, 0); } 
