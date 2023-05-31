#include <stdio.h>
#include <stdint.h>

#include "d_event.h"
#include "d_main.h"
#include "v_video.h"
#include "i_video.h"

struct ccevent {
    uint8_t type;
    uint8_t key_button;
    uint16_t mousex;
    uint16_t mousey;
};

uint8_t * _screen = (uint8_t*)0x2000000; // 320x200
uint8_t * _palette = (uint8_t*)0x200FA00; // 256x3 RGB

extern void setGraphicsMode(int mode);
extern void updatePalette();
extern void updateScreen();
extern int getEvent(struct ccevent * ev);

// Called by D_DoomMain,
// determines the hardware configuration
// and sets up the video mode
void I_InitGraphics (void) {
    setGraphicsMode(2);
    screens[0] = _screen;
}


void I_ShutdownGraphics(void) {
    setGraphicsMode(0);
}

// Takes full 8 bit values.
void I_SetPalette (byte* palette) {
    memcpy(_palette, palette, 768);
    updatePalette();
}

void I_UpdateNoBlit (void) {
    
}

void I_FinishUpdate (void) {
    //memcpy(_screen, screens[0], SCREENWIDTH*SCREENHEIGHT);
    updateScreen();
}

void I_ReadScreen (byte* scr) {
    memcpy(scr, _screen, 64000);
}

static int buttonMask = 0;
void I_StartTic (void) {
    struct ccevent ev;
    event_t event;
    while (getEvent(&ev)) {
        switch (ev.type) {
            case 0: // key down
                event.type = ev_keydown;
                event.data1 = ev.key_button;
                D_PostEvent(&event);
                //printf("key %d\n", ev.key_button);
                break;
            case 1: // key up
                event.type = ev_keyup;
                event.data1 = ev.key_button;
                D_PostEvent(&event);
                break;
            case 2: // mouse down
                buttonMask |= 1 << ev.key_button;
                goto mouse_event;
            case 3: // mouse up
                buttonMask &= ~(1 << ev.key_button);
            case 4: mouse_event: // mouse drag/move
                event.type = ev_mouse;
                event.data1 = buttonMask;
                event.data2 = ev.mousex;
                event.data3 = ev.mousey;
                D_PostEvent(&event);
                break;
        }
    }
}

//
// I_StartFrame
//
void I_StartFrame (void)
{
    // er?

}