#include <stdio.h>
#include <stdlib.h>
#include <linux/input.h>

int main(void) {
    setbuf(stdin, NULL); setbuf(stdout, NULL);

    struct input_event event;
    while (fread(&event, sizeof(event), 1, stdin)) {
        if (event.type == EV_KEY) {
            switch (event.code) {
                case KEY_CAPSLOCK:
                    event.code = KEY_LEFTCTRL;
                    break;
                case KEY_LEFTALT:
                    event.code = KEY_LEFTMETA;
                    break;
                case KEY_102ND:
                    event.code = KEY_LEFTALT;
                    break;
            }
        }

        fwrite(&event, sizeof(event), 1, stdout);
    }
}

