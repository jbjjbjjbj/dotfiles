import os
import tkinter as tk
import time
root = tk.Tk()
root.withdraw()

control_c_sequence = '''keydown Shift_L;
key Left;
keyup Shift_L;
keydown Control_L;
key x;
keyup Control_L
'''

left = 'key Left'
right = 'key Right'

def keypress(sequence):
    #print(sequence.replace('\n', ''))
    for item in sequence.replace('\n', '').split(';'):
        print("xdotool " + item)
        os.system("xdotool " + item)
        

keypress(right)
while(True):
    keypress(control_c_sequence)
    keypress("keydown Shift_L key " + root.clipboard_get() + " keyup Shift_L")
    keypress(right)
    keypress(right)
