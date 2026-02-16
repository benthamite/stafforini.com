---
title: "What to do if your Thinkpad T520 won't power on"
date: 2013-06-02T14:42:13
lastmod: 2013-06-02T14:42:13
categories: ["Misc"]
---

Recently, my Lenovo Thinkpad T520 died. Fixing this problem cost me two days of work, and much anxiety and frustration. Since not much information about this particular issue seems to be available online, I thought I should write a post detailing my solution, in the hope that it might help others solve it more quickly and with less stress. It is, of course, possible that the solution won't work for you, since the problem might be caused by some other factor. It is also possible that the solution will work for you even if you have a different laptop model.

**The symptoms**

- After the laptop wakes up from sleep mode, it displays a notice indicating that it is about to run out of battery, and automatically goes back to sleep again.
- When you press the power button, after connecting it to an external power source, the computer won't go out of sleep mode. When you press the power button for a longer time, the computer won't turn off.
- The computer will only turn off if you unplug it and remove the battery. If, after plugging it back in or reinserting the battery, you press the power button, the power button will lit and the hard drive fan might start spinning, but there will be no other signs of activity (the system won't boot, the BIOS won't load, and the screen will be black).
- As before, the computer won't turn off when you press the power button, no matter how long you press.
- The battery indicator might blink two or three times. (I vaguely remember this being the case, but am not entirely sure.)

**The diagnosis**

The [CMOS battery](https://en.wikipedia.org/wiki/Nonvolatile_BIOS_memory) (also called *backup battery*) is malfunctioning.

**The treatment**

1. Download the [ThinkPad T520 Hardware Maintenance Manual](http://download.lenovo.com/ibmdl/pub/pc/pccbbs/mobiles_pdf/0a60078_01.pdf) (or, if you have a different laptop model, download the corresponding manual [here](http://support.lenovo.com/en_US/guides-and-manuals/default.page)).
2. Scroll down to section '1120 Backup Battery' (p. 78).
3. Follow the four steps listed there, in order. These are steps (4)-(7) below:
4. Remove battery ('1010 Battery pack', pp. 60-61).
5. Remove DIM slot cover ('1030 DIM slot cover', pp. 62-63).
6. Remove keyboard ('1030 keyboard', pp. 67-70).
7. Remove keyboard bezel assembly ('1100 keyboard bezel assembly', pp. 75-77).
8. Now you are ready to unplug the CMOS battery, as indicated on p. 79. You don't need to physically remove the battery, just unplug it (i.e., complete only step 2 in the image, ignoring step 1).
9. Leave the CMOS battery unplugged for 15 hours, then plug it back in.
10. Re-assemble your laptop, reversing steps (4)-(7) above.
11. You are done. If the treatment worked, your computer should boot normally after you press the power button.

If the treatment doesn't work, I'm sorry. These [two](http://forums.lenovo.com/t5/T400-T500-and-newer-T-series/T420s-No-power-blinking-power-Led-won-t-boot/td-p/684845) [threads](http://forums.lenovo.com/t5/T400-T500-and-newer-T-series/T520-4239-failing-to-power-on/td-p/841481) mention some alternative solutions you may want to try. Also, feel free to leave a comment here; I probably won't be able to assist you, but other more experienced readers might.
