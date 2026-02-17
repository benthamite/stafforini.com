+++
title = "How to transcribe recordings"
author = ["Pablo Stafforini"]
date = 2013-09-24
draft = false
+++

Over the past few months, I've spent quite some time transcribing talks and conversations by [effective altruists](http://effective-altruism.com/), and have in the process gained some experience from which I think others might benefit.  The instructions below were originally written for a friend.

You may transcribe the recording yourself, or you may pay other people to do the transcribing.


## Doing it yourself {#doing-it-yourself}

Though I've never calculated how long it takes me to transcribe a conversation of a given duration, it is generally estimated that it takes five hours of work to transcribe an hour of audio. With only a few exceptions, these days I only transcribe myself conversations that contain confidential information, since the costs of paying someone else are comparatively low (see below). Here's how I do it:

1.  Download and install [Foobar2000](http://www.foobar2000.org/).
2.  On the menu, go to Library &gt; Configuration, and click on Keyboard Shortcuts, then on Add new.
3.  On the 'Filter list by' search box, enter 'Back by 1 second', and click on that option (under [main] &gt; Playback &gt; Seek).
4.  On the 'Key' field, press Shift+Ctrl+Win+Left arrow.
5.  Tick the 'Global hotkey' box.
6.  Repeat steps 3-5, entering 'Play or pause' on the search box and Shift+Ctrl+Win+Right arrow on the 'Key' field. Remember to tick the 'Global hotkey' box.
7.  Start the recording you want to transcribe.
8.  Now you can keep Foobar2000 minimized, and focus solely on the document where you are typing the transcription. You can play or pause the recording by pressing Shift+Ctrl+Win+Right arrow, and rewind the recording by one second by pressing Shift+Ctrl+Win+Left arrow.


## Paying others to do it for you {#paying-others-to-do-it-for-you}

> "In some countries, there are people whose job it is to waste their own time for people who can afford to pay not to waste their own." (Robert Levine, _The Power of Persuasion_, New York: John Wiley &amp; Sons, 2003, p. 74.)

Satvik Beri first drew my attention to Amazon Mechanical Turk, where one can hire workers to transcribe audio recordings for about USD 0.30 per minute. [This blog post](http://waxy.org/2008/09/audio_transcription_with_mechanical_turk/) provides detailed instructions on how to do it. Alternatively, you can use specialized sites like [Rev](http://www.rev.com/transcription), which charge about USD 1 per minute. I have no experience with Rev, and strongly recommend trying mTurk before exploring other, more expensive options.

UPDATE (December 2022): I am told that OpenAI's audio transcriber, [Whisper](https://github.com/openai/whisper), is close to human-level. I recommend exploring this option before relying on human labor, whether your own or others'.
