/**
 * Sidenotes — transforms standard Goldmark footnotes into margin notes.
 *
 * On wide screens (≥ BREAKPOINT), footnote content is extracted from the
 * bottom-of-page footnotes section and positioned in a right-margin column
 * alongside the corresponding reference in the text.
 *
 * Dense sidenotes overlap with a fade-out gradient; hovering expands the
 * occluded note and hides notes it overlaps.
 */

(function () {
  'use strict';

  // ── Constants ───────────────────────────────────────────────────
  var BREAKPOINT = 1100;
  var MIN_SHOWN_LINES = 3;
  var MIN_SPACING_LINES = 1.5;
  var FADEOUT_LINES = 1.3;
  var RESIZE_DEBOUNCE_MS = 150;

  // ── DOM references ──────────────────────────────────────────────
  var footnotesSection = document.querySelector('.footnotes');
  var sidenoteColumn = document.querySelector('.sidenote-column');

  if (!footnotesSection || !sidenoteColumn) return;

  var footnoteLis = footnotesSection.querySelectorAll('li[id^="fn:"]');
  if (footnoteLis.length === 0) return;

  // ── Build sidenote elements ─────────────────────────────────────
  var sidenotes = [];
  var refElements = [];

  footnoteLis.forEach(function (li) {
    var id = li.id; // e.g. "fn:1"
    var num = id.replace('fn:', '');

    // Find the corresponding reference in the text
    var ref = document.getElementById('fnref:' + num);
    // Ensure we have the sup wrapper (or its closest ancestor)
    if (ref && ref.tagName !== 'SUP') ref = ref.closest('sup') || ref;
    if (!ref) return;

    // Clone content, remove backref link
    var content = li.cloneNode(true);
    var backrefs = content.querySelectorAll('.footnote-backref');
    backrefs.forEach(function (br) { br.remove(); });
    // Remove the li wrapper id to avoid duplicate IDs
    content.removeAttribute('id');

    // Build sidenote DOM
    var sidenote = document.createElement('div');
    sidenote.className = 'sidenote';
    sidenote.setAttribute('data-footnote', num);
    sidenote.setAttribute('role', 'note');
    sidenote.setAttribute('aria-label', 'Sidenote ' + num);

    var numberSpan = document.createElement('span');
    numberSpan.className = 'sidenote-number';
    numberSpan.textContent = num;

    var contentDiv = document.createElement('div');
    contentDiv.className = 'sidenote-content';
    // Move children from cloned li into contentDiv
    while (content.firstChild) {
      contentDiv.appendChild(content.firstChild);
    }

    sidenote.appendChild(numberSpan);
    sidenote.appendChild(contentDiv);
    sidenoteColumn.appendChild(sidenote);

    sidenotes.push({
      el: sidenote,
      contentEl: contentDiv,
      num: num,
      ref: ref,
      fullHeight: 0,
      top: 0,
      visibleHeight: null,
      overlaps: []
    });
    refElements.push(ref);
  });

  if (sidenotes.length === 0) return;

  // ── Computed values ─────────────────────────────────────────────
  var lineHeight;

  function computeLineHeight() {
    var style = getComputedStyle(sidenotes[0].el);
    var lh = parseFloat(style.lineHeight);
    // getComputedStyle returns lineHeight in px (or "normal" which parseFloat → NaN)
    if (isNaN(lh)) {
      lh = parseFloat(style.fontSize) * 1.2;
    }
    lineHeight = lh;
  }

  // ── Positioning algorithm ───────────────────────────────────────

  function positionSidenotes() {
    computeLineHeight();

    var minSpacing = MIN_SPACING_LINES * lineHeight;
    var minHeight = MIN_SHOWN_LINES * lineHeight;
    var fadeoutHeight = FADEOUT_LINES * lineHeight;

    // The positioned container that holds the content and the sidenote column
    var container = sidenoteColumn.closest('.content-with-sidenotes');
    if (!container) return;

    // Use getBoundingClientRect for accurate positioning of inline elements
    var containerRect = container.getBoundingClientRect();
    sidenotes.forEach(function (sn) {
      var refRect = sn.ref.getBoundingClientRect();
      sn.refTop = refRect.top - containerRect.top;
    });

    // Temporarily remove height constraints to measure full heights
    sidenotes.forEach(function (sn) {
      sn.el.style.height = '';
      sn.el.style.overflow = '';
      // Remove old fadeout/cover elements
      var oldFadeout = sn.el.querySelector('.sidenote-fadeout');
      var oldCover = sn.el.querySelector('.sidenote-cover');
      if (oldFadeout) oldFadeout.remove();
      if (oldCover) oldCover.remove();
    });

    // Force layout so measurements are correct
    sidenoteColumn.offsetHeight;

    sidenotes.forEach(function (sn) {
      sn.fullHeight = sn.el.scrollHeight;
      sn.visibleHeight = null;
      sn.overlaps = [];
    });

    // Compute top positions: always align with the reference; only push down
    // if the previous sidenote's minimum visible area would be violated.
    for (var i = 0; i < sidenotes.length; i++) {
      var sn = sidenotes[i];

      if (i === 0) {
        sn.top = sn.refTop;
      } else {
        var prev = sidenotes[i - 1];
        // The earliest this sidenote can start: previous note must show
        // at least MIN_SHOWN_LINES before being occluded.
        var minPrevEnd = prev.top + minHeight;
        sn.top = Math.max(sn.refTop, minPrevEnd);

        // Check if previous sidenote is partially occluded by this one
        if (prev.top + prev.fullHeight > sn.top) {
          var availableSpace = sn.top - prev.top;
          var linesAvailable = Math.floor(availableSpace / lineHeight);
          var visibleLines = Math.max(linesAvailable, MIN_SHOWN_LINES);
          prev.visibleHeight = visibleLines * lineHeight;
        }
      }
    }

    // Apply positions and overlap layers
    sidenotes.forEach(function (sn) {
      sn.el.style.top = sn.top + 'px';

      if (sn.visibleHeight !== null && sn.visibleHeight < sn.fullHeight) {
        sn.el.style.height = sn.visibleHeight + 'px';
        sn.el.style.overflow = 'hidden';

        // Add fade-out gradient
        var fadeout = document.createElement('div');
        fadeout.className = 'sidenote-fadeout';
        fadeout.style.top = (sn.visibleHeight - fadeoutHeight) + 'px';
        fadeout.style.height = fadeoutHeight + 'px';
        sn.el.appendChild(fadeout);

        // Add solid cover below visible area
        var cover = document.createElement('div');
        cover.className = 'sidenote-cover';
        cover.style.top = sn.visibleHeight + 'px';
        cover.style.height = (sn.fullHeight - sn.visibleHeight) + 'px';
        sn.el.appendChild(cover);
      }
    });

    // Compute overlap sets (which notes overlap when one expands)
    for (var i = 0; i < sidenotes.length; i++) {
      sidenotes[i].overlaps = [];
      for (var j = i + 1; j < sidenotes.length; j++) {
        if (sidenotes[i].top + sidenotes[i].fullHeight > sidenotes[j].top) {
          sidenotes[i].overlaps.push(j);
        } else {
          break; // Notes are ordered, no further overlaps possible
        }
      }
    }
  }

  // ── Hover behavior ─────────────────────────────────────────────

  function findRefLink(ref) {
    // The ref might be a <sup> containing an <a>, or an <a> directly
    return ref.tagName === 'A' ? ref : ref.querySelector('a');
  }

  function activateSidenote(index) {
    var sn = sidenotes[index];
    sn.el.classList.add('is-hovered');

    // Expand to full height
    if (sn.visibleHeight !== null) {
      sn.el.style.height = sn.fullHeight + 'px';
      sn.el.style.overflow = 'visible';
    }

    // Hide notes this one overlaps
    sn.overlaps.forEach(function (j) {
      sidenotes[j].el.classList.add('is-hidden-by-hover');
    });

    // Highlight the reference
    var refLink = findRefLink(sn.ref);
    if (refLink) refLink.classList.add('is-active');
  }

  function deactivateSidenote(index) {
    var sn = sidenotes[index];
    sn.el.classList.remove('is-hovered');

    // Restore constrained height
    if (sn.visibleHeight !== null) {
      sn.el.style.height = sn.visibleHeight + 'px';
      sn.el.style.overflow = 'hidden';
    }

    // Show hidden notes
    sn.overlaps.forEach(function (j) {
      sidenotes[j].el.classList.remove('is-hidden-by-hover');
    });

    // Un-highlight the reference
    var refLink = findRefLink(sn.ref);
    if (refLink) refLink.classList.remove('is-active');
  }

  // Sidenote hover
  sidenotes.forEach(function (sn, i) {
    sn.el.addEventListener('mouseenter', function () { activateSidenote(i); });
    sn.el.addEventListener('mouseleave', function () { deactivateSidenote(i); });
  });

  // Footnote ref hover
  sidenotes.forEach(function (sn, i) {
    var refLink = findRefLink(sn.ref);
    if (!refLink) return;

    refLink.addEventListener('mouseenter', function () { activateSidenote(i); });
    refLink.addEventListener('mouseleave', function () { deactivateSidenote(i); });
  });

  // ── Activation / deactivation based on viewport width ──────────

  var isActive = false;

  function activate() {
    if (isActive) return;
    isActive = true;
    document.body.classList.add('has-sidenotes');
    positionSidenotes();
  }

  function deactivate() {
    if (!isActive) return;
    isActive = false;
    document.body.classList.remove('has-sidenotes');

    // Reset inline styles
    sidenotes.forEach(function (sn) {
      sn.el.style.top = '';
      sn.el.style.height = '';
      sn.el.style.overflow = '';
      var oldFadeout = sn.el.querySelector('.sidenote-fadeout');
      var oldCover = sn.el.querySelector('.sidenote-cover');
      if (oldFadeout) oldFadeout.remove();
      if (oldCover) oldCover.remove();
    });
  }

  function checkViewport() {
    if (window.innerWidth >= BREAKPOINT) {
      activate();
    } else {
      deactivate();
    }
  }

  // ── Resize handling ─────────────────────────────────────────────

  var resizeTimer;
  window.addEventListener('resize', function () {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function () {
      var wasActive = isActive;
      checkViewport();
      if (isActive && wasActive) {
        // Reposition — text may have reflowed
        positionSidenotes();
      }
    }, RESIZE_DEBOUNCE_MS);
  });

  // ── Initial activation ─────────────────────────────────────────
  checkViewport();
})();
