/**
 * Table of contents — scroll spy, title fade-in, and smooth scrolling.
 *
 * Active only at >=1400px (floating TOC). At narrower viewports the TOC
 * is either inline or collapsed and does not need scroll spy.
 */
(function () {
  'use strict';

  // ── Constants ───────────────────────────────────────────────────
  var BREAKPOINT = 1400;
  var RESIZE_DEBOUNCE_MS = 150;

  // ── DOM references ──────────────────────────────────────────────
  var floatingNav = document.querySelector('.toc-floating-nav');
  var titleLink = document.querySelector('.toc-title');
  var articleH1 = document.querySelector('article h1');

  if (!floatingNav) return;

  var tocLinks = floatingNav.querySelectorAll('a:not(.toc-title)');
  if (tocLinks.length === 0) return;

  // Map each TOC link to its corresponding heading element.
  // Pages can have duplicate IDs (e.g. two headings named "misc"), so we
  // pair by occurrence order rather than using getElementById.
  var headings = []; // Array of { el: HTMLElement, link: HTMLAnchorElement }
  var elToIndex = new Map();

  (function buildHeadingMap() {
    var idCount = {};
    tocLinks.forEach(function (link) {
      var href = link.getAttribute('href');
      if (!href || !href.startsWith('#')) return;
      var id = href.slice(1);
      var n = idCount[id] || 0;
      idCount[id] = n + 1;
      var matches = document.querySelectorAll('[id="' + CSS.escape(id) + '"]');
      var el = matches[n];
      if (el) {
        elToIndex.set(el, headings.length);
        headings.push({ el: el, link: link });
      }
    });
  })();

  if (headings.length === 0) return;

  // ── Scroll spy ────────────────────────────────────────────────
  var headingObserver = null;
  var titleObserver = null;
  var currentActive = -1;

  function setActive(index) {
    if (currentActive === index) return;
    currentActive = index;

    headings.forEach(function (h, i) {
      if (i === index) {
        h.link.classList.add('toc-active');
        // Auto-scroll the TOC nav so the active link stays visible
        if (floatingNav.scrollHeight > floatingNav.clientHeight) {
          h.link.scrollIntoView({ block: 'nearest' });
        }
      } else {
        h.link.classList.remove('toc-active');
      }
    });
  }

  function clearActive() {
    currentActive = -1;
    headings.forEach(function (h) {
      h.link.classList.remove('toc-active');
    });
  }

  function createHeadingObserver() {
    // 1px "tripwire" at 1/3 viewport height — headings crossing this line
    // become the active TOC entry
    var TRIGGER_FRACTION = 1 / 3;
    var triggerPoint = Math.round(window.innerHeight * TRIGGER_FRACTION);
    var rootMarginTop = -triggerPoint;
    var rootMarginBottom = -(window.innerHeight - triggerPoint - 1);

    return new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            var index = elToIndex.get(entry.target);
            if (index !== undefined) setActive(index);
          }
        });
      },
      {
        rootMargin: rootMarginTop + 'px 0px ' + rootMarginBottom + 'px 0px',
        threshold: 0
      }
    );
  }

  function createTitleObserver() {
    if (!titleLink || !articleH1) return null;

    return new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            titleLink.classList.remove('is-visible');
          } else {
            titleLink.classList.add('is-visible');
          }
        });
      },
      { threshold: 0 }
    );
  }

  // ── Smooth scrolling ─────────────────────────────────────────
  var prefersReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');
  var SCROLL_DURATION = 500; // ms — duration of animated scroll to heading

  function easeInOutCubic(t) {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }

  var scrollAnimationId = null;

  function animateScrollTo(targetY) {
    if (scrollAnimationId) cancelAnimationFrame(scrollAnimationId);
    var startY = window.scrollY;
    var diff = targetY - startY;
    if (diff === 0) return;
    var startTime = null;

    function step(time) {
      if (!startTime) startTime = time;
      var progress = Math.min((time - startTime) / SCROLL_DURATION, 1);
      window.scrollTo(0, startY + diff * easeInOutCubic(progress));
      if (progress < 1) {
        scrollAnimationId = requestAnimationFrame(step);
      } else {
        scrollAnimationId = null;
      }
    }

    scrollAnimationId = requestAnimationFrame(step);
  }

  // Build a map from TOC link elements to their heading elements for click handling.
  // This covers all TOC variants (floating, inline, collapsible).
  var linkToEl = new Map();
  document.querySelectorAll('.toc-floating-nav, .toc-inline, .toc-collapsible-nav').forEach(function (nav) {
    var idCount = {};
    nav.querySelectorAll('a:not(.toc-title)').forEach(function (link) {
      var href = link.getAttribute('href');
      if (!href || !href.startsWith('#')) return;
      var id = href.slice(1);
      var n = idCount[id] || 0;
      idCount[id] = n + 1;
      var matches = document.querySelectorAll('[id="' + CSS.escape(id) + '"]');
      var el = matches[n];
      if (el) linkToEl.set(link, el);
    });
  });

  function handleTocClick(e) {
    var link = e.target.closest('a');
    if (!link) return;

    var href = link.getAttribute('href');
    if (!href || !href.startsWith('#')) return;

    // Scroll-to-top for title link
    if (href === '#' && link.classList.contains('toc-title')) {
      e.preventDefault();
      if (prefersReducedMotion.matches) {
        window.scrollTo(0, 0);
      } else {
        animateScrollTo(0);
      }
      return;
    }

    var target = linkToEl.get(link);
    if (!target) return;
    e.preventDefault();

    if (prefersReducedMotion.matches) {
      target.scrollIntoView();
    } else {
      var targetY = target.getBoundingClientRect().top + window.scrollY;
      animateScrollTo(targetY);
    }
  }

  // Attach click handlers to all TOC variants
  document.querySelectorAll('.toc-floating-nav, .toc-inline, .toc-collapsible-nav').forEach(function (nav) {
    nav.addEventListener('click', handleTocClick);
  });

  // ── Activation / deactivation ─────────────────────────────────
  var isActive = false;

  // Debounced scroll fallback for fast scrolling (IntersectionObserver may miss headings)
  var scrollFallbackTimer;
  function startScrollFallback() {
    window.addEventListener('scroll', onScrollFallback, { passive: true });
  }
  function stopScrollFallback() {
    window.removeEventListener('scroll', onScrollFallback);
    clearTimeout(scrollFallbackTimer);
  }
  var SCROLL_FALLBACK_DEBOUNCE_MS = 100;
  function onScrollFallback() {
    clearTimeout(scrollFallbackTimer);
    scrollFallbackTimer = setTimeout(function () {
      var triggerY = window.scrollY + window.innerHeight / 3;
      var bestIndex = -1;
      headings.forEach(function (h, i) {
        var top = h.el.getBoundingClientRect().top + window.scrollY;
        if (top <= triggerY) bestIndex = i;
      });
      if (bestIndex >= 0) setActive(bestIndex);
    }, SCROLL_FALLBACK_DEBOUNCE_MS);
  }

  function activate() {
    if (isActive) return;
    isActive = true;

    headingObserver = createHeadingObserver();
    headings.forEach(function (h) {
      headingObserver.observe(h.el);
    });

    titleObserver = createTitleObserver();
    if (titleObserver && articleH1) {
      titleObserver.observe(articleH1);
    }

    startScrollFallback();
  }

  function deactivate() {
    if (!isActive) return;
    isActive = false;

    if (headingObserver) {
      headingObserver.disconnect();
      headingObserver = null;
    }
    if (titleObserver) {
      titleObserver.disconnect();
      titleObserver = null;
    }
    clearActive();
    if (titleLink) titleLink.classList.remove('is-visible');
    stopScrollFallback();
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
      // Always recreate observers — rootMargin depends on viewport height
      deactivate();
      checkViewport();
    }, RESIZE_DEBOUNCE_MS);
  });

  // ── Initial activation ─────────────────────────────────────────
  checkViewport();
})();
