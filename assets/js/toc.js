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

  // Collect heading IDs from TOC links
  var headingIds = [];
  tocLinks.forEach(function (link) {
    var href = link.getAttribute('href');
    if (href && href.startsWith('#')) {
      headingIds.push(href.slice(1));
    }
  });

  // ── Scroll spy ────────────────────────────────────────────────
  var headingObserver = null;
  var titleObserver = null;
  var currentActive = null;

  function setActive(id) {
    if (currentActive === id) return;
    currentActive = id;

    tocLinks.forEach(function (link) {
      var href = link.getAttribute('href');
      if (href === '#' + id) {
        link.classList.add('toc-active');
        // Auto-scroll the TOC nav so the active link stays visible
        if (floatingNav.scrollHeight > floatingNav.clientHeight) {
          link.scrollIntoView({ block: 'nearest' });
        }
      } else {
        link.classList.remove('toc-active');
      }
    });
  }

  function clearActive() {
    currentActive = null;
    tocLinks.forEach(function (link) {
      link.classList.remove('toc-active');
    });
  }

  function createHeadingObserver() {
    // 1px "tripwire" at 1/3 viewport height
    var triggerPoint = Math.round(window.innerHeight / 3);
    var rootMarginTop = -triggerPoint;
    var rootMarginBottom = -(window.innerHeight - triggerPoint - 1);

    return new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            setActive(entry.target.id);
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
        window.scrollTo({ top: 0, behavior: 'smooth' });
      }
      return;
    }

    var target = document.getElementById(href.slice(1));
    if (!target) return;
    e.preventDefault();

    if (prefersReducedMotion.matches) {
      target.scrollIntoView();
    } else {
      target.scrollIntoView({ behavior: 'smooth' });
    }
  }

  // Attach click handlers to all TOC variants
  document.querySelectorAll('.toc-floating-nav, .toc-inline, .toc-collapsible-nav').forEach(function (nav) {
    nav.addEventListener('click', handleTocClick);
  });

  // ── Activation / deactivation ─────────────────────────────────
  var isActive = false;

  function activate() {
    if (isActive) return;
    isActive = true;

    headingObserver = createHeadingObserver();
    headingIds.forEach(function (id) {
      var el = document.getElementById(id);
      if (el) headingObserver.observe(el);
    });

    titleObserver = createTitleObserver();
    if (titleObserver && articleH1) {
      titleObserver.observe(articleH1);
    }
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
