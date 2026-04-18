(function () {
  var HOVER_DEBOUNCE_MS = 150;  // avoid fetching on brief hover-throughs
  var POPUP_GAP = 6;            // px between link and popup edge
  var POPUP_EDGE_MARGIN = 8;    // px margin from viewport edge on reposition
  var PREVIEW_MAX_LENGTH = 200; // truncate preview text to roughly one paragraph

  var cache = new Map();
  var popup = null;
  var timer = null;
  var activeLink = null;

  // Paths whose pages are notes (i.e. have a .note-body to preview).  /notes/
  // covers most notes; /about/ and /contact/ are notes whose URLs are
  // overridden via EXPORT_HUGO_URL to live at the site root.
  var PREVIEWABLE_PREFIXES = ['/notes/', '/about/', '/contact/'];

  function isPreviewableHref(href) {
    if (!href) return false;
    for (var i = 0; i < PREVIEWABLE_PREFIXES.length; i++) {
      if (href.startsWith(PREVIEWABLE_PREFIXES[i])) return true;
    }
    return false;
  }

  function createPopup() {
    var el = document.createElement('div');
    el.className = 'link-preview';
    el.setAttribute('role', 'tooltip');
    document.body.appendChild(el);
    return el;
  }

  function showPreview(link) {
    var href = link.getAttribute('href');
    if (!isPreviewableHref(href)) return;

    activeLink = link;

    timer = setTimeout(function () {  // debounce hover
      if (activeLink !== link) return;

      if (cache.has(href)) {
        displayPopup(link, cache.get(href));
      } else {
        fetch(href)
          .then(function (r) {
            if (!r.ok) throw new Error(r.status);
            return r.text();
          })
          .then(function (html) {
            var doc = new DOMParser().parseFromString(html, 'text/html');
            // .note-body is a class set in layouts/notes/single.html (not in CSS)
            var body = doc.querySelector('.note-body');
            var p = body ? body.querySelector('p') : null;
            var text = p ? p.textContent.trim() : '';
            if (text.length > PREVIEW_MAX_LENGTH) text = text.substring(0, PREVIEW_MAX_LENGTH) + '\u2026';
            cache.set(href, text);
            if (activeLink === link) {
              displayPopup(link, text);
            }
          })
          .catch(function () { /* Don't cache on failure — allow retry on next hover */ });
      }
    }, HOVER_DEBOUNCE_MS);
  }

  function displayPopup(link, text) {
    if (!text) return;
    if (!popup) popup = createPopup();
    popup.textContent = text;

    var rect = link.getBoundingClientRect();
    popup.style.left = rect.left + window.scrollX + 'px';
    popup.style.top = (rect.bottom + window.scrollY + POPUP_GAP) + 'px';
    popup.classList.add('visible');

    // Reposition if popup overflows viewport (horizontal)
    var popupRect = popup.getBoundingClientRect();
    if (popupRect.right > window.innerWidth) {
      popup.style.left = Math.max(0, window.innerWidth - popupRect.width - POPUP_EDGE_MARGIN + window.scrollX) + 'px';
    }
    // Reposition if popup overflows viewport (vertical) — show above the link
    popupRect = popup.getBoundingClientRect();
    if (popupRect.bottom > window.innerHeight) {
      popup.style.top = (rect.top + window.scrollY - popupRect.height - POPUP_GAP) + 'px';
    }
  }

  function hidePreview() {
    activeLink = null;
    clearTimeout(timer);
    if (popup) popup.classList.remove('visible');
  }

  // Capture phase required: pointerenter/pointerleave don't bubble, so delegation only works via capture
  document.addEventListener('pointerenter', function (e) {
    var el = e.target.nodeType === Node.ELEMENT_NODE ? e.target : e.target.parentElement;
    if (!el) return;
    var link = el.closest('a[href^="/notes/"], a[href^="/about/"], a[href^="/contact/"]');
    if (link) showPreview(link);
  }, true);

  document.addEventListener('pointerleave', function (e) {
    var el = e.target.nodeType === Node.ELEMENT_NODE ? e.target : e.target.parentElement;
    if (!el) return;
    var link = el.closest('a[href^="/notes/"], a[href^="/about/"], a[href^="/contact/"]');
    if (link && (!e.relatedTarget || !link.contains(e.relatedTarget))) {
      hidePreview();
    }
  }, true);

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hidePreview();
  });
})();
