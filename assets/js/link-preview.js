(function () {
  var HOVER_DEBOUNCE_MS = 150;  // avoid fetching on brief hover-throughs
  var POPUP_GAP = 6;            // px between link and popup edge
  var POPUP_EDGE_MARGIN = 8;    // px margin from viewport edge on reposition
  var PREVIEW_MAX_LENGTH = 200; // truncate preview text to roughly one paragraph

  var cache = new Map();
  var popup = null;
  var timer = null;
  var activeLink = null;

  function createPopup() {
    var el = document.createElement('div');
    el.className = 'link-preview';
    el.setAttribute('role', 'tooltip');
    document.body.appendChild(el);
    return el;
  }

  function showPreview(link) {
    var href = link.getAttribute('href');
    // Link previews only apply to notes (other sections don't have prose content to preview)
    if (!href || !href.startsWith('/notes/')) return;

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
            var body = doc.querySelector('.note-body');
            var p = body ? body.querySelector('p') : null;
            var text = p ? p.textContent.trim() : '';
            if (text.length > PREVIEW_MAX_LENGTH) text = text.substring(0, PREVIEW_MAX_LENGTH) + '\u2026';
            cache.set(href, text);
            if (activeLink === link) {
              displayPopup(link, text);
            }
          })
          .catch(function () { cache.set(href, ''); }); // Cache empty string on failure to prevent repeated fetch attempts
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
    var link = el.closest('a[href^="/notes/"]');
    if (link) showPreview(link);
  }, true);

  document.addEventListener('pointerleave', function (e) {
    var el = e.target.nodeType === Node.ELEMENT_NODE ? e.target : e.target.parentElement;
    if (!el) return;
    var link = el.closest('a[href^="/notes/"]');
    if (link && (!e.relatedTarget || !link.contains(e.relatedTarget))) {
      hidePreview();
    }
  }, true);

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hidePreview();
  });
})();
